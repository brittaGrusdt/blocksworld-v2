library(here)
source(here("R", "joint_experiment", "analysis-utils.R"))
# source(here("R", "joint_experiment", "fit-priors.R"))
# CREATE TABLES FOR MODEL PREDICTIONS -------------------------------------
sample_probs_independent = function(n, par, sd=0.01){
  pa = rbeta(n, par$p_a1, par$p_a2)
  pc = rbeta(n, par$p_c1, par$p_c2)
  upper = pmin(pa, pc)
  lower = pa + pc 
  lower[lower<=1] = 0
  lower[lower>1] = lower[lower>1]-1
  ac = rtruncnorm(n, a=lower, b=upper, mean=pa*pc, sd=sd)
  return(tibble(p_a=pa, p_c=pc, AC=ac))
}
create_independent_tables <- function(params, n=1000, sd=0.01){
  par.ind = params %>% filter(cn == "A || C")
  tables.all = pmap_dfr(par.ind, function(...) {
    row = tibble(...)
    print(paste(row$cn, row$id))
    probs = sample_probs_independent(n, row, sd);
    tables <- probs %>%
      mutate(`A-C`=p_a-AC, `-AC`=p_c-AC,
              s=AC+`A-C`+`-AC`, `-A-C`= case_when(s >= 1 ~ 0,
                                                 TRUE ~ 1-s));
    tables.mat = tables %>% dplyr::select(-s, -p_a, -p_c)
    df.tables = prop.table(tables.mat %>% as.matrix() + epsilon, 1) %>%
      as_tibble() %>% rowid_to_column("id")
    tables_long <- df.tables %>% group_by(id) %>%
      pivot_longer(cols=c(`AC`, `A-C`, `-AC`, `-A-C`), names_to="cell",
                   values_to="val")
    tables_long %>%
      summarise(ps = list(val), vs=list(cell), .groups = 'drop') %>%
      add_column(cn="A || C", stimulus=row$id) %>%
      dplyr::select(-id) %>% rename(stimulus_id=stimulus)
  });
  return(tables.all)
}

sample_probs_dependent = function(n, par){
  p_neg = rbeta(n, par$neg1, par$neg2)
  p_marg = rbeta(n, par$marg1, par$marg2)
  p_pos = rbeta(n, par$pos1, par$pos2)
  return(tibble(p_neg=p_neg, p_pos=p_pos, p_marg=p_marg))
}

create_dependent_tables <- function(params, n, generate_by="p"){
  cn="A implies C"
  par.dep = params %>% filter(cn == (!! cn)) 
  tables.all = pmap_dfr(par.dep, function(...){
    row = tibble(...)
    print(paste(row$cn, row$id))
    probs = sample_probs_dependent(n, row);
    tables = probs %>%
      mutate(AC = p_pos * p_marg, `A-C` = (1-p_pos) * p_marg,
             `-AC` = p_neg * (1-p_marg), `-A-C` = (1-p_neg) * (1-p_marg));
    
    tables.mat = tables %>% dplyr::select(-p_pos, -p_neg, -p_marg)
    df.tables = prop.table(tables.mat %>% as.matrix() + epsilon, 1) %>%
      as_tibble() %>% rowid_to_column("id")
    tables_long <- df.tables %>%
      pivot_longer(cols=c(`AC`, `A-C`, `-AC`, `-A-C`),
                   names_to="cell", values_to="val")
    tables_long %>% group_by(id) %>%
      summarise(ps = list(val), vs=list(cell), .groups = 'drop') %>%
      add_column(cn=(!! row$cn), stimulus = (!! row$id)) %>%
      dplyr::select(-id) %>% rename(stimulus_id=stimulus) 
  });
  return(tables.all)
}

# Create Tables -----------------------------------------------------------
create_tables = function(target_dir, n=2500){
  params = read_csv(
    paste(RESULT.dir, "beta-fits.csv", sep=SEP)
  ) %>% group_by(id, cn)
  
  tables_ind <- create_independent_tables(params, n*4)
  tables_dep <- create_dependent_tables(params, n, generate_by = "p")
  df.tables <- bind_rows(tables_ind, tables_dep) %>%
    rowid_to_column("id")
  tables <- df.tables %>% unnest(c(vs, ps)) %>%
    group_by(id) %>% pivot_wider(names_from="vs", values_from="ps") %>%
    mutate(p_a=AC+`A-C`, p_c=AC+`-AC`, p_c_given_a=AC/p_a, p_c_given_na=`A-C`/(1-p_a))
  
  stimuli = tables$stimulus_id %>% unique()
  tables.ll=map_dfr(stimuli, function(id){
    print(id)
    cn = ifelse(str_detect(id, "independent"), "A || C", "A implies C")
    par = params %>% filter(id == (!! id))
    ll = tables %>% log_likelihood(cn, par)
    ll %>% mutate(ll.key= (!!id))
  })
  tables.ll = tables.ll %>% dplyr::select(-starts_with("p_"), -lb, -ub) %>%
    filter(!is.na(AC) && !is.na(`A-C`) && !is.na(`-AC`) && !is.na(`-A-C`)) %>%
    group_by(id, ll.key) %>% 
    mutate(vs=list(c("AC", "A-C", "-AC", "-A-C")),
           ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`))) %>%
    dplyr::select(-`AC`, -`A-C`, -`-AC`, -`-A-C`)
  return(tables.ll)
}

formatGeneratedTables4Webppl = function(generated_tables) {
  # 1.Independent
  tables.ind = generated_tables %>% filter(cn=="A || C") %>% rowid_to_column("bn_id") %>%
    unnest(c(vs,ps)) %>% group_by(bn_id) %>%
    mutate(ps=round(ps, 2)) %>% 
    pivot_wider(names_from="vs", values_from="ps") %>%
    ungroup() %>% dplyr::select(-bn_id)
  
  tables.ind.empirical = TABLES.ind %>%
    dplyr::select(id, prolific_id, AC, `A-C`, `-AC`, `-A-C`) %>%
    mutate(AC=round(AC, 2),
           `A-C`=round(`A-C`, 2),
           `-AC`=round(`-AC`, 2),
           `-A-C`=round(`-A-C`, 2))
  # 2. Dependent
  tables.dep = generated_tables %>% filter(cn!="A || C") %>% rowid_to_column("bn_id") %>%
    unnest(c(vs,ps)) %>% group_by(bn_id) %>%
    mutate(ps=round(ps, 2)) %>% 
    pivot_wider(names_from="vs", values_from="ps") %>%
    ungroup() %>% dplyr::select(-bn_id)
  
  tables.dep.empirical = TABLES.dep %>%
    dplyr::select(id, prolific_id, AC, `A-C`, `-AC`, `-A-C`) %>%
    mutate(AC=round(AC, 2),
           `A-C`=round(`A-C`, 2),
           `-AC`=round(`-AC`, 2),
           `-A-C`=round(`-A-C`, 2))
  
  tables.empirical = bind_rows(tables.ind.empirical, tables.dep.empirical) %>%
    rename(stimulus_id=id) %>%  unite("trial_id", c(stimulus_id, prolific_id), sep="--")
  
  tables.model = bind_rows(tables.ind, tables.dep) %>%
    distinct_at(vars(c(AC, `A-C`, `-AC`, `-A-C`, stimulus_id, ll.key)), .keep_all = TRUE) %>% 
    rename(table_id=id) %>% unite("bn_id", c(stimulus_id, table_id), sep="--") %>%
    group_by(bn_id)
  df = tables.model %>% dplyr::select(-ll, -ll.key,-cn) %>% distinct() %>% 
    left_join(tables.empirical, by=c("AC", "A-C", "-AC", "-A-C"))
  model = left_join(tables.model, df) %>%
    mutate(empirical=ifelse(is.na(trial_id), FALSE, TRUE))
  
  df.eq = model %>% filter(!is.na(trial_id)) %>% 
    separate(bn_id, into=c("stimulus_src", "table_id"), sep="--") %>% 
    separate(trial_id, into=c("stimulus_id", "prolific_id"), sep="--")
  tables.eq.by_stimulus = df.eq %>% filter(stimulus_src==stimulus_id) %>%
    distinct_at(vars(c(AC, `A-C`, `-AC`, `-A-C`, stimulus_src)))
  tables.eq.across_stimuli = df.eq %>%
    distinct_at(vars(c(AC, `A-C`, `-AC`, `-A-C`)), .keep_all = TRUE)
  print(paste('nb. of generated empirical tables (dependent on stimulus):', nrow(tables.eq.by_stimulus)))
  print(paste('nb. of generated empirical tables (across stimuli):', nrow(tables.eq.across_stimuli)))

  # take src stimulus with highest log likelihood among independent/dependent stimuli
  # (for ll_ind and ll_dep)
  df = model %>% mutate(trial_id=list(trial_id)) %>% distinct()
  df1 = df %>% filter(str_detect(ll.key, "independent")) %>% 
    mutate(ll.key=ll.key[ll==max(ll)], ll=max(ll)) %>%
    distinct()
  df2 = df %>% filter(!str_detect(ll.key, "independent")) %>% 
    mutate(ll.key=ll.key[ll==max(ll)], ll=max(ll)) %>%
    distinct()
  df.model = bind_rows(df1, df2) %>%
    mutate(ll.key = case_when(startsWith(ll.key, "independent") ~ "logL_ind",
                              TRUE ~ "logL_if_ac")) %>%
    group_by(bn_id, ll.key) %>% arrange(desc(ll)) %>% 
    distinct()
  
  tables.toWPPL = df.model %>% group_by(bn_id) %>% 
    pivot_wider(names_from=ll.key, values_from=ll) %>% 
    mutate(vs=list(c("AC", "A-C", "-AC", "-A-C")),
           ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`))) %>%
    dplyr::select(-AC, -`A-C`, -`-AC`, -`-A-C`) %>%
    rename(stimulus_id=bn_id)
  save_to = paste(target_dir, "model-tables-stimuli.rds", sep=SEP)
  saveRDS(tables.toWPPL, save_to);
  print(paste('saved generated tables to:', save_to))
  
  mapping = tables.toWPPL %>% dplyr::select(stimulus_id, trial_id, empirical) %>%
    unnest(c(trial_id)) %>% filter(!is.na(trial_id)) %>% distinct()
  save_to = paste(target_dir, "mapping-tableID-prolificID.rds", sep=SEP)
  saveRDS(mapping, save_to);
  print(paste('saved generated tables to:', save_to))
  return(tables.toWPPL)
}

# Plot Tables -------------------------------------------------------------
plot_tables <- function(tables.model, tables.empirical){
  # data must be in long format with columns *cell* and *val*
  df.model = tables.model %>% add_column(generated_by="model") %>%
    rowid_to_column() %>% pivot_longer(cols=c(AC, `A-C`, `-AC`, `-A-C`),
                                              names_to="cell", values_to="val")
  df.empirical = tables.empirical %>% add_column(generated_by="human") %>%
    rowid_to_column() %>% pivot_longer(cols=c(AC, `A-C`, `-AC`, `-A-C`),
                                              names_to="cell", values_to="val")
  p <- bind_rows(df.model, df.empirical) %>% 
    ggplot(aes(x=generated_by, y=val, color=id)) +
    geom_jitter(alpha=0.5) +
    facet_wrap(~cell, nrow = 2, ncol=2) +
    theme_classic(base_size = 20) +
    theme(legend.position = "bottom")
  return(p)
}

# target_dir = "../MA-project/conditionals/data";
target_dir = here("data", "prolific", "results", exp.name)
tables.nested = create_tables(target_dir, 10)
# tables = formatGeneratedTables4Webppl(tables.nested)


# stimulus = "independent_hl"
# plot_tables(tables.model %>% filter(id==stimulus),
#             tables.empirical %>% filter(id==stimulus))

tables = TABLES.all %>%
  dplyr::select(AC, `A-C`, `-AC`, `-A-C`, p_c_given_a, p_c_given_na,
                prolific_id, id) %>%
  mutate(diff=abs(p_c_given_a - p_c_given_na),
         id=as.character(id),
         cn=case_when(startsWith(id, "independent") ~ "independent",
                      TRUE ~ "dependent"),
         id=as.factor(id)) 

p = tables %>% ggplot(aes(x=diff, color=cn)) +
  geom_density() +
  theme_classic(base_size = 20) +
  labs(x="|P(C|A) - P(C|¬A)|") +
  theme(legend.position="top")

ggsave(paste(PLOT.dir, "diff-tables.png", sep=SEP), p, width=10, height=12)



