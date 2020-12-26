library(tidyverse)
library(greta)
library(here)
library(MCMCpack)
source(here("R", "utils.R"))

result_dir = here("data", "prolific", "results", "toy-blocks-pilot-2")
fn <- paste(result_dir, "toy-blocks-pilot-2_tables_smooth.csv", sep=fs)
table_data <- read_csv(fn) %>% arrange(id)

get_optimal_alphas <- function(st_id) {
  y <- table_data %>% 
    filter(id == st_id)
  y <- y[, 3:6] %>% 
    as.matrix()
  y <- prop.table(y + epsilon, 1)
  
  alpha <- uniform(0,20, 4)
  
  distribution(y) <- dirichlet(t(alpha), n_realisations = nrow(y))
  
  m <- model(alpha)
  
  fit_opt <- opt(m)
  
  tibble(
    id = st_id,
    alpha_1 = fit_opt$par$alpha[1],
    alpha_2 = fit_opt$par$alpha[2],
    alpha_3 = fit_opt$par$alpha[3],
    alpha_4 = fit_opt$par$alpha[4],
    )
}

stimulus_id_list <- table_data %>% pull(id) %>% unique()

# results <- map_df(
#   stimulus_id_list, 
#   function(s) {
#     print(s)
#     get_optimal_alphas(st_id = s)  
#   }
# )
# write_csv(results, paste(result_dir, "results-dirichlet-fits.csv", sep=fs))
results=read_csv(paste(result_dir, "results-dirichlet-fits.csv", sep=fs))

# generate tables ---------------------------------------------------------
# sample tables from fitted dirichlet priors
sample_tables <- function(params, n){
  set.seed(seed_fitted_tables)
  return(
    pmap_dfr(params, function(...){
     row = tibble(...) 
     rdirichlet(n, row[1, 2:5] %>% as.numeric()) %>% as_tibble() %>% add_column(stimulus=row$id)
    })
  )
}
n=2500
tables.generated = sample_tables(results, n) %>%
  rename(`AC`=V1, `A-C`=V2, `-AC`=V3, `-A-C`=V4) %>%
  mutate(`AC.round`=as.integer(round(AC, 2) * 100),
         `A-C.round`=as.integer(round(`A-C`, 2) * 100),
         `-AC.round`=as.integer(round(`-AC`, 2) * 100),
         `-A-C.round`=as.integer(round(`-A-C`, 2) * 100)) %>%
  distinct_at(vars(c(ends_with(".round"))), .keep_all = TRUE) %>%
  rowid_to_column("table_id")

# tables.generated %>% group_by(stimulus) %>% 
#   save_data(paste(result_dir, "tables-fitted-dirichlet.rds", sep=fs))
# use model tables insteads
# tables.generated = readRDS(here("model", "data", "tables-model.rds")) %>%
#   mutate(AC=round(AC, 2), `A-C`=round(`A-C`,2),
#          `-AC`=round(`-AC`, 2), `-A-C`=round(`-A-C`, 2)) 

# which of the empirical tables were sampled from fitted priors?
# check for all exact generated tables, not augmented ones
tables.empiric.pids = readRDS(paste(result_dir, "tables-empiric-pids.rds", sep=fs)) %>%
  rename(AC=bg, `A-C`=b, `-AC`=g, `-A-C`=none,
         AC.round=`bg.round`, `A-C.round`=`b.round`,
         `-AC.round`=`g.round`, `-A-C.round`=`none.round`)
tables = left_join(tables.generated, tables.empiric.pids,
                   by=c("AC.round", "A-C.round", "-AC.round", "-A-C.round"))
empirical_in_tables = tables %>% filter(!is.na(empirical_id)) %>%
  pull(empirical_id) %>% unique() 
length(empirical_in_tables) / nrow(tables.empiric.pids)

# check how many tables were sampled from fitted priors given augmented tables
tables.emp.augmented = readRDS(paste(result_dir, "tables-empiric-augmented.rds",
                                     sep=fs)) %>%
  rename(AC=bg, `A-C`=b, `-AC`=g, `-A-C`=none,
         AC.round=`bg.round`, `A-C.round`=`b.round`,
         `-AC.round`=`g.round`, `-A-C.round`=`none.round`)

tbls.emp.augmented = left_join(
  tables.emp.augmented,
  tables.empiric.pids %>% dplyr::select(empirical_id, p_id),
  by=c("empirical_id")
)

tbls = left_join(tables.generated, tbls.emp.augmented,
                   by=c("AC.round", "A-C.round", "-AC.round", "-A-C.round")) %>% 
  mutate(empirical = !is.na(empirical_id)) %>%
  arrange(augmented)
empirical_in_tables = tbls %>% filter(empirical) %>%
  pull(empirical_id) %>% unique()
length(empirical_in_tables) / nrow(tables.empiric.pids)

# save mapping of table-empirical ids
# 1 empirical id --MAPS TO--> several table_ids

# generated + empirical
tbls.gen.emp = tbls %>% filter(empirical) %>% group_by(empirical_id) %>% 
  mutate(n=n(), s=sum(augmented), only_augmented=s==n) %>%
  mutate(orig_table=case_when(augmented ~ FALSE, 
                              TRUE ~ TRUE))
# generated + not empirical
tbls.gen.not_emp = tbls %>% filter(!empirical) %>%
  dplyr::select(-ends_with(".y")) %>% 
  rename(`AC`=`AC.x`, `A-C`=`A-C.x`, `-AC`=`-AC.x`, `-A-C`=`-A-C.x`) %>%
  add_column(only_augmented=FALSE, orig_table=FALSE)
  
# 1. use original empirical tables whenever possible, i.e. when match within 
# generated and empirical tables where augmented is false
# --> take table where augemented is false

# generated + empirical + origianl
tbls.gen.emp.orig = tbls.gen.emp %>% filter(orig_table) %>% 
  dplyr::select(-ends_with(".x")) %>% 
  rename(`AC.x`=`AC.y`, `A-C.x`=`A-C.y`, `-AC.x`=`-AC.y`, `-A-C.x`=`-A-C.y`)

# 2. but some generated tables match only with augmented empirical tables
# generated + empirical + not original
tbls.gen.emp.not_orig = tbls.gen.emp %>% filter(!orig_table) %>%
  dplyr::select(-ends_with(".y"))

tbls.gen.emp = bind_rows(tbls.gen.emp.orig, tbls.gen.emp.not_orig) %>%
  rename(`AC`=`AC.x`, `A-C`=`A-C.x`, `-AC`=`-AC.x`, `-A-C`=`-A-C.x`) %>%
  dplyr::select(-n, -s)

# 3. merge again generated tables
tbls.gen.all = bind_rows(tbls.gen.emp, tbls.gen.not_emp)

# 4. for generated tables from (2.) that match only with augmented empirical tables
# check what the original table was and also add this one
tbl_id.max = tbls.gen.all$table_id %>% unique() %>% length()
ids.aug = tbls.gen.emp %>% filter(only_augmented) %>% pull(empirical_id) %>% unique()
tbls.aug = tables.empiric.pids %>% filter(empirical_id %in% ids.aug) %>%
  dplyr::select(-p_id) %>% add_column(orig_table=TRUE)
tbls.aug = tbls.aug %>%
  add_column(table_id = seq(tbl_id.max+1, tbl_id.max+nrow(tbls.aug)),
             row_id=NA_integer_, augmented = NA, stimulus="")
map.ids = tables.empiric.pids %>% dplyr::select(empirical_id, p_id)
tbls.aug = left_join(tbls.aug, map.ids, by=c("empirical_id")) 

# add these tables to generated tables
tables.generated.all = bind_rows(tbls.gen.all, tbls.aug)

save_data(tables.generated.all,
          paste(result_dir, "mapping-tables-fitted-dirichlet-ids.rds", sep=fs))

# save tables for input to webppl model -------------------------------
tables.model = tables.generated.all %>%
  dplyr::select(-row_id, -ends_with(".round"), -augmented, -only_augmented)

indep_sigma <- configure(c("model_tables"))$indep_sigma
tables.toWPPL = tables.model %>% 
  group_by(table_id) %>% 
  mutate(vs=list(c("AC", "A-C", "-AC", "-A-C")),
         ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`))) %>% 
  likelihood(indep_sigma)

tables.toWPPL %>% 
  save_data(paste(result_dir, "tables-fitted-dirichlet-empirical.rds", sep=fs))

# compute log likelihood for each sample table and each stimulus
ll_dirichlet = function(tables, par){
  tables.mat = tables %>% as.matrix()
    # dplyr::select(-table_id, -augmented_id) %>% 
  tables.smooth = prop.table(tables.mat + epsilon, 1) 
  vec = rep(par %>% dplyr::select(-id) %>% as.matrix(), nrow(tables.smooth))
  par.mat = matrix(vec, nrow(tables.smooth), 4, byrow = TRUE)
  densities = MCMCpack::ddirichlet(tables.smooth, par.mat)
  name_ll = paste("ll", par$id, sep="__")
  df = tables %>% add_column(ll=log(densities)) %>% rename(!!name_ll:=ll)
  return(df)
}

# df.ll=map_dfc(stimulus_id_list, function(id){
#   print(id)
#   par = results %>% filter(id == (!! id))
#   ll = tables.model %>% dplyr::select(-table_id, -stimulus, -empirical) %>% 
#     ll_dirichlet(par)
#   ll[,ncol(ll)]
# })
# # all tables with all log likelihoods
# tables.ll = bind_cols(tables.model, df.ll)
# 
# # take src stimulus with highest log likelihood among independent/dependent stimuli
# # (for ll_ind and ll_dep)
# df=tables.ll %>% 
#   pivot_longer(cols = starts_with("ll__"), names_to="ll.key", values_to="ll") %>%
#   group_by(table_id)
# df.ind = df %>% filter(str_detect(ll.key, "independent")) %>% 
#   mutate(ll.key=ll.key[ll==max(ll)], ll=max(ll)) %>% distinct() %>%
#   rename(logL_ind=ll, ll.key_ind=ll.key)
# df.dep = df %>% filter(str_detect(ll.key, "if")) %>% 
#   mutate(ll.key=ll.key[ll==max(ll)], ll=max(ll)) %>% distinct() %>%
#   rename(logL_if_ac=ll, ll.key_dep=ll.key)
# 
# tables.toWPPL = left_join(df.ind, df.dep) %>% group_by(table_id) %>% 
#   mutate(vs=list(c("AC", "A-C", "-AC", "-A-C")),
#          ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`))) %>% 
#   rename(stimulus_id=table_id) # for model, id needs to have name stimulus_id
# 
# save_to = paste(result_dir, "model-tables-stimuli.rds", sep=fs)
# saveRDS(tables.toWPPL, save_to)
# # tables.toWPPL = readRDS(save_to)

# print(paste('saved generated tables to:', save_to))

# Goodness fits -----------------------------------------------------------
ll_empirical_data = function(params){
  # only exact empirical data ll 
  tables = readRDS(paste(result_dir, "mapping-tableID-prolificID.rds",
                         sep=fs)) %>%
    unnest(c(ids)) %>% separate(col=ids, into=c("prolific_id", "trial", "prior"), sep="_") %>%
    unite("stimulus_id", c(trial, prior)) %>% group_by(stimulus_id) %>%
    rename(augmented_id=empirical_id)
  
  ll.empirical=map_dfr(stimulus_id_list, function(id){
    par = params %>% filter(id == (!! id))
    ll = tables %>% filter(stimulus_id==(!!id)) %>% ungroup() %>% 
      dplyr::select(-prolific_id, -stimulus_id, -table_id, -augmented_id) %>% 
      ll_dirichlet(par)
    tibble(stimulus_id=id, ll_sample=sum(ll[,ncol(ll)]))
  });
  return(ll.empirical)
}

goodness_fits_dirichlet = function(params, n, N=30){
  # likelihood of empirical data
  ll.empirical = ll_empirical_data(params)
  # Sample n times N=30 values for each of the distinctive probabilities mapping
  # to a probability table
  p_values = pmap_dfr(params, function(...){
    par = tibble(...)
    print(par$id)
    # empirical likelihood for all participants data for current cn(+id)
    # given fitted params
    ll.obs = ll.empirical %>% filter(stimulus_id == par$id) %>% pull(ll_sample)
    # sample n times N (#participants,30) tables and compute log likelihood
    probs = sample_tables(par, N*n)
    ll.simulated = probs %>% dplyr::select(-id) %>% ll_dirichlet(par)
    colnames(ll.simulated)[ncol(ll.simulated)] = "ll";
    
    ll.simulated = ll.simulated %>% add_column(idx=rep(seq(1, n), N)) %>%
      group_by(idx) %>% summarize(s=sum(ll), .groups = "drop_last") %>%
      dplyr::select(-idx);
  
    p.val = (ll.simulated < ll.obs) %>% sum()/n
    ll.simulated %>%
      mutate(stimulus_id=par$id, p.val=p.val, n=n)
  }) ;
  return(p_values)
}
params=read_csv(paste(result_dir, "results-dirichlet-fits.csv",
                      sep=fs)) %>% filter(id != "ind2")
res.goodness = goodness_fits_dirichlet(params, 10**4) %>% arrange(desc(p.val));
res.goodness %>% select(-s) %>% distinct()
saveRDS(res.goodness, paste(result_dir, "simulated-p-values.rds", sep=fs))

p_vals = res.goodness %>% dplyr::select(stimulus_id, p.val) %>% distinct()
p_vals
ll.obs = ll_empirical_data(params) %>% filter(stimulus_id != "ind2")

res.goodness %>% ggplot(aes(x=s)) +
  geom_density() +
  geom_point(data=ll.obs, aes(x=ll_sample, y=0), color='red', size=2)+
  facet_wrap(~stimulus_id) +
  theme_classic() +
  labs(x="log-likelihood simulated data")

