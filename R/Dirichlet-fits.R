library(tidyverse)
library(greta)
library(here)
library(MCMCpack)
source(here("R", "joint_experiment", "analysis-utils.R"))

fn <- here("data", "prolific", "results", "toy-blocks-pilot-2",
           "toy-blocks-pilot-2_tables_all.csv")
table_data <- read_csv(fn) %>% arrange(id)

epsilon <- 0.000001

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

results <- map_df(
  stimulus_id_list, 
  function(s) {
    print(s)
    get_optimal_alphas(st_id = s)  
  }
)
write_csv(results, here("data", "prolific", "results", "toy-blocks-pilot-2",
                                                "results-dirichlet-fits.csv"))
# results=read_csv(here("data", "prolific", "results", "toy-blocks-pilot-2", "results-dirichlet-fits.csv"))

# generate tables ---------------------------------------------------------
# sample tables from fitted dirichlet priors
sample_tables <- function(params, n){
  return(
    pmap_dfr(params, function(...){
     row = tibble(...) 
     rdirichlet(n, row[1, 2:5] %>% as.numeric()) %>% as_tibble() %>% add_column(stimulus=row$id)
    })
  )
}
n=5000
tables.generated = sample_tables(results, n) %>%
  rename(`AC`=V1, `A-C`=V2, `-AC`=V3, `-A-C`=V4) %>%
  mutate(AC=round(AC, 2), `A-C`=round(`A-C`,2),
         `-AC`=round(`-AC`, 2), `-A-C`=round(`-A-C`, 2)) %>%
  distinct() %>% rowid_to_column("table_id")

# model tables
# tables.generated = readRDS(here("model", "data", "tables-model.rds")) %>%
#   mutate(AC=round(AC, 2), `A-C`=round(`A-C`,2),
#          `-AC`=round(`-AC`, 2), `-A-C`=round(`-A-C`, 2)) 

# which of the empirical tables were sampled from fitted priors?
# all (potentially redundant) empiric tables  
tables.empiric = readRDS(paste(RESULT.dir, "tables-empiric-pids.rds", sep=SEP))
tables = left_join(tables.generated, tables.empiric, by=c("AC", "A-C", "-AC", "-A-C"))
empirical_in_tables = tables %>% filter(!is.na(empirical_id)) %>% pull(empirical_id) %>% unique() 
length(empirical_in_tables) / nrow(tables.empiric)

# check how many tables were sampled from fitted priors given augmented tables
tables.emp.augmented = readRDS(paste(RESULT.dir, "tables-empiric-augmented.rds", sep=SEP))
tables = left_join(tables.generated, tables.emp.augmented,
                   by=c("AC", "A-C", "-AC", "-A-C")) %>% 
  mutate(empirical = !is.na(empirical_id))
empirical_in_tables = tables %>% pull(empirical_id) %>% unique() 
length(empirical_in_tables) / nrow(tables.empiric)

# preparte tables for input to webppl model -------------------------------
tables.model = tables %>% dplyr::select(-augmented_id, -empirical_id)
tables.toWPPL = bind_cols(
  prop.table(
    tables.model %>%
      dplyr::select(-table_id, -empirical, -cn, -starts_with("logL_")) %>%
      as.matrix() + epsilon, 1
  ) %>% as_tibble(),
  tables.model %>% dplyr::select(table_id, cn, empirical, starts_with("logL_"))
)

tables.toWPPL = tables.toWPPL %>%  group_by(table_id) %>% 
  mutate(vs=list(c("AC", "A-C", "-AC", "-A-C")),
         ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`))) %>%
  add_column(stimulus="")

#save_to = paste(RESULT.dir, "model-tables-stimuli.rds", sep=SEP)
# save_to = params$tables_path
# save_data(tables.toWPPL, save_to)
# save_data(params, params$target_params)

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

STIMULI = TABLES.all$id %>% unique()
# df.ll=map_dfc(STIMULI, function(id){
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
# save_to = paste(RESULT.dir, "model-tables-stimuli.rds", sep=SEP)
# saveRDS(tables.toWPPL, save_to)
# # tables.toWPPL = readRDS(save_to)

# print(paste('saved generated tables to:', save_to))

# Goodness fits -----------------------------------------------------------
ll_empirical_data = function(params){
  # only exact empirical data ll 
  tables = readRDS(paste(RESULT.dir, "mapping-tableID-prolificID.rds", sep=SEP)) %>%
    unnest(c(ids)) %>% separate(col=ids, into=c("prolific_id", "trial", "prior"), sep="_") %>%
    unite("stimulus_id", c(trial, prior)) %>% group_by(stimulus_id) %>%
    rename(augmented_id=empirical_id)
  
  ll.empirical=map_dfr(STIMULI, function(id){
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
params=read_csv(here("data", "prolific", "results", "toy-blocks-pilot-2",
                     "results-dirichlet-fits.csv")) %>% filter(id != "ind2")
res.goodness = goodness_fits_dirichlet(params, 10**4) %>% arrange(desc(p.val));
res.goodness %>% select(-s) %>% distinct()
saveRDS(res.goodness, paste(RESULT.dir, "simulated-p-values.rds", sep=.Platform$file.sep))

p_vals = res.goodness %>% dplyr::select(stimulus_id, p.val) %>% distinct()
p_vals
ll.obs = ll_empirical_data(params) %>% filter(stimulus_id != "ind2")

res.goodness %>% ggplot(aes(x=s)) +
  geom_density() +
  geom_point(data=ll.obs, aes(x=ll_sample, y=0), color='red', size=2)+
  facet_wrap(~stimulus_id) +
  theme_classic() +
  labs(x="log-likelihood simulated data")


# fit independent ---------------------------------------------------------
ind = tables.empiric %>% mutate(diff=AC-((AC+`A-C`)*(AC+`-AC`))) %>% unnest(c(p_id)) %>%
  separate(p_id, into=c("prolific_id", "stimulus", "prior"), sep="_") %>%
  unite("stimulus_id", c(stimulus, prior), sep="-") %>%
  group_by(stimulus_id) # %>% filter(!str_detect(stimulus_id, "independent"))
ind %>% ggplot(aes(x=diff, color=stimulus_id)) +
  geom_density() +
  facet_wrap(~stimulus_id)
  
