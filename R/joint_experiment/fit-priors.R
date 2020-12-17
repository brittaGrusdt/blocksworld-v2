library(here)
source(here("R", "joint_experiment", "analysis-utils.R"))

# target_dir_params= "../MA-project/conditionals/data"
target_dir_params = here("data", "prolific", "results", exp.name)

# Functions ---------------------------------------------------------------
getTables = function(exp.name, data_filtered=FALSE){
  if(data_filtered) {
    fn = "empiric-filtered-tables-smooth.rds"
    tables = readRDS(here("data", "prolific", "results", exp.name, fn))
  } else {
    fn = "empiric-all-tables-smooth.rds"
    tables = readRDS(here("data", "prolific", "results", exp.name, fn))
  }
  return(tables)
}

cnToProbs = function(y.tables, cn){
  if(cn=="A implies C"){
    df = y.tables %>%
      mutate(p_pos=p_c_given_a, p_neg=p_c_given_na, p_marginal=p_a,
             causal_power=theta_ac) %>% select(-starts_with("theta_"))
  } else if(cn=="A implies -C") {
    df = y.tables %>%
      mutate(p_pos=1-p_c_given_a, p_neg=1-p_c_given_na, p_marginal=p_a,
             causal_power=theta_anc) %>% select(-starts_with("theta_"))
  } else if(cn=="C implies A") {
    df = y.tables %>%
      mutate(p_pos=p_a_given_c, p_neg=p_a_given_nc, p_marginal=p_c,
             causal_power=theta_ca) %>% select(-starts_with("theta_"))
  } else if(cn=="C implies -A") {
    df = y.tables %>%
      mutate(p_pos=1-p_a_given_c, p_neg=1-p_a_given_nc, p_marginal=p_c,
             causal_power=theta_cna) %>% select(-starts_with("theta_"))
  } else if(cn == "A || C"){
    df = y.tables
  }
  return(df)
}

# Fit P(C|A), P(C|-A), P(A) beta distributions ----------------------------
BetaFits = function(fn_suffix, exp.name, filtered_data=FALSE,
                    target_dir_params= "../MA-project/conditionals/data") {
  tables.all = getTables(exp.name, data_filtered=filtered_data)
  get_optimal_params = function(cn, id){
    if(cn == "A || C"){
      df.observations = tables.all %>% filter(id==(!!id)) %>%
        mutate(diff = AC-p_a*p_c)
      # independent tables: only use subset who "understood" relation
      # df.rel.good = understood_relations(data.prior.norm) %>%
      #   mutate(id=as.character(id)) %>% 
      #   filter(id == (!! id)) %>% filter(as_expected) %>% select(id, prolific_id)
      # df.observations = left_join(df.rel.good, df)
      y = list(p_a=df.observations$p_a,
               p_c=df.observations$p_c,
               diff=df.observations$diff)
      # PRIORS of the parameters of the distributions
      p_a.shape = uniform(0, 10, 2)
      p_c.shape = uniform(0, 10, 2)
      diff.shape = uniform(0, 1, 1)
      
      # LIKELIHOODS
      distribution(y$p_a) <- beta(p_a.shape[1], p_a.shape[2])
      distribution(y$p_c) <- beta(p_c.shape[1], p_c.shape[2])
      distribution(y$diff) <- normal(0, diff.shape[1], truncation=c(-1,1))
      
      m.p_a <- model(p_a.shape)
      m.p_c <- model(p_c.shape)
      m.diff <- model(diff.shape)
      fit_opt.p_a <- opt(m.p_a)
      fit_opt.p_c <- opt(m.p_c)
      fit_opt.diff <- opt(m.diff)
      
      params = tibble(p_a1 = fit_opt.p_a$par$p_a.shape[1],
                      p_a2 = fit_opt.p_a$par$p_a.shape[2],
                      p_c1 = fit_opt.p_c$par$p_c.shape[1],
                      p_c2 = fit_opt.p_c$par$p_c.shape[2],
                      p_diff_sd = fit_opt.diff$par$diff.shape[1],
                      cn=cn, id=id);
    } else {
      df.observations = tables.all %>% filter(id==(!!id)) %>% cnToProbs(cn);
      y = list(conditional_pos = df.observations %>% pull(p_pos),
               conditional_neg = df.observations %>% pull(p_neg),
               conditional_marginal = df.observations %>% pull(p_marginal)
      );
      # priors of the parameters of the beta distributions
      pos.shape = uniform(0, 10, 2)
      neg.shape = uniform(0, 10, 2)
      marg.shape = uniform(0, 10, 2)
      distribution(y$conditional_pos) <- beta(pos.shape[1], pos.shape[2])
      distribution(y$conditional_neg) <- beta(neg.shape[1], neg.shape[2])
      distribution(y$conditional_marginal) <- beta(marg.shape[1], marg.shape[2])
      
      # fits
      m.pos <- model(pos.shape)
      m.neg <- model(neg.shape)
      m.marg <- model(marg.shape)
      fit_opt.pos <- opt(m.pos)
      fit_opt.neg <- opt(m.neg)
      fit_opt.marg <- opt(m.marg)
      
      params = tibble(pos1=fit_opt.pos$par$pos.shape[1],
                      pos2=fit_opt.pos$par$pos.shape[2],
                      neg1 = fit_opt.neg$par$neg.shape[1],
                      neg2 = fit_opt.neg$par$neg.shape[2],
                      marg1 = fit_opt.marg$par$marg.shape[1],
                      marg2 = fit_opt.marg$par$marg.shape[2],
                      cn=cn, id=id);
    }
    return(params)
  }
  results = pmap_dfr(ID_CNS, function(id, cn){
                     print(paste(id, cn, sep=","))
                     get_optimal_params(cn, id)
                     });
  fn = paste("beta-fits", fn_suffix, ".csv", sep="")
  write_csv(results, paste(RESULT.dir, fn, sep=SEP))
  return(results)
}

#log likelihood for entire sample of empirical probability tables for each stimulus
save_params_best_ll = function(res.fits, fn_suffix, exp.name, filtered_data=FALSE){
  tables = getTables(exp.name, data_filtered=filtered_data) %>% rename(stimulus_id=id) %>%
    group_by(stimulus_id)
  fits = bind_rows(res.fits,
                   res.fits %>%
                     mutate(cn=as.character(cn),
                            cn=case_when(cn=="A implies C" ~ "A implies -C",
                                         cn=="C implies A" ~ "C implies -A",
                                         TRUE ~ cn),
                    cn=as.factor(cn))
                   )
  ll.all = pmap_dfr(fits, function(...){
    row = tibble(...)
    print(row$cn)
    tables %>% log_likelihood(row$cn, row) %>% select(ll, stimulus_id) %>%
      summarize(ll.sample=sum(ll), .groups="keep") %>%
      add_column(cn=paste(row$cn, row$id, sep="--"))
  }) %>%
    group_by(stimulus_id) %>% arrange(desc(ll.sample)) %>%
    mutate(stimulus_id = as.character(stimulus_id))
  
  ll.all %>% distinct_at(vars(c(stimulus_id)), .keep_all = TRUE)
  
  # dependent
  par.dep = ll.all  %>% 
    filter(!startsWith(stimulus_id, "independent") & !startsWith(cn, "A || C")) %>%
    arrange(desc(ll.sample)) %>% 
    distinct_at(vars(c(stimulus_id)), .keep_all = TRUE)
  
  # independent: use only independent cn here!
  par.ind = ll.all %>%
    filter(startsWith(stimulus_id, "independent") & startsWith(cn, "A || C")) %>%
    arrange(desc(ll.sample)) %>% 
    distinct_at(vars(c(stimulus_id)), .keep_all = TRUE)
  
  par.best = right_join(res.fits %>% unite("cn", c(cn, id), sep="--"),
             bind_rows(par.dep, par.ind), by=c("cn"))
  
  fn = paste("params-ll-best-cn", fn_suffix, ".rds", sep="")
  save_to = paste(target_dir_params, fn, sep=SEP)
  write_rds(par.best, save_to)
  print(paste('saved best params to', save_to))
  
  par.best.formatted = formatParams4WebPPL(par.best %>% select(-ll.sample) %>%
                                             rename(id=stimulus_id));
  fn = paste("params-ll-best-cn-formatted", fn_suffix, ".rds", sep="")
  save_to = paste(target_dir_params, fn, sep=SEP)
  write_rds(par.best.formatted, save_to)
  print(paste('saved best params (formatted for webppl) to', save_to))
  
  return(list(best=par.best.formatted, all=ll.all))
}

# for each causal net (A->C/C->A/A,C independent) fit the respective distributions
# e.g. for cn=A->C: fit distributions for P(C|A), P(C|¬A) and P(A).
# for cn: A->-C and C->-A, same parameters as for A->C but reversed as the complementary
# probabilities are considered (e.g. P(¬C|A) instead of P(C|A)).
# Compute log-likelihood for all possible cns. dont switch params, as for
# A->C / A->¬C respectively the "positive" probability shall come from the same 
# best distribution, only that once this is P(C|¬A) and once P(C|A). If we switch
# params we will get the exact same likelihood for both cns which is nonsense.

# res.fits = BetaFits("-all-cns", exp.name, target_dir_params = target_dir_params)
# params.ll_all = save_params_best_ll(res.fits, "", exp.name)

# res.fits.filtered = BetaFits("-all-cns-filtered-tables", exp.name, filtered_data = TRUE)
# params.ll_filtered = save_params_best_ll(res.fits.filtered, "-filtered-data",
                                           # exp.name, filtered_data = TRUE)

  


