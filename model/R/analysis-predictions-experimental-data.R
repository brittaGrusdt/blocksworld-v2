library(tidyverse)
library(here)
source("R/helper-functions.R")

merge_empirical_model_tables = function() {
  exp.name = "experiment-wor(l)ds-of-toy-blocks"
  data.dir = paste("/home/britta/UNI/Osnabrueck/blocksworld-v2/data/prolific/results",
                   exp.name, "", sep=.Platform$file.sep)
  tables.empirical = read_csv(paste(data.dir, exp.name, "_tables_all.csv", sep=""),
                              col_names = TRUE) %>%
    unite("stimulus_id", c(id, prolific_id), sep="--")
  
  params <-readRDS(here("data", "test-default", "params-none-PL.rds"))
  # params <-readRDS(here("data", "default-model", "params-speaker.rds"))
  tables.model = readRDS(params$tables_path) %>% ungroup() %>% select(-id)
  
  # save empirical tables in input format -----------------------------------
  tables.empirical.enriched = tables.empirical %>%
    rename(AC=bg, `A-C`=b, `-AC`=g, `-A-C`=none) %>%
    group_by(stimulus_id) %>% 
    likelihood(params$indep_sigma) %>%
    mutate(logL_if_ac=case_when(is.na(logL_if_ac) ~ -Inf, TRUE ~ logL_if_ac),
           logL_if_anc=case_when(is.na(logL_if_anc) ~ -Inf, TRUE ~ logL_if_anc), 
           logL_if_ca=case_when(is.na(logL_if_ca) ~ -Inf, TRUE ~ logL_if_ca),
           logL_if_cna=case_when(is.na(logL_if_cna) ~ -Inf, TRUE ~ logL_if_cna)
           )
  tables.empirical.long <- tables.empirical.enriched %>%
    pivot_longer(cols=c(`AC`, `A-C`, `-AC`, `-A-C`),
                 names_to="vs", values_to="ps")
  tables.empirical.to_wppl <- tables.empirical.long %>%
    mutate(vs=list(c("AC", "A-C", "-AC", "-A-C"))) %>% 
    group_by(stimulus_id, vs, logL_ind, logL_if_ac, logL_if_anc, logL_if_ca, logL_if_cna) %>%
    summarise(ps = list(ps), .groups = 'drop') %>%
    add_column(empirical=TRUE)
  
  save_data(tables.empirical.to_wppl, here("data", "tables-empirical.rds"))
  
  
  # merge empirical tables with our prior tables ----------------------------
  tables.joint = bind_rows(tables.empirical.to_wppl %>% add_column(cn=""), 
                           tables.model %>% select(-seed) %>% add_column(empirical=FALSE)
                           ) %>%
    rowid_to_column("id") %>% group_by(id)
  
  save_data(tables.joint, here("data", "tables-empirical-model.rds"))
}






