library(rwebppl)
library(tidyverse)
library(here)
source(here("model", "R", "helpers-webppl.R"))
source(here("model", "R", "helper-functions.R"))
source(here("model", "R", "helpers-values-of-interest.R"))
source(here("R", "utils-exp2.R"))

# params <- configure(c("prior", "tables_theoretic"))
# params <- configure(c("speaker", "tables_theoretic"))
# params <- configure(c("pl", "tables_theoretic"))


# params <- configure(c("prior", "tables_fitted_dirichlet"))
# params <- configure(c("speaker", "tables_fitted_dirichlet"))
params <- configure(c("pl", "tables_fitted_dirichlet"))


# Setup -------------------------------------------------------------------
dir.create(params$target_dir, recursive = TRUE)
params$target <- file.path(params$target_dir, params$target_fn, fsep=.Platform$file.sep)
params$target_params <- file.path(params$target_dir, params$target_params, fsep=.Platform$file.sep)

## Generate/Retrieve tables
tables <- readRDS(params$tables_empiric)
print(paste("tables read from:", params$tables_empiric))
params$tables = tables %>% ungroup %>%
  dplyr::select(table_id, ps, vs, stimulus, starts_with("logL_"))

params$params_ll = read_csv(paste(params$dir_empiric, params$params_ll, sep=.Platform$file.sep))
if(!("bn_ids" %in% names(params))){
  params$bn_ids = tables %>% filter(empirical) %>% pull(table_id)
}

## Generate/Retrieve utterances
generate_utts <- function(params){
  utterances <- run_webppl(here("model", "model", "default-model", "utterances.wppl"), params)
  utts <- utterances %>% map(function(x){x %>% pull(value)}) %>% unlist()
  utts %>% save_data(params$utts_path)
  return(utts)
}
if(params$generate_utterances || !file.exists(params$utts_path)){
  utterances <- generate_utts(params)
} else {
  utterances <- readRDS(params$utts_path)
  print(paste("utterances read from:", params$utts_path))
}
params$utterances <- utterances

# Run Model ---------------------------------------------------------------
posterior <- run_webppl(params$model_path, params)

# restructure data and save
if(params$level_max == "speaker") {
  speaker <- posterior %>% structure_speaker_data(params) %>%
    group_by(stimulus)
  speaker_avg <- speaker %>% average_speaker(params) %>% arrange(avg)
  speaker_avg
  res.behav_model = join_model_behavioral_data(speaker, params);

} else if(params$level_max %in% c("priorN")){
    data <- structure_bns(posterior, params)
} else if(params$level_max == "log_likelihood"){
  data <- tibble(id=posterior$id$value, cn=posterior$cn$value,
                 logL=posterior$logL$value)
} else {
  data <- posterior %>% structure_listener_data(params)
  # trust <- data %>% listener_beliefs("PL", params)
  data_voi <- voi_default(data, params)
}
