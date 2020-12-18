library(here)
library(tidyverse)
source("R/utils.R")
source("R/utils-exp1.R")
source("R/utils-exp2.R")

# Setup -------------------------------------------------------------------
# experiment = "prior"
# experiment="production"
experiment = "joint"

# debug_run = TRUE  # vs. experimental (prolific) run
debug_run = FALSE

# data_fn <- "results_15_WorldOfToyBlocks-Pilot_BG.csv"
# data_fn <- "results_38_Experiment-2-Fridge-Pilot_BG_20.csv"
# data_fn <- "results_39_wor(l)ds-of-toy-blocks-pilot_BG.csv"
data_fn <- "results_50_toy-blocks-pilot-2_BG.csv"

# result_fn = "experiment1-v2"
# result_fn = "wor(l)ds-of-toy-blocks"
# result_fn = "experiment2"
result_fn = "toy-blocks-pilot-2"

data_dir = ifelse(debug_run,  here("data", "test-runs"), here("data", "prolific"));
result_dir <- paste(data_dir, "results", result_fn, sep=.Platform$file.sep)
if(!dir.exists(result_dir)){
  dir.create(result_dir, recursive=TRUE);
}
# Processing --------------------------------------------------------------
if(experiment == "prior"){
  N_trials <- list(train=14, test=15, color_vision=0);
} else if(experiment == "production") {
  N_trials <- list(train=15, test=18, color_vision=8);
} else if(experiment == "joint"){
  # N_trials <- list(train=14, test=19*2, color_vision=8)
  # N_trials <- list(train=14, test=14*2, color_vision=6);
  N_trials = list(train=14+3+10, test=13*2+1, color_vision=6,
                  slider_choice=10, attention_check=3);
}
data <- process_data(data_dir, data_fn, result_dir, result_fn, debug_run,
                     N_trials, experiment)
# create dir for filtered data if filtered later
filtered_dir <- paste(result_dir, "filtered_data", sep=.Platform$file.sep)
if(!dir.exists(filtered_dir)){
  dir.create(filtered_dir, recursive=TRUE);
}

# Save data in different formats ------------------------------------------
# Quality of data in slider ratings: squared distance to mean each table cell
# entry (considering all participants) summed for each participant across all
# 4 questions/joint events
test.prior = data$test %>% filter(str_detect(trial_name, "multiple_slider"))
prior.quality = test.prior %>%  dplyr::select(-response, -custom_response) %>% 
  responsesSquaredDiff2Mean() %>%
  mutate(stimulus_id=factor(stimulus_id))
saveRDS(object=prior.quality,
        file=paste(filtered_dir, "test-data-prior-quality.rds", sep=.Platform$file.sep))

# 2. merge data from prior elicitation and production
getPriorElicitation = function(test.prior, normalized=TRUE){
  df.prior_responses = test.prior %>%
    dplyr::select(-custom_response, -QUD, -trial_number, -trial_name) 
  if(normalized){
    df.prior_responses = df.prior_responses %>% dplyr::select(-r_orig) %>% 
      pivot_wider(names_from = "question", values_from = "r_norm")
  } else {
    df.prior_responses = df.prior_responses %>% dplyr::select(-r_norm) %>%
      pivot_wider(names_from = "question", values_from = "r_orig")
  }
  df.prior_responses = df.prior_responses %>% add_probs()
  
  prior_responses = df.prior_responses %>%
    pivot_longer(cols=c("b", "g", "bg", "none", starts_with("p_")),
                 names_to="prob", values_to="val") %>%
    mutate(utterance=
             case_when(prob=="b" ~ standardized.sentences$b,
                       prob=="g" ~ standardized.sentences$g,
                       prob=="bg" ~ standardized.sentences$bg,
                       prob=="none" ~ standardized.sentences$none,
                       prob=="p_a" ~ "blue falls",
                       prob=="p_c" ~ "green falls",
                       prob=="p_na" ~ "blue does not fall",
                       prob=="p_nc" ~ "green does not fall",
                       prob=="p_c_given_a" ~ standardized.sentences$if_bg,
                       prob=="p_c_given_na" ~ standardized.sentences$if_nbg,
                       prob=="p_a_given_c" ~ standardized.sentences$if_gb,
                       prob=="p_a_given_nc" ~ standardized.sentences$if_ngb,
                       prob=="p_nc_given_a" ~ standardized.sentences$if_bng,
                       prob=="p_nc_given_na" ~ standardized.sentences$if_nbng,
                       prob=="p_na_given_c" ~ standardized.sentences$if_gnb,
                       prob=="p_na_given_nc" ~ standardized.sentences$if_ngnb,
                       prob=="p_likely_a" ~ "blue might fall",
                       prob=="p_likely_na" ~ "blue might not fall",
                       prob=="p_likely_c" ~ "green might fall",
                       prob=="p_likely_nc" ~ "green might not fall",
                       TRUE ~ NA_character_)
    )
  
  exp1.human = prior_responses %>% dplyr::select(-group, -n) %>%
    rename(human_exp1=val, question=prob) %>% 
    mutate(question = case_when(!question %in% c("bg", "b", "g", "none") ~ NA_character_,
                                TRUE ~ question))
  return(exp1.human)
}
exp1.human.orig = getPriorElicitation(test.prior, normalized = FALSE) %>% dplyr::select(-response)
exp1.human.norm = getPriorElicitation(test.prior, normalized = TRUE) %>% dplyr::select(-response)

test.production = data$test %>%
  filter(str_detect(trial_name, "fridge_")) %>%
  standardize_sentences();
exp2.human = test.production %>%
  dplyr::select(prolific_id, id, response, RT, custom_response) %>%
  rename(utterance=response) %>% add_column(human_exp2=1)
joint.human = left_join(exp1.human.norm %>% dplyr::select(-question, -RT),
                        exp2.human %>% dplyr::select(-RT, -custom_response),
                        by=c("prolific_id", "id", "utterance"))

joint.human.orig = left_join(
  exp1.human.orig %>% dplyr::select(-question, -RT),
  exp2.human %>% dplyr::select(-RT, -custom_response),
  by=c("prolific_id", "id", "utterance"))
saveRDS(joint.human.orig, paste(result_dir, "human-orig-exp1-exp2.rds", sep=.Platform$file.sep))


saveRDS(exp2.human %>% rename(response=utterance),
        paste(result_dir, "human-exp2.rds", sep=.Platform$file.sep))
saveRDS(exp1.human.orig %>% rename(response=human_exp1),
        paste(result_dir, "human-exp1-orig.rds", sep=.Platform$file.sep))
saveRDS(exp1.human.norm %>% rename(response=human_exp1),
        paste(result_dir, "human-exp1-normed.rds", sep=.Platform$file.sep))
saveRDS(joint.human, paste(result_dir, "human-exp1-exp2.rds", sep=.Platform$file.sep))


df = exp1.human.norm %>% rename(r_norm=human_exp1) %>% dplyr::select(-utterance) %>% 
  filter(!is.na(question))
distances = distancesResponses(df)
saveRDS(object=distances,
        file=paste(result_dir, "distances-quality.rds", sep=.Platform$file.sep))

# generate tables that are provided to model
tables.model = makeAndSaveModelTables()

# functions ---------------------------------------------------------------
filter_data = function(target_dir, exp.name, by_quality=FALSE,
                       by_color_vision=FALSE, out.by_comments=NA){
  # load smoothed tables
  tables = readRDS(paste(target_dir, "empiric-all-tables-smooth.rds", sep=.Platform$file.sep))
  data = readRDS(paste(target_dir, .Platform$file.sep, exp.name, "_tidy.rds", sep=""))
  stimuli = data$test$id %>% unique()
  df = tibble()
  if(by_quality) {
    exp1.quality = readRDS(paste(paste(target_dir, "filtered_data", sep=.Platform$file.sep),
                           "test-data-prior-quality.rds", sep=.Platform$file.sep))
    dat.quality = exp1.quality %>%
      # mutate(quantiles=list(quantile(sum_sq_diff))) %>%
      mutate(iqr=IQR(sum_sq_diff), mean=mean(sum_sq_diff),
             outlier=sum_sq_diff < (mean-2*iqr) | (sum_sq_diff>mean + 2*iqr)) %>%
      filter(!outlier) %>% dplyr::select(prolific_id, stimulus_id)
    
    df = bind_rows(df, right_join(tables, dat.quality %>% rename(id=stimulus_id)))
  }
  
  if(by_color_vision){
    dat.color = data$color %>%
      mutate(correct = expected == response,  N=max(trial_number)) %>%
      filter(correct) %>% group_by(prolific_id) %>%
      mutate(n=n()) %>% filter(n == N) %>% dplyr::select(prolific_id) %>% unique() %>%
      add_column(id=list(stimuli)) %>%
      unnest(c(id))

    message(paste("#participants excluded as at least 1 wrong color question:",
            length(data$color$prolific_id %>% unique())-length(dat.color$prolific_id %>% unique)))
    df=right_join(df, dat.color)
  }
  
  if(!is.na(out.by_comments)){
    df = anti_join(df, out.by_comments)
  }
  save_to = paste(target_dir, "filtered_data",
                  "empiric-filtered-tables-smooth.rds", sep=.Platform$file.sep)
  saveRDS(df, save_to)
  print(paste("saved filtered smooth tables to:", save_to))
  message(paste("remaining tables:", nrow(df), "(", nrow(df)/nrow(tables), ")"))
  return(df)
}

# df.filtered=filter_data(result_dir, result_fn, by_quality=TRUE, by_color_vision=TRUE)


