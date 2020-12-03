library(here)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(here)
library(reticulate)
library(scales)
library(truncnorm)
use_condaenv("anaconda3/py36")
library(greta)
source(here("R", "utils.R"))
source(here("R", "utils-exp1.R"))
source(here("R", "utils-exp2.R"))

# General Data ------------------------------------------------------------
# exp.name = "wor(l)ds-of-toy-blocks"
exp.name = "toy-blocks-pilot-2"

IDS.dep=c("if1_uh", "if1_u-Lh", "if1_hh", "if1_lh",
          "if2_ul", "if2_u-Ll", "if2_hl", "if2_ll");
IDS.ind = c("independent_ll", "independent_hh", "independent_hl",
            "independent_ul", "independent_uh"); #, "ind2");
IDS = c(IDS.dep, IDS.ind)

CNS.dep = c("A implies C", "A implies -C", "C implies A", "C implies -A");
ID_CNS = rbind(expand.grid(id=IDS.ind, cn=c("A || C"), stringsAsFactors=FALSE),
               expand.grid(id=IDS.dep, cn=c("A implies C", "C implies A"),
                           stringsAsFactors=FALSE));
questions.train = c("ry", "r", "y", "none")
labels.train = c(ry="Red and Yellow", r="Red and ¬Yellow",
                 y="¬Red and Yellow", none="¬Red and ¬Yellow")

questions.test = c("bg", "b", "g", "none")
labels.test = c(bg="Blue and Green", b="Blue and ¬Green",
                 g="¬Blue and Green", none="¬Blue and ¬Green")

train.edges = tribble(
  ~id, ~block, ~condition, ~dir,
  "ssw0", "yellow", "uncertain", "vert",
  "ssw0", "red", "low", "horiz",
  "ssw1", "red", "uncertain", "vert",
  "ssw1", "yellow", "low", "horiz",
  "uncertain0", "red", "uncertain", "horiz",
  "uncertain0", "yellow", "uncertain", "vert",
  "uncertain1", "red", "uncertainH", "horiz",
  "uncertain1", "yellow", "low", "horiz",
  "uncertain2", "red", "high", "horiz",
  "uncertain2", "yellow", "uncertain", "vert",
  "uncertain3", "red", "low", "vert",
  "uncertain3", "yellow", "high", "vert",
  "ac0", "red", "high", "horiz",
  "ac1", "yellow", "high", "vert",
  "ac2", "red", "uncertainH", "horiz",
  "ac3", "yellow", "uncertain", "vert",
  "ind0", "yellow", "low", "vert",
  "ind1", "yellow", "uncertain", "vert",
  "ind2", "green", "high", "vert",
  "ssw0", "yellow", "uncertainH", "vert",
  "ssw0", "red", "low", "horiz",
  "ssw1", "yellow", "low", "horiz",
  "ssw1", "red", "uncertain", "vert"
);
train.ramp = tribble(
  ~id, ~block, ~condition, ~dir,
  "ac0", "yellow", "high", "horiz",
  "ac1", "red", "low", "vert",
  "ac2", "yellow", "uncertain", "vert",
  "ac3", "red", "high", "horiz",
  "ind0", "red", "uncertainH", "vert",
  "ind1", "red", "high", "vert",
  "ind2", "blue", "uncertain", "horiz"
)

prior.dir_ramp = tribble(~relation, ~condition, ~dir, ~event,
                         "if2", "hl", "vert", "p_blue",
                         "if2", "ll", "horiz", "p_blue",
                         "if2", "ul", "vert", "p_blue",
                         "if2", "u-Ll", "vert", "p_blue",
                         "independent", "hh", "vert", "p_blue",
                         "independent", "hl", "vert", "p_green",
                         "independent", "ll", "horiz", "p_green",
                         "independent", "uh", "vert", "p_green",
                         "independent", "ul", "horiz", "p_green",
                         "if1", "hh", "vert", "g_given_b",
                         "if1", "lh", "vert", "g_given_b",
                         "if1", "uh", "vert", "g_given_b",
                         "if1", "u-Lh", "vert", "g_given_b")

prior.dir_edges = tribble(~relation, ~condition, ~dir, ~event,
                          "independent", "hh", "vert", "p_green",
                          "independent", "hl", "horiz", "p_blue",
                          "independent", "ll", "vert", "p_blue",
                          "independent", "uh", "horiz", "p_blue",
                          "independent", "ul", "vert", "p_blue",
                          "if2", "u-Ll", "horiz", "g_given_nb",
                          "if2", "ul", "horiz", "g_given_nb",
                          "if2", "hl", "horiz", "g_given_nb",
                          "if2", "ll", "horiz", "g_given_nb"
)

RESULT.dir = here("data", "prolific", "results", exp.name)
PLOT.dir = here("data", "prolific", "results", exp.name, "plots")
if(!dir.exists(PLOT.dir)){dir.create(PLOT.dir, recursive=TRUE)}

epsilon = 0.00001
SEP = .Platform$file.sep

# Experimental Data -------------------------------------------------------
data <- readRDS(paste(RESULT.dir, SEP, exp.name, "_tidy.rds", sep=""));
data.info = data$info
data.comments = data$comments
data.color = data$color 

data.production = readRDS(paste(RESULT.dir, "human-exp2.rds", sep=SEP));
data.prior.norm = readRDS(paste(RESULT.dir, "human-exp1-normed.rds", sep=SEP))
data.prior.orig = readRDS(paste(RESULT.dir, "human-exp1-orig.rds", sep=SEP))
data.joint = readRDS(paste(RESULT.dir, "human-exp1-exp2.rds", sep=SEP))
data.joint.orig = readRDS(paste(RESULT.dir, "human-orig-exp1-exp2.rds", sep=SEP))

data.quality = readRDS(paste(RESULT.dir, SEP, "filtered_data", SEP,
                             "test-data-prior-quality.rds", sep=""))
data.distances = readRDS(paste(RESULT.dir, "distances-quality.rds", sep=SEP))

# empirical tables (test-trials)
TABLES.ind = readRDS(paste(RESULT.dir,"empiric-ind-tables-smooth.rds", sep=SEP)) %>%
  filter(id != "ind2")
TABLES.dep = readRDS(paste(RESULT.dir, "empiric-dep-tables-smooth.rds", sep=SEP))
TABLES.all = bind_rows(TABLES.ind, TABLES.dep)

data.train.norm = data$train.norm
data.train.orig = data$train.orig
# for each participant only the last 50% of all train trials
data.train.norm.half = data.train.norm %>% 
  separate(id, into=c("trial.relation", "trial.idx"), sep=-1, remove=FALSE) %>%
  group_by(prolific_id, trial.relation) %>% arrange(desc(trial_number)) %>%
  top_frac(0.5, trial_number) %>% 
  select(-expected) %>% 
  pivot_wider(names_from=question, values_from=response) %>%
  ungroup() %>% 
  select(-trial.relation, -trial.idx)


# Model Prediction Data ---------------------------------------------------
path.results_model <- here("..", "MA-project", "conditionals", "data",
                           "predictions-tables-stimuli")
data.model <- readRDS(paste(path.results_model, "results-tables-stimuli.rds", sep=SEP)) %>%
  separate(stimulus_id, into=c("stimulus", "id"), sep="--") %>%
  group_by(id) %>% 
  select(stimulus, cn, id, AC, `A-C`, `-AC`, `-A-C`, utterance, probs)

best_utt = function(data.model, group="bg"){
  df = data.model %>% mutate(m=max(probs)) %>%
    filter(m==probs) %>% rename(response=utterance, model.p=probs) %>% 
    select(-m)
  return(translate_utterances(df, group) %>% rename(model=response))
}
data.model.best = best_utt(data.model)

model.avg = data.model %>% group_by(stimulus, utterance) %>% 
  summarise(sd=sd(probs), ratio=mean(probs), .groups="drop_last")
model.avg.best = best_utt(model.avg %>% rename(probs=ratio), group="bg")

# translate utterances for avg  model data and all model data
model.avg = model.avg %>% rename(response=utterance, id=stimulus) %>%
  translate_utterances() %>% add_column(predictor="model")

data.model = data.model %>%
  rename(table_id=id, response=utterance, id=stimulus) %>%
  translate_utterances() %>% group_by(table_id, cn)

model.all = data.model %>% ungroup() %>% 
  distinct_at(vars(c(`AC`, `A-C`, `-AC`, `-A-C`, cn)), .keep_all = TRUE) %>%
  group_by(table_id, cn)
  
mapping_table_participant = readRDS(
  here("..", "MA-project", "conditionals", "data",
       "mapping-tableID-prolificID.rds")) %>%
  select(-empirical) %>%
  separate(stimulus_id, into=c("stimulus_src", "table_id"), sep="--")
  # separate(trial_id, into=c("stimulus_id", "prolific_id"), sep= "--")



# Other -------------------------------------------------------------------
# ordered by informativity
levels.responses = rev(c(
  standardized.sentences$bg, standardized.sentences$none,
  standardized.sentences$g, standardized.sentences$b,
  standardized.sentences$only_b, standardized.sentences$only_g,
  standardized.sentences$only_nb, standardized.sentences$only_ng,
  standardized.sentences$if_bg, standardized.sentences$if_gb, 
  standardized.sentences$if_nbng, standardized.sentences$if_ngnb,
  standardized.sentences$if_bng, standardized.sentences$if_gnb,
  standardized.sentences$if_nbg, standardized.sentences$if_ngb,
  standardized.sentences$might_b, standardized.sentences$might_g,
  standardized.sentences$might_nb, standardized.sentences$might_ng
))




