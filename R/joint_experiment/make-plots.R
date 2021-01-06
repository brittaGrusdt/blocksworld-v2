library(cowplot)
library(ggpubr)
source("R/joint_experiment/analysis-utils.R")

pids = data.joint.orig %>% pull(prolific_id) %>% unique()
stimuli =  data.prior.orig %>% pull(id) %>% unique()

df.orig = data.joint.orig %>% 
  mutate(question = case_when(utterance==standardized.sentences$bg ~ "bg",
                               utterance==standardized.sentences$b ~ "b",
                               utterance==standardized.sentences$g ~ "g",
                               utterance==standardized.sentences$none ~ "none", 
                               TRUE ~ utterance))
df.smooth = data.joint.smooth %>%
  mutate(question = case_when(utterance==standardized.sentences$bg ~ "bg",
                               utterance==standardized.sentences$b ~ "b",
                               utterance==standardized.sentences$g ~ "g",
                               utterance==standardized.sentences$none ~ "none", 
                               TRUE ~ utterance))
  
# summary slider-ratings ----------------------------------------------------
tables.smooth = data.joint.smooth %>%
  dplyr::select(-human_exp2) %>% distinct() %>% 
  mutate(utterance = case_when(utterance==standardized.sentences$bg ~ "bg",
                              utterance==standardized.sentences$b ~ "b",
                              utterance==standardized.sentences$g ~ "g",
                              utterance==standardized.sentences$none ~ "none", 
                              TRUE ~ utterance)) %>%
  filter((utterance %in% questions.test)) %>%
  pivot_wider(names_from="utterance", values_from="human_exp1") %>%
  group_by(prolific_id, id) %>%
  rowid_to_column()

tables.long = tables.smooth %>% 
  pivot_longer(cols=c(bg, b, g, none), names_to="question", values_to="response") 
save_to = paste(PLOT.dir, "slider-ratings-densities", sep=fs)
if(!dir.exists(save_to)) {dir.create(save_to)}
tables.long %>%
  plotSliderDensities(questions.test, labels.test, target_dir=save_to)

save_to = paste(PLOT.dir, "slider-ratings-boxplots", sep=fs)
if(!dir.exists(save_to)) {dir.create(save_to)}
tables.long %>%
  plotSliderRatings(questions.test, labels.test, cluster_by="bg",
                    relation=FALSE, target_dir=save_to)

# Slider Ratings ----------------------------------------------------------
# 1.slider ratings with utterance and normalized ratings
save_to = paste(PLOT.dir, "slider-ratings-tables", sep=fs)
if(!dir.exists(save_to)) {dir.create(save_to)}

for(pid in pids) {
  df.pid.orig = df.orig %>% filter(prolific_id == (!! pid))
  df.pid.smooth = df.smooth %>% filter(prolific_id == (!! pid))
  
  target_folder = paste(save_to, pid, sep=fs) 
  if(!dir.exists(target_folder)){ dir.create(target_folder)
  }
  for(stimulus in stimuli) {
    df.stim.orig = df.pid.orig %>%
      filter(id==stimulus & question %in% questions.test) %>%
      rename(means=human_exp1)
    if(df.stim.orig %>% nrow() != 0){
      df.stim.smooth = df.pid.smooth %>% filter(id==stimulus) %>% rename(normalized=human_exp1)
      uttered = df.stim.smooth %>% filter(!is.na(human_exp2)) %>% pull(utterance)
      
      stim_utt = paste(stimulus, uttered, sep=": ") 
      df.stim = left_join(df.stim.orig %>% dplyr::select(-human_exp2),
                          df.stim.smooth %>% filter(question %in% questions.test),
                          by=c("prolific_id", "id", "question"))
      p = df.stim %>% PlotMeans(stimulus, sd_error_bars = FALSE) + 
        labs(y="unnormalized slider rating", x="",title=stim_utt) +
        geom_point(aes(y=normalized), size=2, color='orange') +
        theme(axis.text.x=element_blank(), legend.position="none")
      ggsave(paste(save_to, fs, pid, fs, stimulus, ".png", sep=""), p, width=12)
    }
  }
}
# Literal meaning probabilities -------------------------------------------
df = left_join(data.prior.smooth %>% dplyr::select(-question, -RT) %>%
                 rename(human_exp1=response),
               data.production %>% dplyr::select(-RT) %>% rename(utterance=response),
               by=c("id", "prolific_id", "utterance")) %>%
  filter(!is.na(human_exp2)) %>% group_by(id) %>% 
  mutate(human_exp1=case_when(str_detect(utterance, "might") & human_exp1>0.1 ~ 1,
                              TRUE ~ human_exp1))
p <- df %>%
  ggplot(aes(y=utterance, x=human_exp1)) +
  geom_point(aes(color=id)) +
  geom_vline(aes(xintercept=0.7), color="black", linetype='dashed') +
  theme_bw(base_size = 20) +
  theme(text = element_text(size=20), legend.position="bottom") +
  labs(x="rated probability (prior elicitation)", y="response")

ggsave(paste(PLOT.dir, "prior-vs-utt.png", sep=fs), p, width=14, height=7)

#Same plot, here results plotted separately for each participant. Stimuli are color coded.
p <- df %>%
  ggplot(aes(y=utterance, x=human_exp1)) +
  geom_point(aes(color=id)) +
  geom_vline(aes(xintercept=0.7), color="black", linetype='dashed') +
  facet_wrap(~prolific_id) +
  theme_bw(base_size = 14) +
  theme(legend.position="none") +
  labs(x="rated probability (prior elicitation)", y="response")
ggsave(paste(PLOT.dir, "prior-vs-utt-per-proband.png", sep=fs), p, width=18, height=18)

# Slider Ratings
# normalized ratings with produced utterance
fn = paste(PLOT.dir, "by-participants-normalized-ratings-with-production", sep=fs)
if(!dir.exists(fn)){dir.create(fn, recursive = TRUE)}
plotSliderRatingsAndUtts(data.joint.smooth, fn)
# todo here

# Data Quality ------------------------------------------------------------
p = data.quality  %>%
  ggplot(aes(x=stimulus_id,  y=sum_sq_diff)) +
  geom_boxplot(aes(colour=stimulus_id), outlier.shape=NA) +
  geom_jitter(height=0, width=0.05, alpha=0.4, size=1.5) +
  theme_bw(base_size = 20) +
  theme(axis.text.x=element_text(angle=90, vjust=0.5), legend.position="none")
ggsave(paste(PLOT.dir, "quality-sum-sq-diff-to-mean.png", sep=fs), p, width=15, height=10)


# Model-vs-human ----------------------------------------------------------
# todo: go through this again!
plotModelAndBehavioral = function(use_fitted_tables){
  if(use_fitted_tables){
    save_to = paste(PLOT.dir, "comparison-exp1-exp2-model", "fitted-tables", sep=fs)
    joint_data = data.behav_model.fitted.each
    mapping = readRDS(paste(RESULT.dir, "mapping-tables-dirichlet-empirical-ids.rds", sep=fs))
    prior = readRDS(here("model", "results", "fitted-tables", "results-prior.rds")) %>%
      ungroup() %>% dplyr::select(cn, table_id, prob) %>% distinct()
  } else{
    save_to = paste(PLOT.dir, "comparison-exp1-exp2-model", "tables-model", sep=fs)
    joint_data = data.behav_model.theoretic.each
    mapping = readRDS(here("model", "data", "mapping-tables-model-ids.rds"))
    prior = readRDS(here("model", "results", "tables-model", "results-prior.rds")) %>%
      ungroup() %>% dplyr::select(cn, table_id, prob) %>% distinct()
  }
  if(!dir.exists(save_to)) {dir.create(save_to)}
  
  dat = left_join(
    joint_data, 
    mapping %>% dplyr::select(empirical_id, p_id) %>% unnest(c("p_id")) %>% distinct(),
    by=c("empirical_id")
  )
  dat = dat %>% 
    separate(p_id, into=c("pid", "stimulus", "prior"), sep="_") %>%
    unite("stimulus", c("stimulus", "prior"), sep="_") %>%
    filter(id==stimulus & prolific_id == pid) %>%
    dplyr::select(-pid, -stimulus) %>% group_by(table_id) %>% 
    mutate(utterance=factor(utterance, levels=levels.responses))
  
  df.ids = dat %>% ungroup() %>% dplyr::select(prolific_id, id, empirical_id) %>%
    distinct() %>% rowid_to_column("rowid")
  
  for(i in seq(1, nrow(df.ids))) {
    if(i%%10==0) print(i)
    pid = df.ids[i,]$prolific_id
    stimulus = df.ids[i,]$id
    df.row = dat %>% filter(prolific_id == (!! pid) & id == (!! stimulus)) %>%
      mutate(table_id=as.factor(table_id))
    target_folder = paste(save_to, pid, sep=fs) 
    if(!dir.exists(target_folder)) dir.create(target_folder);
    
    behavioral = df.row %>% ungroup() %>%
      dplyr::select(-table_id, -model.p, -model.table, -orig.table) %>% distinct()
    behavioral.uttered = behavioral %>% filter(human_exp2==1) 
    table_ids = df.row$table_id %>% unique()
    df.prior = prior %>% filter(table_id %in% table_ids) %>%
      group_by(table_id) %>%
      mutate(table_id=as.factor(table_id), cn=as.factor(cn),
             s_prob=sum(prob), p=prob/s_prob) 
      
    p.prior = df.prior %>% filter() %>% 
      ggplot(aes(y=prob, x=table_id, fill=cn)) +
      geom_bar(stat="identity", position=position_dodge()) +
      theme_classic(base_size = 20) +
      theme(legend.position="bottom") +
      labs(y="prior(cn, table)")
    
    ggsave(paste(save_to, fs, pid, fs, stimulus, "-prior", ".png", sep=""),
           p.prior, height=12, width=20)
    
    p.speaker = behavioral %>%
      ggplot(aes(y=utterance)) +
      geom_bar(data=behavioral, aes(x=human_exp1), stat="identity", color='grey') +
      geom_point(data=df.row %>%
                   filter(model.p > 0), aes(x=model.p, color=table_id),
                 size=6) +
      geom_point(data=behavioral.uttered, aes(x=human_exp2, shape=utterance),
                 color='orange', size=8) +
      theme_classic(base_size=20) +
      theme(legend.position="bottom") +
      geom_vline(aes(xintercept=0.8), color="gray", linetype="solid", size=1,
                 show.legend=FALSE)
    tbl.orig = df.row %>% filter(orig.table)
    if(tbl.orig %>% nrow() != 0){
      p.speaker = p.speaker +
        geom_point(data=tbl.orig, aes(x=model.p, color=table_id, shape=orig.table),
                   size=6) +
      scale_shape_manual(
        name="", values = c(8, rep(18, length(levels.responses))),
        breaks=c(TRUE, levels.responses),
        labels=c("prediction for exact empirical table", rep("utterance participant",
                  length(levels.responses)))
      )
    } else {
      p.speaker = p.speaker +
        scale_shape_manual(
          name="", values = rep(18, length(levels.responses)),
          breaks=levels.responses,
          labels=rep("utterance participant", length(levels.responses))
        )
    }
    # p=plot_grid(p.speaker, p.prior, align = "h", nrow = 1, axis = "b",
    #           rel_widths = c(0.6, 0.4))
    ggsave(paste(save_to, fs, pid, fs, stimulus, ".png", sep=""), p.speaker,
           height=12, width=20)
  }
}

# model predictions with theoretic/dirichlet-fitted tables
plotModelAndBehavioral(use_fitted_tables = FALSE)
plotModelAndBehavioral(use_fitted_tables = TRUE)


plotAveragePredictions = function(tables_fn, across_empirical){
  if(across_empirical){
    fn = "predictions-empirical-based"
    data = readRDS(here("model", "results", tables_fn,
                        paste("model-behavioral-avg-stimuli_", fn, ".rds", sep="")))
  } else {
    fn = "predictions-stimulus-prior-based"
    data = readRDS(here("model", "results", tables_fn,
                        paste("model-behavioral-avg-stimuli_", fn, ".rds", sep="")))
  }
  data = data %>% mutate(utterance=factor(utterance, levels = levels.responses),
                         predictor=as.factor(predictor))

  p.bars = data %>% filter(p > 0) %>% arrange(desc(p)) %>% 
    ggplot(aes(x=p, y=utterance, fill=predictor)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw(base_size=20) +
    theme(legend.position="bottom") +
    labs(x="ratio participants/average model prediction") +
    facet_wrap(~stimulus)
  target_dir = paste(PLOT.dir, "comparison-exp1-exp2-model", tables_fn, sep=fs)
  if(!dir.exists(target_dir)){dir.create(target_dir)}
  
  ggsave(paste(target_dir, paste(fn, "bars.png", sep="_"), sep=fs), p.bars,
         height=20, width=16)

  data.wide = data %>% pivot_wider(names_from="predictor", values_from="p")
  p.scatter = 
    ggscatter(data.wide, y = "behavioral", x = "model", add = "reg.line",
              conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
              ylab = "Empirical observations", xlab = "Model predictions") +
    geom_point(data=data.wide, aes(y=behavioral, x=model, color=utterance)) +
    theme_bw(base_size=20) + theme(legend.position = "top") 
  ggsave(paste(target_dir, paste(fn, "scattered.png", sep="_"), sep=fs),
         p.scatter, height=12, width=20)
  
  p.scatter.stim = p.scatter + facet_wrap(~stimulus)
  ggsave(paste(target_dir, paste(fn, "scattered-stim.png", sep="_"), sep=fs),
         p.scatter.stim, height=14, width=20)

}
plotAveragePredictions("tables-dirichlet", TRUE)
plotAveragePredictions("tables-dirichlet", FALSE)
plotAveragePredictions("tables-model", TRUE)





# Following code needs to be tidied up!

# Model-theoretic-tables --------------------------------------------------
tbls.empiric = readRDS(paste(RESULT.dir, "tables-empiric-pids.rds", sep=fs))
theta=0.8

joint_data = data.behav_model.theoretic.each

p = here("model", "data", "tables-model-empirical.rds")
tbls.theoretic = readRDS(p) %>% dplyr::select(-starts_with("logL")) %>%
  dplyr::select(-empirical, -orig_table, -vs, -ps, -p_id, -stimulus)
df.best = joint_data %>% group_by(table_id, prolific_id, id) %>% 
  mutate(best.utt= model.p== max(model.p)) %>% filter(best.utt)

df = df.best %>% 
  filter(model.p > 0 & str_detect(utterance, "if") & 
         str_detect(id, "independent") & human_exp1 > theta) %>%
  group_by(prolific_id, id) %>% arrange(desc(model.p)) %>%
  distinct_at(vars(c(prolific_id, id)), .keep_all = TRUE)

df = joint_data %>% 
  filter(model.p > 0 & str_detect(utterance, "if") & 
           str_detect(id, "independent") & human_exp1 > theta) %>%
  group_by(prolific_id, id) %>% arrange(desc(model.p)) %>%
  distinct_at(vars(c(prolific_id, id)), .keep_all = TRUE)

x=df[3,]

tbls.theoretic %>% filter(table_id==x$table_id)
tbls.empiric %>% filter(empirical_id == x$empirical_id)


# Model-fitted-tables -----------------------------------------------------

joint_data = data.behav_model.fitted.each
p = here("data", "prolific","results", "toy-blocks-pilot-2",
         "tables-dirichlet-empirical.rds")
tbls.fiited = readRDS(p) %>% dplyr::select(-starts_with("logL")) %>%
  dplyr::select(-empirical, -orig_table, -vs, -ps, -p_id, -stimulus)
df.best = joint_data %>% group_by(table_id, prolific_id, id) %>% 
  mutate(best.utt= model.p== max(model.p)) %>% filter(best.utt)

theta=0.8
df = df.best %>% 
  filter(model.p > 0 & str_detect(utterance, "if") & 
           str_detect(id, "independent") & human_exp1 > theta) %>%
  group_by(prolific_id, id)

df = joint_data %>% 
  filter(model.p > 0 & str_detect(utterance, "if") & 
           str_detect(id, "independent") & human_exp1 > theta) %>%
  group_by(prolific_id, id) %>% arrange(desc(model.p)) %>%
  distinct_at(vars(c(prolific_id, id)), .keep_all = TRUE)

x=df[3,]

tbls.fitted = readRDS(here("model", "data", "mapping-tables-dirichlet-ids.rds"))
tbls.fitted %>% filter(table_id==x$table_id)
tbls.empiric %>% filter(empirical_id == x$empirical_id)











# Model Tables ---------------------------------------------------------------
path.results_model <- here("model", "data", "results-prior.rds")
model.tables = readRDS(paste(RESULT.dir, "model-tables-stimuli.rds", sep=fs)) %>%
  dplyr::select(table_id, stimulus, empirical, vs, ps) %>% unnest(c(vs,ps)) %>%
  rename(id=stimulus)
model.tables.wide = model.tables %>% pivot_wider(names_from="vs", values_from="ps")

analyze_tables("", 0.8, model.tables)

model.prior <- readRDS(path.results_model) %>% ungroup() %>% dplyr::select(-cn)
model.prior.wide = model.prior %>% group_by(bn.id) %>%
  pivot_wider(names_from="cell", values_from="val")

# summary model tables
df.wide = model.tables %>% pivot_wider(names_from="vs", values_from="ps") %>% 
  group_by(table_id, id) %>%
  rename(bg=AC, b=`A-C`, g=`-AC`, none=`-A-C`) %>%
  dplyr::select(bg, b, g, none, table_id, id) %>%
  rowid_to_column()
df.long = df.wide %>% 
  pivot_longer(cols=c(bg, b, g, none), names_to="question", values_to="response") 
save_to = here("model", "R", "figs")
if(!dir.exists(save_to)) {dir.create(save_to)}
df.long %>%
  plotSliderDensities(questions.test, labels.test, target_dir=save_to, jitter=FALSE)




# analyze tables
df.prior = model.prior.wide %>% rename(table_id = bn.id) %>%
  dplyr::select(-bn_id, -level, -bias, -starts_with("p_"), -`AC`, -`A-C`, -`-AC`, -`-A-C`)

df = left_join(df.prior, model.tables.wide, by=c("table_id")) %>%
  rename(bg=AC, b=`A-C`, g=`-AC`, none=`-A-C`) %>% add_probs() %>%
  dplyr::select(!starts_with("p_likely"))

# bns where A is true
df.certain_both = df %>% filter((p_a >=0.8 | p_a <0.2) & (p_c >=0.8 | p_c <0.2))
df.certain_both %>% group_by(bn.stimulus) %>% summarize(sum_p = sum(prob)) %>%
  arrange(desc(sum_p))


# model tables
p = here("model", "data", "tables-model.rds")
theta = 0.8
tables.theoretic = readRDS(p) %>% dplyr::select(-starts_with("logL")) %>%
  mutate(blue=AC+`A-C`, green=`-AC` + AC)
# uncertain and no literal is applicable
tbls.unc = tables.theoretic %>%
  filter(((blue > 0.4 & blue < 0.6) | (green > 0.4 & green < 0.6)) &
          !(blue >= theta) & !(green >= theta) &
           !(blue <= 1-theta) & !(green <= 1-theta)) %>%
  pivot_longer(cols=c(AC, `A-C`, `-AC`, `-A-C`), names_to="vs", values_to="ps")

analyze_tables("", theta, tbls.unc)

analyze_tables("", theta, tbls.unc %>% filter(cn == "A || C"))
analyze_tables("", theta, tbls.unc %>% filter(cn != "A || C"))

data.model = load_model_data(fn_tables="theoretic-tables")
data.model = load_model_data(fn_tables="fitted-tables")

prior = data.model$prior %>%
  mutate(cn=case_when(cn=="A || C" ~ cn,
                      TRUE ~ "dependent")) %>% 
  group_by(cn)

# uncertain and no literal is applicable
prior.unc = prior %>%
  mutate(blue=AC+`A-C`, green=`-AC` + AC) %>% 
  filter(((blue > 0.4 & blue < 0.6) | (green > 0.4 & green < 0.6)) &
         !(blue >= theta) & !(green >= theta) & !(blue <= 1-theta) & !(green <= 1-theta))

prior %>% summarize(s=sum(prob), .groups="drop") %>%
  pivot_wider(names_from="cn", values_from="s") %>% mutate(fct=dependent/`A || C`)
prior.unc %>% summarize(s=sum(prob), .groups="drop") %>%
  pivot_wider(names_from="cn", values_from="s") %>% mutate(fct=dependent/`A || C`)


