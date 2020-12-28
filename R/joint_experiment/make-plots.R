library(cowplot)
source("R/joint_experiment/analysis-utils.R")

pids = data.joint.orig %>% pull(prolific_id) %>% unique()
stimuli =  data.prior.orig %>% pull(id) %>% unique()

getData =function(normalized=TRUE){
  if(normalized){
    df=data.joint
  } else {
    df = data.joint.orig
  }
  df = df %>%
    mutate(question = case_when(utterance==standardized.sentences$bg ~ "bg",
                                utterance==standardized.sentences$b ~ "b",
                                utterance==standardized.sentences$g ~ "g",
                                utterance==standardized.sentences$none ~ "none", 
                                TRUE ~ utterance)) %>%
    filter((question %in% questions.test) |
             !is.na(human_exp2))
  return(df)
}
df.norm = getData()
df.orig = getData(normalized=FALSE)


# summary slider-ratings ----------------------------------------------------
df.wide = TABLES.dep %>% 
  # filter(prolific_id %in% pids.false_colors) %>%
  group_by(prolific_id, id) %>%
  rename(bg=AC, b=`A-C`, g=`-AC`, none=`-A-C`) %>%
  select(bg, b, g, none, prolific_id, id) %>%
  rowid_to_column()
df.long = df.wide %>% 
  pivot_longer(cols=c(bg, b, g, none), names_to="question", values_to="response") 
save_to = paste(PLOT.dir, "slider-ratings-densities", sep=fs)
if(!dir.exists(save_to)) {dir.create(save_to)}
df.long %>%
  plotSliderDensities(questions.test, labels.test, target_dir=save_to)

save_to = paste(PLOT.dir, "slider-ratings-boxplots", sep=fs)
if(!dir.exists(save_to)) {dir.create(save_to)}
df.long %>% plotSliderRatings(questions.test, labels.test, cluster_by="bg",
                              relation=FALSE, target_dir=save_to)

# Slider Ratings ----------------------------------------------------------
# 1.unnormalized slider ratings with utterance and normalized ratings
save_to = paste(PLOT.dir, "slider-ratings-tables", sep=fs)
if(!dir.exists(save_to)) {dir.create(save_to)}

for(pid in pids) {
  df.pid.orig = df.orig %>% filter(prolific_id == (!! pid))
  df.pid.norm = df.norm %>% filter(prolific_id == (!! pid))
  
  target_folder = paste(save_to, pid, sep=fs) 
  if(!exists(target_folder)){
    dir.create(target_folder)
  }
  for(stimulus in stimuli) {
    df.stim.orig = df.pid.orig %>%
      filter(id==stimulus & question %in% questions.test) %>%
      rename(means=human_exp1)
    if(df.stim.orig %>% nrow() != 0){
      df.stim.norm = df.pid.norm %>% filter(id==stimulus) %>% rename(normalized=human_exp1)
      uttered = df.stim.norm %>% filter(!is.na(human_exp2)) %>% pull(utterance)
      
      stim_utt = paste(stimulus, uttered, sep=": ") 
      df.stim = left_join(df.stim.orig %>% select(-human_exp2),
                          df.stim.norm %>% filter(question %in% questions.test),
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
df = left_join(data.prior.norm %>% select(-question, -RT) %>% rename(human_exp1=response),
               data.production %>% select(-RT) %>% rename(utterance=response),
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

ggsave(paste(PLOT.dir, "prior-vs-utt.png", sep=fs), p, width=15)

#Same plot, here results plotted separately for each participant. Stimuli are color coded.
p <- df %>%
  ggplot(aes(y=utterance, x=human_exp1)) +
  geom_point(aes(color=id)) +
  geom_vline(aes(xintercept=0.7), color="black", linetype='dashed') +
  facet_wrap(~prolific_id) +
  theme_bw(base_size = 14) +
  theme(legend.position="none") +
  labs(x="rated probability (prior elicitation)", y="response")
ggsave(paste(PLOT.dir, "prior-vs-utt-per-proband.png", sep=fs), p, width=15, height=18)


ratios_large = df %>% group_by(prolific_id) %>%
  summarize(n=n(), ratio_large=sum(human_exp1 >= 0.7)/n) %>%
  arrange(desc(ratio_large))
ids.large = ratios_large %>% filter(ratio_large>=0.7) %>% pull(prolific_id)
ids.small = ratios_large %>% filter(ratio_large<0.7) %>% pull(prolific_id)

# Slider Ratings
# 2.normalized ratings with produced utterance
fn = paste(PLOT.dir, "by-participants-normalized-ratings-with-production",
           "large_beliefs", sep=fs)
if(!dir.exists(fn)){dir.create(fn, recursive = TRUE)}
plotSliderRatingsAndUtts(data.joint %>% filter(prolific_id %in% ids.large), fn)

fn = paste(PLOT.dir, "by-participants-normalized-ratings-with-production",
           "small_beliefs", sep=fs)
if(!dir.exists(fn)){dir.create(fn, recursive = TRUE)}
plotSliderRatingsAndUtts(data.joint %>% filter(prolific_id %in% ids.small), fn)

# 3.unnormalized ratings with produced utterance
# fn = paste(PLOT.dir, "by-participants-transformed-ratings-with-production", sep=fs)
# if(!dir.exists(fn)){dir.create(fn)}
# plotSliderRatingsAndUtts(data.joint.transformed, fn)



# Data Quality ------------------------------------------------------------
p = data.quality  %>%
  ggplot(aes(x=stimulus_id,  y=sum_sq_diff)) +
  geom_boxplot(aes(colour=stimulus_id), outlier.shape=NA) +
  geom_jitter(height=0, width=0.05, alpha=0.4, size=1.5) +
  theme_bw(base_size = 20) +
  theme(axis.text.x=element_text(angle=90, vjust=0.5), legend.position="none")
ggsave(paste(PLOT.dir, "quality-sum-sq-diff-to-mean.png", sep=fs), p, width=15, height=10)


# Train data --------------------------------------------------------------
plotTrainUncertain = function(fn) {
  df=data.train.norm %>% 
    mutate(question=case_when(question=="bg" ~ "ry",
                              question=="b" ~ "r",
                              question=="g" ~ "y",
                              TRUE ~ question)) %>%
    pivot_wider(names_from="question", values_from="response") %>%
    mutate(red=ry+r, yellow=ry+y, not_red=y+none, not_yellow=r+none,
           ratio_red=case_when(yellow>=not_yellow ~ ry/y,
                               TRUE ~ r/none),
           ratio_yellow=case_when(red>=not_red ~ ry/r,
                                  TRUE ~ y/none)) %>%
    select(prolific_id, id, trial_number, expected, ratio_red, ratio_yellow, red, yellow)

  if(str_detect(fn, "edge")){
    df = left_join(df, train.edges, by="id") %>% filter(!is.na(condition))
  } else {
    df =left_join(df, train.ramp, by="id") %>% filter(!is.na(condition))
  }
  
  df = df %>%
    mutate(block=case_when(block=="blue" ~ "red",
                           block=="green" ~ "yellow",
                           TRUE ~ block)) %>% 
    mutate(falls = case_when(block=="yellow" & expected %in% c("ry", "y") ~ TRUE,
                                       block=="red" & expected %in% c("ry", "r") ~ TRUE,
                                       TRUE ~ FALSE))  
  max_y = df %>% filter(!is.infinite(ratio_yellow)) %>% pull(ratio_yellow) %>% max()
  max_r = df %>% filter(!is.infinite(ratio_red)) %>% pull(ratio_red) %>% max()
  
  df.long = df %>% pivot_longer(cols=c("red", "yellow", "ratio_red", "ratio_yellow"),
                                names_to="key", values_to="p") %>%
    filter(((str_detect(key, "ratio") & str_detect(key, block)) | key==block) &
             startsWith(condition, "uncertain")) %>%
    mutate(value=case_when(str_detect(key, "ratio") ~  "ratio",
                           TRUE ~ "normalized rating"),
           p=case_when(is.infinite(p) ~ max(max_y, max_r),
                       TRUE ~ p))

  for(pid in df.long$prolific_id %>% unique()) {
    p = df.long %>% filter(prolific_id == pid) %>%
      ggplot(aes(x=trial_number, y=p, colour=dir)) +
      geom_point(aes(shape=falls), size=2) +
      geom_line() +
      geom_hline(aes(yintercept=0.5)) +
      geom_hline(aes(yintercept=1)) +
      labs(title=paste("trials uncertain block", fn)) +
      facet_wrap(~value, scales="free_y")
    target = paste(PLOT.dir, "by-participants-train", fn, sep=fs)
    if(!dir.exists(target)) {dir.create(target, recursive = TRUE)}
    ggsave(paste(target, paste(pid, "-", fn, ".png", sep=""), sep=fs), height=8, width=10)
  }
}

plotTrainUncertain("edge")
plotTrainUncertain("ramp")


# Model-vs-human ----------------------------------------------------------
plotModelAndBehavioral = function(use_fitted_tables){
  if(use_fitted_tables){
    save_to = paste(PLOT.dir, "comparison-exp1-exp2-model", "fitted-tables", sep=fs)
    joint_data = data.behav_model.fitted.each
    mapping = readRDS(paste(RESULT.dir, "mapping-tables-fitted-dirichlet-ids.rds", sep=fs))
    prior = readRDS(here("model", "results", "fitted-tables", "results-prior.rds")) %>%
      ungroup() %>% dplyr::select(cn, table_id, prob) %>% distinct()
  } else{
    save_to = paste(PLOT.dir, "comparison-exp1-exp2-model", "theoretic-tables", sep=fs)
    joint_data = data.behav_model.theoretic.each
    mapping = readRDS(here("model", "data", "mapping-tables-model-ids.rds"))
    prior = readRDS(here("model", "results", "theoretic-tables", "results-prior.rds")) %>%
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




# Model -------------------------------------------------------------------
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
# p = "/home/britta/UNI/Osnabrueck/MA-project/conditionals/data/tables-10000-(in)dependent.rds"
p = "/home/britta/UNI/Osnabrueck/prob-modeling-conditionals/data/default-model/tables-default.rds"
tables.theoretic = readRDS(p) %>% dplyr::select(-starts_with("logL"), -seed) %>%
  group_by(id)

analyze_tables("", 0.9, tables.theoretic %>% unnest(c(vs,ps)))



