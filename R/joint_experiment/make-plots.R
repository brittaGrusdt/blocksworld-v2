source("R/joint_experiment/analysis-utils.R")

df.wide = TABLES.dep %>% 
  # filter(prolific_id %in% pids.false_colors) %>%
  group_by(prolific_id, id) %>%
  rename(bg=AC, b=`A-C`, g=`-AC`, none=`-A-C`) %>%
  select(bg, b, g, none, prolific_id, id) %>%
  rowid_to_column()
df.long = df.wide %>% 
  pivot_longer(cols=c(bg, b, g, none), names_to="question", values_to="response") 
save_to = paste(PLOT.dir, "slider-ratings-densities", sep=SEP)
if(!dir.exists(save_to)) {dir.create(save_to)}
df.long %>%
  plotSliderDensities(questions.test, labels.test, target_dir=save_to)

save_to = paste(PLOT.dir, "slider-ratings-boxplots", sep=SEP)
if(!dir.exists(save_to)) {dir.create(save_to)}
df.long %>% plotSliderRatings(questions.test, labels.test, cluster_by="bg",
                              relation=FALSE, target_dir=save_to)


# Slider Ratings ----------------------------------------------------------
# 1.unnormalized slider ratings with utterance and normalized ratings
save_to = paste(PLOT.dir, "slider-ratings-tables", sep=SEP)
if(!dir.exists(save_to)) {dir.create(save_to)}
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

for(pid in pids) {
  df.pid.orig = df.orig %>% filter(prolific_id == (!! pid))
  df.pid.norm = df.norm %>% filter(prolific_id == (!! pid))
  
  target_folder = paste(save_to, pid, sep=SEP) 
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
      ggsave(paste(save_to, SEP, pid, SEP, stimulus, ".png", sep=""), p, width=12)
    }
  }
}

# 2.normalized ratings with produced utterance
fn = paste(PLOT.dir, "by-participants-normalized-ratings-with-production", sep=SEP)
if(!dir.exists(fn)){dir.create(fn)}
plotSliderRatingsAndUtts(data.joint, fn)

  # 3.unnormalized ratings with produced utterance
fn = paste(PLOT.dir, "by-participants-transformed-ratings-with-production", sep=SEP)
if(!dir.exists(fn)){dir.create(fn)}
plotSliderRatingsAndUtts(data.joint.transformed, fn)


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

ggsave(paste(PLOT.dir, "prior-vs-utt.png", sep=SEP), p, width=15)

#Same plot, here results plotted separately for each participant. Stimuli are color coded.
p <- df %>%
  ggplot(aes(y=utterance, x=human_exp1)) +
  geom_point(aes(color=id)) +
  geom_vline(aes(xintercept=0.7), color="black", linetype='dashed') +
  facet_wrap(~prolific_id) +
  theme_bw(base_size = 14) +
  theme(legend.position="none") +
  labs(x="rated probability (prior elicitation)", y="response")
ggsave(paste(PLOT.dir, "prior-vs-utt-per-proband.png", sep=SEP), p, width=15, height=18)


# Data Quality ------------------------------------------------------------
p = data.quality  %>%
  ggplot(aes(x=stimulus_id,  y=sum_sq_diff)) +
  geom_boxplot(aes(colour=stimulus_id), outlier.shape=NA) +
  geom_jitter(height=0, width=0.05, alpha=0.4, size=1.5) +
  theme_bw(base_size = 20) +
  theme(axis.text.x=element_text(angle=90, vjust=0.5), legend.position="none")
ggsave(paste(PLOT.dir, "quality-sum-sq-diff-to-mean.png", sep=SEP), p, width=15, height=10)


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
    target = paste(PLOT.dir, "by-participants-train", fn, sep=SEP)
    if(!dir.exists(target)) {dir.create(target, recursive = TRUE)}
    ggsave(paste(target, paste(pid, "-", fn, ".png", sep=""), sep=SEP), height=8, width=10)
  }
}

plotTrainUncertain("edge")
plotTrainUncertain("ramp")

