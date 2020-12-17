library(here)
source(here("R", "joint_experiment", "analysis-utils.R"))

# check whether there are participants who chose a particular answer nearly always
df.production.means = data.production %>% filter(id != "ind2") %>%
  select(response, prolific_id, id) %>% 
  group_by(prolific_id,response) %>% 
  mutate(n=n()) %>% group_by(prolific_id) %>% mutate(N=n(), ratio=n/N) %>%
  arrange(desc(ratio)) %>% distinct() %>%
  mutate(response=as.factor(response))

# check time spent
df = left_join(df.production.means, data.info %>% select(prolific_id, timeSpent), 
               by=c("prolific_id")) %>% 
  mutate(timeSpent=round(timeSpent, 2))

p  = df %>%
  select(prolific_id, response, n, timeSpent) %>% distinct() %>% 
  ggplot() +
  geom_bar(aes(y=response, x=n), stat="identity") +
  geom_text(aes(x=9, y="green might not fall", label=timeSpent), color="red") + 
  theme_bw(base_size=18) +
  facet_wrap(~prolific_id) + 
  theme(legend.position = "none")
  
ggsave(paste(PLOT.dir, "utterance-frequencies.png", sep=SEP),
       p, width=12, height=15)

# color vision data

pids.color <- data.color %>% group_by(prolific_id, id) %>%
  filter(expected != response) %>%
  pull(prolific_id)

df %>% filter(prolific_id %in% pids.color)




