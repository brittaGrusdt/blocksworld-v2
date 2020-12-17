source("R/helper-functions.R")
library(tidyverse)
library(ggplot2)
TARGET_DIR <- file.path("data", "default-model", "figs", fsep=.Platform$file.sep)


# Acceptability conditions for listener data  -----------------------------
data_long <- read_rds(file.path("data", "default-model", "results-none.rds")) %>%
data_wide <- data_long %>% spread(key=cell, val=val)
  
data <- data_wide %>% gather(p_delta, p_rooij, key=accept_cond, val=val) %>% 
          mutate(level=factor(level, levels = c("prior", "LL", "PL"))) %>% 
          group_by(level, accept_cond) %>% arrange(desc(val))

x_breaks <- seq(from=0.85, to=0.95, by=0.05)
x_breaks_factor <- factor(x_breaks)
cdf <-list(); i <- 1
for(lim in x_breaks){
  prob <- data %>% filter(val>lim) %>% 
    summarize(prob_greater_x=sum(prob)) %>% add_column(x=lim)
  cdf[[i]] <- prob
  i <- i + 1
}
cdf <- bind_rows(cdf) %>% mutate(x=factor(x),
                                 prob_greater_x=round(prob_greater_x, 2))

p <- cdf  %>% 
  ggplot(aes(x=x, y=prob_greater_x, fill=level)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=prob_greater_x, x=x,  y=prob_greater_x), size=5, vjust=-0.1,
            position=position_dodge(0.9)) +
  facet_wrap(~accept_cond, labeller = labeller(
    accept_cond = c(p_delta = "X:= △P",
                    p_rooij = "X:= △*P"))
  ) +
  labs(y="P(X >= x)") +   
  scale_fill_discrete(name="",
                      breaks=c("prior", "LL", "PL"),
                      labels=c("Prior Belief", "Literal interpretation", "Pragmatic interpretation"),
                      position="bottom") + 
  theme(text = element_text(size= 20),
        axis.text.x = element_text(size=20, angle=0),
        legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size=18)
  ) 
p
ggsave(paste(TARGET_DIR, "assertability-conditions.png", sep=.Platform$file.sep), p, width=12, height=5)





# Acceptability conditions for speaker data -------------------------------
speaker_long <- read_rds(file.path("data", "default-model", "results-none-speaker.rds"))
sp_ifac <- speaker_long %>% filter(utterance=="A > C") %>% arrange(desc(probs))
sp_ifac_long <- sp_ifac %>% gather(p_delta, p_rooij, key=accept_cond, val=val) 
#   mutate(level=factor(level, levels = c("prior", "LL", "PL"))) %>% 
#   group_by(level, accept_cond) %>% arrange(desc(val))

p <- sp_ifac_long  %>% filter(val>0 & probs>0.2) %>% 
      ggplot(aes(x=probs, y=val, color=accept_cond)) +
      geom_point() +
      labs(y="Value acceptability conditions", 
           x="Speaker's probability to choose 'If A, C'") +   
  theme(text = element_text(size= 20),
        axis.text.x = element_text(size=20, angle=0),
        legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size=18)
  ) 
p
ggsave(paste(TARGET_DIR, "assertability-conditions.png", sep=.Platform$file.sep), p, width=12, height=5)










  
# samples <- sample_webppl_distr(df %>% rename(p=p_delta))
# ggplot(samples) + geom_density(aes(x=support, col=level)) + labs(y="p_delta") +facet_wrap(~level)

# samples <- sample_webppl_distr(df %>% rename(p=p_rooij))
# ggplot(samples) + geom_density(aes(x=support, col=level)) + labs(y="rel_difference") +facet_wrap(~level)

thresholds <- c(0.5, 0.6, 0.7, 0.8, 0.9)
results <- list()
idx <- 1
for(t in thresholds){
  p_delta <- df %>% filter(p_delta<t) %>% group_by(level) %>% summarize(p_delta=sum(prob))
  relative_diff <- df %>% filter(p_rooij<t) %>% group_by(level) %>% summarize(rel_diff=sum(prob))  
  results[[idx]] <- bind_cols(p_delta, relative_diff) %>% add_column(threshold=t) %>% select(-level1)
  idx <- idx + 1
}
results <- bind_rows(results) %>% gather(p_delta, rel_diff, key="measure", val="value")
results %>% filter(measure=="p_delta") %>%  ggplot() + geom_point(aes(x=threshold, y=value, color=level)) + labs(y="p_delta")
results %>% filter(measure=="rel_diff") %>%  ggplot() + geom_point(aes(x=threshold, y=value, color=level)) + labs(y="rel_diff")

samples <- sample_webppl_distr(df %>% rename(p=p_rooij))
samples %>% ggplot() + geom_density(aes(x=support))

df %>% filter(p<0.75) %>% group_by(level) %>% summarize(s=sum(prob))
# is the same as:
# samples %>% group_by(level) %>% summarize(s=sum(support>0.75)/n())



# acceptance conditions ---------------------------------------------------
# base_plot <- function(data, title_str){
#   p0_base <- data %>% ggplot(aes(x=val)) + geom_density() +
#     labs(title=title_str) + 
#     theme_classic(base_size = 20) +
#     scale_color_gradient(low="blue", high="red") +
#     facet_wrap(~cn, scales="free") +
#     theme(legend.position="bottom", legend.text = element_text(size=8),
#           legend.title=element_text(size=10), 
#           legend.key.width = unit("2.5", "cm")
#     )
#   
#   return(p0_base)
# }

# df_cond_neg <- df_long %>% filter(val<0 & val>-100 & condition=="p_rooij")
# p0_base <- df_cond_neg_long %>%  base_plot("")
# p0 <- p0_base + 
#   geom_point(aes(x=val, y=p_c_given_a, color=p_c_given_a), size=0.5) +
#         facet_grid(cn~condition, scales="free",
#                    labeller=labeller(condition=c(`p_delta`="△P", 
#                                                  `p_rooij`="△*P"))) +
#         labs(color="P(C|A)", x="")
# p0
# ggsave(paste(TARGET_DIR, "accept_conditions_small.png",
#              sep=.Platform$file.sep), p0, width=15, height=12)
# 

# zoom in high density areas
# p0_zoom <-p0 + scale_x_continuous(limits=c(0.5, max(df_not_assertable$val)))
# p0_zoom

# p0_indep <-df_not_assertable %>% filter(cn=="A || C") %>%
#             base_plot(TeX("$P_S(u=A\\rightarrow C)=0$")) +
#             scale_x_continuous(limits=c(0, 0.25))  +
#             geom_point(aes(x=val, y=p_c_given_a, color=p_c_given_a)) +
#             facet_wrap(~condition) + 
#             labs(color="P(C|A)", x="value acceptance condition")
# p0_indep
# ggsave(paste(TARGET_DIR, "accept_conditions_ifac_not_assertable_cn_independent.png",
#              sep=.Platform$file.sep), p0_indep, width=10, height=5)


# # 3. "A>C" is assertable (P_S(u=A>C)>0)
# df_assertable <- df %>% filter(probs>0 & val < 0)
# p1_base <- df_assertable %>% base_plot(TeX("$P_S(u=A\\rightarrow C)>0$")) +
#             facet_grid(cn~condition, scales="free", labeller = 
#                          labeller(condition=c(p_delta="△P", p_rooij="△*P")))
# p1 <- p1_base + geom_point(aes(x=val, y=p_c_given_a, color=`A > C`)) +
#         labs(color=TeX("$P(C|A)$")) +
#         scale_color_gradient(low="yellow", high="red")
#   
# p1
# ggsave(paste(TARGET_DIR, "accept_conditions_ifac_assertable.png",
#              sep=.Platform$file.sep), p1, width=10, height=12)
# 
# # zoom in high density area
# p1_zoom <-p1 + scale_x_continuous(limits=c(0.825, max(df_assertable$val)))
# p1_zoom
# 
# 
# df_assertable %>% spread(key=condition, val=val) %>%
#   filter(p_delta < 0.25 & p_rooij < 0.25) %>%
#   ggplot() +
#   geom_point(aes(x=val, y=p_c_given_a, color=p_c_given_a)) +


