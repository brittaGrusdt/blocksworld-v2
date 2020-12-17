library(ggplot2)
library(ggallin)

x <- c(-1,-10,-100, 1, 10, 100)
y <- c(1,2,3, 1,2,3)

df = data.frame(x = x, y = y)

My_Plot = ggplot(
  df, 
  aes(x=x, y=y)) + 
  geom_point() + 
  scale_x_continuous(trans = pseudolog10_trans)

My_Plot

breaks <- c(5, 10, 20, 50, Inf)

d <- tibble(x=c(1,2,2,3,10, 12, 15, 100, 120, 500)) %>%
  mutate(y=case_when(x<5 ~ 1,x<10~2, x<20~3, x < 50 ~ 4, TRUE ~5))

d %>% ggplot(aes(x=y)) +
  geom_histogram(aes(y = ..count..),
                 breaks = seq(1, length(breaks))) +
  scale_x_continuous(name="", breaks=seq(1, length(breaks)), labels = breaks)

table(d$y) %>% as.data.frame() %>% ggplot(aes(x=Freq)) +
  geom_histogram(binwidth = 1)



plot_accept_conditions <- function(dat, fn, transform=NA){
  facets <- c(p_delta="△P", p_rooij="△*P")
  breaks <- c(-100000, -1000, -50, -10, -1, seq(0, 1, by=0.01))
  dat %>% mutate(group = case_when(val < -1000 ~ -1000,
                                   val < -50 ~ -50,
                                   val < -10 ~ -10, 
                                   val < -1 ~ -1, 
                                   TRUE ~ 0))
  
  p <- dat %>% ggplot(aes(x=group)) +
    # geom_density(aes(fill=category), alpha=0.4) +
    geom_histogram(aes(y=..count..),
                   closed="right",
                   breaks=c(-100000, -1000, -50, -10, -1, seq(0, 1, by=0.01))) +
    facet_grid(category~condition, scales="free",
               # strip.position = "top",
               labeller = labeller(condition = facets)) +
    scale_x_continuous(trans = pseudolog10_trans) +
    # , labels=trans_format("identity", function(x) -x)
    labs(x="accept/assert condition", y="value", fill="speaker condition") +
    theme_bw(base_size=25) +
    theme(legend.position="bottom", axis.text.x = element_text(angle=90))
  # if(!is.na(transform)){
  #   p <- p + scale_x_continuous(trans=transform)
  #     # + scale_y_continuous(trans=transform)
  # }
  ggsave(paste(PLOT_DIR, fn, sep=.Platform$file.sep), p, width=16, height=8)
  return(p)
}
p2 <- plot_accept_conditions(df %>% filter(condition == "p_rooij" & val != 0),
                             "accept-conditions.png", "pseudo_log")



# reverselog_trans <- function(base = exp(1)) {
#   trans <- function(x) -log(x, base)
#   inv <- function(x) base^(-x)
#   trans_new(paste0("reverselog-", format(base)), trans, inv, 
#             log_breaks(base = base))
# }