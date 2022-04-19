# 2022-04-19
# TidyTuesday Week 16 Crossword Puzzles and Clues
# Data from Cryptic Crossword Clues

library(tidyverse)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Roboto Mono")
f1 = "Roboto Mono"

# Data
big_dave <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/big_dave.csv')
times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv')

range(big_dave$puzzle_date)
summary(times$puzzle_date)

# Wrangle 
df1 = big_dave %>%
  filter(!is.na(definition)) %>%
  mutate(definition = str_to_lower(definition)) %>% 
  count(definition, sort=T) %>% 
  mutate(pct=n/sum(n)) %>%
  mutate(g=1,grp="Big Dave's") %>% slice(1:20)

df2 = times %>% 
  filter(!is.na(definition)) %>%
  mutate(definition = str_to_lower(definition)) %>% 
  count(definition, sort=T)  %>% 
  mutate(pct=n/sum(n)) %>%
  mutate(g=2,grp="Times") %>% slice(1:20)
  
df = rbind(df1,df2) %>%
  group_by(grp) %>%
  mutate(rank=rank(desc(n),ties.method = 'first')) %>%
  ungroup()

selected = df %>% select(definition, g) %>% 
  count(definition) %>%
  filter(n==2) %>%
  pull(definition)
  
# Plot
df %>%
  ggplot(aes(x=g, y=rank)) +
  geom_text(aes(label=definition, hjust=ifelse(g==1,1,0)), family=f1) +
  geom_line(data=df %>% filter(definition %in% selected),
            aes(group=definition)) +
  geom_segment(data=df %>% filter(g==2),
               aes(x=g+.8, xend=g+.8+pct*600, y=rank, yend=rank, color=pct), size=5) +
  geom_segment(data=df %>% filter(g==1),
               aes(x=g-.8, xend=g-.8-pct*600, y=rank, yend=rank, color=pct), size=5) +
  scico::scale_color_scico(palette="bamako", direction=-1) +
  ggnewscale::new_scale_color() +
  geom_text(data=df %>% filter(g==2),
            aes(x=g+0.85, y=rank, color=I(ifelse(pct>0.0015,"white","black")), 
                label=scales::percent(pct, accuracy = .01)), 
            size=3, hjust=0) +
  ggnewscale::new_scale_color() +
  geom_text(data=df %>% filter(g==1),
            aes(x=g-0.85, y=rank, color=I(ifelse(pct>0.0014,"white","black")), 
                label=scales::percent(pct, accuracy = .01)), 
            size=3, hjust=1) +
  annotate(geom="text",y=-.3,x=0.68, label="Big Dave's",size=4.3, fontface="bold") +
  annotate(geom="text",y=-.3,x=2.2, label="Times",size=4.3, fontface="bold") +
  scale_y_reverse() +
  scale_x_continuous(limits=c(-1.5,4.5), expand=c(0,0)) +
  cowplot::theme_map(13) +
  theme(legend.position = "none",
        plot.margin=margin(.5,.5,.3,.5, unit="cm"),
        plot.title=element_text(size=15, hjust=.5),
        plot.subtitle = element_text(hjust=.5),
        plot.caption=element_text(size=8.5)) +
  labs(caption="#TidyTuesday week 16  |  Data from Cryptics.georgeho.org",
       title="20 most common crossword puzzle definitions",
       subtitle="from Big Dave's (2009-02-27 to 2022-04-15) and Times (2012-12-27 to 2021-09-12)")
       
ggsave("2022_16.png", height=6, width=8, bg="#fafafa")