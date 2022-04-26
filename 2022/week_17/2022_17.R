# 2022-04-26
# TidyTuesday week 17 Kaggle Hidden Gems
# data from Kaggle by way of Martin Henze (Heads or Tails)

# Libraries 
library(tidyverse)
library(tidytext)
library(SnowballC)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("IBM Plex Serif")
f1="IBM Plex Serif"

# Data
hidden_gems <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-26/hidden_gems.csv')

# title
hidden_gems %>% distinct(vol,title) %>%
  unnest_tokens(word, title) %>%
  anti_join(get_stopwords()) %>% 
  count(word, sort=T)

# review
r1 = hidden_gems %>% 
  distinct(vol,review) %>%
  mutate(review = gsub(r"{\s*\([^\)]+\)}","",as.character(review)),
         review = gsub("\\[|\\]", "", review)) %>%
  unnest_tokens(word, review) %>%
  anti_join(get_stopwords())
  
r1 %>% 
  mutate(stem = wordStem(word)) %>% 
  count(stem, sort=T)

# Wrangle
df = hidden_gems %>%
  group_by(vol) %>%
  mutate(id = row_number()) %>%
  mutate(group = case_when(str_detect(review, "visu") | str_detect(review, "viz") ~ "1", TRUE~"0")) %>%
  select(vol, date, title, author=author_name, review, group) %>%
  mutate(vol_date= paste(vol,"-",date)) 
  
df2 = df %>%
  group_by(vol) %>%
  mutate(wgrp = row_number()) %>%
  mutate(grp = cut(vol, breaks=c(0,20,40,60,80,100), ordered=T,
                   labels=c("Vol 1-20","Vol 21-40","Vol 41-60",
                            "Vol 61-80","Vol 81-100"))) %>%
  ungroup()
  
# Plot
df2 %>%
  ggplot(aes(x=wgrp, y=fct_rev(factor(vol)), fill=group)) +
  geom_tile(height=.8, width=.8, color="black") +
  scale_fill_manual(values=c("transparent","#00786B")) +
  facet_wrap(~grp, scales = "free", ncol=5) +
  theme_minimal(14) +
  theme(legend.position = "none",
        text=element_text(family=f1),
        panel.grid=element_blank(),
        panel.spacing = unit(2, "lines"),
        axis.text.y = element_text(color="black"),
        axis.text.x=element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(face="bold", hjust=.5),
        plot.subtitle = element_text(hjust=.5, color="#005F52", size=12.5),
        plot.caption=element_text(hjust=0, size=9.5, lineheight = 1.1),
        plot.margin=margin(.5,.75,.5,.5, unit="cm"),
        axis.title=element_text(size=11),
        strip.text = element_text(size=10.5, face="bold"),
        ) +
  labs(y="Volume",x="Notebook",
       title="Kaggle Hidden Gems",
       subtitle="Notebook reviews with mention of visuals",
       caption="\nNotebook in the order of appearance in dataset.\n#TidyTuesday week 17  |  Data from Kaggle by way of Martin Henze (Heads or Tails)")
       
ggsave("2022_17.png", height=7.5, width=7.5, bg="#fafafa")