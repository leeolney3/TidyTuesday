# 2022-05-10
# TidyTuesday week 19, NYTimes best sellers
# Data from Post45 Data

# Libraries
library(tidyverse)
library(gt)
library(gtExtras)

# Data
nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

# Prepare table data
df1 = nyt_titles %>% slice_max(total_weeks, n=20) %>%
  mutate(title=str_to_title(title)) 

df2 = nyt_full %>% filter(title_id %in% df1$id) %>%
  mutate(rank=-1*rank) %>%
  group_by(id=title_id) %>%
  arrange(week) %>%
  summarise(timeline=list(rank),.groups="drop")

df3 = df1 %>% left_join(df2, by="id")

# {gt} table
df3 %>%
  select(-id) %>%
  mutate(wk = total_weeks) %>%
  relocate(wk,.after=total_weeks) %>%
  gt() %>%
  gt_theme_nytimes() %>%
  gt_sparkline(timeline, label=F, range_colors = c("#7014f2", "#06b178")) %>%
  gt_plt_bar_pct(wk, fill="#ffc300") %>%
  cols_width(wk~px(100),
             first_week~px(100)) %>%
  cols_align(align="right", columns=first_week) %>%
  cols_label(total_weeks="total weeks",
             first_week="first week",
             debut_rank="debut rank",
             best_rank="best rank",
             wk="") %>%
  tab_header(title="NY Times bestsellers",
             subtitle=md("List of titles with more than 80 total weeks on the fiction bestseller list of ***The New York Times*** between the years of 1931 and 2020, arranged in descending order of total weeks.")) %>%
  tab_source_note("#TidyTuesday week 19 | Data from Post45 Data by way of Sara Stoudt") %>%
  tab_style(style = list(cell_text(style = "italic", color="black")),
    locations = cells_body(columns = title))