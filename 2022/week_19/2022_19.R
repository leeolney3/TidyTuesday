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
    
# Bump chart
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
font_add_google("Source Sans Pro")
f1 = "Source Sans Pro"

df4 = nyt_full %>% 
  mutate(yr=year(week)) %>%
  filter(year==2020) %>%
  group_by(title_id) %>%
  mutate(n=n())
  
selected = df4 %>% filter(rank==1) %>% count(title_id, sort=T) %>% filter(n>1) %>% pull(title_id)

lab = df4 %>% filter(title_id %in% selected) %>% group_by(title_id) %>% filter(week==min(week))

df4 %>%
  ggplot(aes(x=week, y=rank)) +
  geom_point(color="grey", alpha=.8, size=.5) +
  ggbump::geom_bump(aes(group=title_id), alpha=.7, size=.6, color="grey80") +
  ggbump::geom_bump(data= df4 %>% filter(title_id %in% selected), 
                    aes(group=title_id, color=title), 
                    alpha=.8, size=1, show.legend = F) +
  geom_point(data= df4 %>% filter(title_id %in% selected),
             size=.7, shape=21, fill="white", color="black", stroke=.3) +
  geom_text(data=lab %>% filter(title_id!=414 & title_id!=6601 & title_id!=6126), 
            aes(y=0.6,label=str_to_title(title), color=title), 
            size=4, show.legend=F, family=f1, hjust=0, fontface="bold") +
  geom_text(data=lab %>% filter(title_id %in% c(414,6601,6126)), 
            aes(y=0.1,label=str_to_title(title),color=title), 
            size=4, show.legend=F, family=f1, hjust=0, fontface="bold") +
  scale_x_date(expand=c(0.02,0.02), date_breaks = "1 month", date_labels = "%b") +
  scale_y_reverse(breaks=seq(1,15,1), expand=c(0.02,0.02),
                  labels=c("Rank 1","","","4","","","7","","","10","","","13","","")) +
  MetBrewer::scale_color_met_d("Lakota") +
  coord_cartesian(clip="off") +
  theme_minimal(14) +
  theme(text=element_text(family=f1),
        legend.position = "top",
        plot.subtitle = element_text(size=12.5, margin=margin(b=10)),
        plot.title.position = "plot",
        plot.title=element_text(family=f1, face="bold", size=15.5),
        panel.grid=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_line(color="grey50", size=.4),
        axis.ticks.length=unit(.25, "cm"),
        axis.text=element_text(color="black"),
        plot.caption=element_text(hjust=0, color="grey50", size=11),
        plot.margin=margin(.5,.9,.3,.35, unit="cm"),
        plot.caption.position = "plot") +
  labs(title="NY Times Bestsellers 2020",
       subtitle="Titles with rank==1 for two or more weeks in The New York Times fiction bestseller list, from 2020-01-05 to 2020-12-06.",
       caption="\n#TidyTuesday week 19 | Data from Post45 Data by way of Sara Stoudt")
  
# table 2: top 20 titles by Stephen King
sel2 = nyt_full %>% filter(author=="Stephen King") %>% count(title_id,title, sort=T) %>%
  slice(1:20) %>%
  pull(title_id)
  
dfs = nyt_full %>% 
  filter(title_id %in% sel2) %>%
  mutate(rank1=-1*rank) %>%
  select(title, rank,rank1, week) %>%
  group_by(title) %>%
  summarise(total_weeks=n_distinct(week),
            highest_rank=min(rank),
            rank_1=length(rank[rank==1]),
            rank_distribution=list(rank),
            sparkline=list(rank1),
            min_date=min(week),
            max_date=max(week),
            ) %>%
  ungroup() %>%
  arrange(desc(total_weeks)) %>%
  mutate(title=case_when(title!="IT"~str_to_title(title),TRUE~title))
  
gt(dfs) %>%
  gt_theme_nytimes() %>%
  gt_sparkline("rank_distribution", width=35, type="histogram", bw=1) %>%
  gt_sparkline("sparkline", width=35, label=F) %>%
  #gt_fa_repeats("rank_1", name="book", palette="#f6bd60") %>%
  cols_label(total_weeks="Total weeks",
             highest_rank="Highest rank",
             rank_1=md("Rank==1<br>count"),
             rank_distribution="Rank distribution",
             sparkline="Rank over time",
             min_date = "Min. date",
             max_date= "Max. date") %>%
  tab_header(title="NY Times bestsellers, Stephen King's books",
             subtitle="20 titles by Stephen King that appeared the most weeks on The New York Times fiction bestseller list from 1977-03-27 to 2020-09-13, arranged in descending order of total weeks.") %>%
  tab_source_note(source_note = "#TidyTuesday week 19  |  Data from Post45")  
  
# table 3: titles with most rank==1, by year, from 1980 to 2019
y1 = nyt_full %>% 
  filter(rank==1) %>%
  count(year, title, author, title_id, rank) %>%
  group_by(year) %>%
  filter(n==max(n)) 

y2= y1 %>% 
  mutate(title=str_to_title(title)) %>%
  select(year, title, n) %>%
  ungroup() %>%
  filter(between(year,1980,2019))
  
my_gt_function <- function(x) {
  gt(x) %>%
    gt_theme_538() %>%
    gtExtras::gt_color_rows(columns = year, domain = range(y2$year), 
                            palette = "ggsci::grey_material") %>%
    gtExtras::gt_color_rows(columns = n, domain = range(y2$n),
                            palette = "ggsci::indigo_material") %>%
    cols_label(n=md("Rank==1<br>week(s)")) %>%
    opt_table_font(font=list(google_font(name="Roboto"))) 
}

two_tables <- gt_double_table(y2, my_gt_function, nrows = 23)
str(two_tables, max.level = 1)
gtExtras::gt_two_column_layout(two_tables) 
