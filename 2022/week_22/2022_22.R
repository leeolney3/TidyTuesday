# 2022-05-31 
# TidyTuesday Week 22
# 2022 Axios-Harris Poll, data from Axios and Harris Poll

# Libraries
library(tidyverse)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("IBM Plex Serif")
f1 = "IBM Plex Serif"

# Data
poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')

# Category score 
r1 = reputation %>% 
  mutate(name=str_to_title(name),
         name=case_when(name=="P&S"~"Product and\nService", TRUE~name))

r1%>%
  ggplot(aes(fct_rev(name), score, group=company)) +
  geom_line(alpha=.2, size=.3) +
  geom_point(size=.3, alpha=.5) +
  geom_line(data=r1 %>% filter(company %in% c("Facebook","Target","Nike")), aes(color=company), size=.6) +
  geom_point(data=r1 %>% filter(company %in% c("Facebook","Target","Nike")), aes(color=company), size=.6) +
  geom_text(data=r1 %>% filter(name=="Citizenship") %>% filter(company %in% c("Facebook","Target","Nike")), aes(color=company, label=company),vjust=-.8, family=f1, fontface="bold", size=4.2) +
  coord_flip(clip="off") +
  scale_y_continuous(limits=c(50,90), expand=c(0,0)) +
  scale_x_discrete(expand=c(0.03,0.03)) +
  scale_color_manual(values=c("#025590","#ffbd00","#a60900")) +
  cowplot::theme_minimal_grid(14) +
  theme(text=element_text(family=f1),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption=element_text(hjust=0, size=11.2),
        plot.subtitle=element_text(lineheight=1.1, size=11.2),
        axis.ticks.length.y=unit(.25, "cm"),
        axis.title.x=element_text(size=12),
        axis.text.x=element_text(size=11),
        axis.text.y=element_text(face="bold"),
        axis.title.y=element_blank(),
        plot.margin=margin(.4,.7,.4,.4,unit="cm")) +
  labs(caption="TidyTuesday week 21 | Data source: Axios and Harris Poll",
       y="Score", title="2022 Axios-Harris Poll",
       subtitle="Three companies with the largest difference in scores between reputation categories. Based on the\nAxios Harris Poll 100 survey of 33,096 Americans in a nationally representative sample conducted\nMarch 11-April 3, 2022.\n")
       
ggsave("2022_22.png", height=6.5, width=8.5, bg="#fafafa")       
       
# {gt} table
s1 = c("Stellantis","Google","IBM","The Home Depot","Samsung","Sony","Starbucks Corporation","Microsoft","Adidas","General Electric","Spectrum","Pfizer","The Walt Disney Company","PepsiCo","Chick-fil-A","Unilever","Electronic Arts, Inc.","Costco","Citigroup","eBay")

t1 = poll %>% filter(company %in% s1) %>%
  group_by(change,company, industry,`2022_rank`, `2022_rq`) %>%
  summarise(spark_rank=list(rank),
            spark_rq = list(rq))  %>%
  ungroup()

t2 = reputation %>% filter(company %in% s1) %>%
  select(-score, -industry) %>%
  mutate(name=str_to_title(name)) %>%
  pivot_wider(names_from = name, values_from = rank)

t1 %>% left_join(t2, by="company") %>%
  select(-spark_rank) %>%
  filter(!company %in% c("Stellantis","Adidas","Sony","Spectrum","Electronic Arts, Inc.")) %>%
  arrange(desc(change)) %>%
  gt() %>%
  gt_theme_538() %>%
  gt_sparkline(spark_rq) %>%
  gt_fa_rank_change(change) %>%
  cols_label(change="Change in Rank",
             `2022_rank`="2022 Rank",
             `2022_rq`="2022 RQ score",
             spark_rq="2017-2021 RQ score",
             ) %>%
  tab_spanner(label="2022 Reputation Rank",columns=Trust:Culture) %>%
  opt_table_font(font=list(google_font(name="Fira Sans"))) %>%
  tab_options(table.font.size = px(13.5),
              table.font.color = "black") %>%
  tab_source_note(source_note = "#TidyTuesday week 22 | Source: Axios and Harris Poll") %>%
  tab_header(title="2022 Axios-Harris Poll", "Fifteen Companies with the largest rank changes from 2021 to 2022 and RQ score from 2017 to 2022.") %>%
  tab_footnote(footnote="Product and services",location=cells_column_labels(column="P&S")) %>%
  cols_width(Trust:Culture~px(50),
             `2022_rank`:`2022_rq`~px(50))