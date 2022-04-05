# 2022-04-05
# TidyTuesday Week 14 Digital Publications
# Data source: Project Oasis by way of Data is Plural

# Libaries
library(tidyverse)
library(ggtext)
library(usmap)
library(showtext)

# Font
font_add_google("Fira Sans Condensed")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Data
news_orgs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')

# Summary
skimr::skim(news_orgs)

news_orgs %>% summarise_all(n_distinct) %>%
  pivot_longer(everything())
  
# Wrangle data
df = news_orgs %>%
  filter(country!="Canada") %>%
  mutate(region = case_when(state %in% .northeast_region ~"Northeast region",
                            state %in% .midwest_region ~"Midwest region",
                            state %in% .south_region  ~"South region",
                            state %in% .west_region ~"West region"
                            )) %>%
  mutate(tax_status_current=case_when(tax_status_current=="Nonprofit 501c(3) or Canadian nonprofit"~"Nonprofit 501c(3)", TRUE~tax_status_current)) %>%
  filter(!is.na(region), !is.na(tax_status_current))

levels = df %>% count(tax_status_current) %>% arrange(n) %>% pull(tax_status_current)

df1 = df %>% count(tax_status_current, region) 

# Plot
df1 %>% 
  ggplot(aes(n, factor(tax_status_current, levels=levels))) +
  geom_segment(aes(x=0, xend=n, y=factor(tax_status_current, levels=levels), yend=factor(tax_status_current, levels=levels)), size=1, color="white") +
  geom_point(size=3, color="white") +
  geom_text(aes(x=n+8, label=n), size=4, hjust=0, family="Fira Sans Condensed", color="white") +
  facet_wrap(~region, ncol=4) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 19)) +
  scale_x_continuous(breaks=c(0), expand = expansion(mult = c(0, .2))) +
  cowplot::theme_minimal_grid(13) +
  coord_cartesian(clip="off") +
  theme(text=element_text(family="Fira Sans Condensed", color="white"),
        axis.title = element_blank(),
        plot.title.position="plot",
        axis.text.x=element_blank(),
        axis.text.y=element_text(color="white"),
        panel.grid.major.y=element_line(linetype = "dotted", color="#8aabc3"),
        panel.grid.major.x=element_line(color="#8aabc3"),
        plot.title=element_text(size=18),
        plot.subtitle = element_text(margin=margin(t=1,b=10)),
        strip.text=element_text(size=13, face="bold"),
        plot.margin=margin(.5,.5,.3,.5, unit="cm"),
        plot.caption=element_text(size=9.2),
        axis.ticks=element_blank(),
        legend.position = "none",
        plot.background=element_rect(fill="#4B83A6", color=NA)) +
  labs(title="Digital News Publications",
       subtitle="Number of publications by U.S. regions and current tax status, publications founded between 1958 and 2021.",
       caption="TidyTuesday week 14 | Data from Project Oasis by way of Data is Plural")
       
ggsave("2022_14.png", height=6, width=8)