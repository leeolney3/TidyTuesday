# 2022-05-17
# TidyTuesday week 20, Data from Eurovision, credits to Tanya Shapiro and Bob Rudis

library(tidyverse)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

font_add_google("Karla")
f1 = "Karla"

# Data
eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')

# Summary
eurovision %>% 
  filter(year!=2020, section %in% c("final", "grand-final")) %>%
  group_by(year) %>%
  mutate(last= case_when(rank==max(rank)~1, TRUE~0),
         n_host = case_when(host_country==artist_country~1, TRUE~0)) %>%
  ungroup() %>%
  group_by(country_emoji, artist_country) %>%
  summarise(n=n(),
            n_winner=length(n[winner=="TRUE"]),
            #win_year=list(year[winner=="TRUE"]),
            n_last = sum(last),
            highest_rank=min(rank),
            min_year=min(year),
            max_year=max(year),
            n_host= sum(n_host)) %>%
  ungroup()

# Wrangle
gf = eurovision %>% 
  filter(section=="grand-final", year!=2020) %>%
  group_by(artist_country) %>%
  mutate(n=n_distinct(year))
  
gf3 = gf %>% filter(rank %in% c(1,2,3))
gfl = gf %>% group_by(year) %>% slice_max(rank,n=1)  

# Plot
gf %>%
  ggplot(aes(x=year, y=	fct_rev(artist_country))) +
  geom_line(aes(group=fct_rev(artist_country)), size=.3, color="grey70") +
  geom_text(data=gf %>% select(artist_country, n) %>% distinct(),
            aes(x=2022.5, y=fct_rev(artist_country), label=glue::glue("n= {n}")), 
            family=f1, color="grey50", size=3.5, hjust=0) +
  geom_point(shape=21, size=2.5, fill="white") +
  geom_point(data=gf3, aes(fill=rank_ordinal), size=2.5, shape=21) +
  scale_fill_manual(values=c("#F50405","#F7C83A","#1CB4EB"), 
                    guide = guide_legend(order = 1)) +
  ggnewscale::new_scale_fill() +
  geom_point(data=gfl, aes(fill="Last"), size=2.5, shape=21) +
  scale_fill_manual(values=c("grey50")) +
  scale_x_continuous(position="top", breaks=seq(2005,2020,5),
                     expand = expansion(mult = c(.02, NA))) +
  coord_cartesian(clip = "off") +
  cowplot::theme_minimal_vgrid(12) +
  theme(text=element_text(family=f1),
        legend.title = element_blank(),
        axis.title=element_blank(),
        axis.line=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "top",
        plot.margin=margin(.4,1.4,.3,.4, unit="cm"),
        plot.title.position = "plot",
        plot.title=element_text(size=13),
        plot.caption.position = "plot",
        plot.caption=element_text(hjust=0, color="grey20", margin=margin(t=13), size=9)
        ) +
  labs(title="Eurovision Grand Final Rankings",
       subtitle="From 2004 to 2022, arranged in alphabetical order of artist country",
       caption="#TidyTuesday week 20 | Data from Eurovision, credits to Tanya Shapiro and Bob Rudis")
       
ggsave("2022_20.png",height=8, width=7, bg="white")

# v2
# h/t to Priyanka Mehta @Priyank79286307 for the suggestion to order country by who participated most and who won most
lev = gf %>% 
  group_by(artist_country) %>%
  summarise(highest_rank=min(rank),
            n=n_distinct(year)) %>%
  ungroup() %>%
  arrange(n, desc(highest_rank)) %>%
  pull(artist_country)
  
gf %>%
  mutate(artist_country=factor(artist_country, levels=lev)) %>%
  ggplot(aes(x=year, y=	artist_country)) +
  geom_line(aes(group=artist_country), size=.3, color="grey70") +
  geom_text(data=gf %>% select(artist_country, n) %>% distinct(),
            aes(x=2022.5, y=artist_country, label=glue::glue("n= {n}")), 
            family=f1, color="grey50", size=3.5, hjust=0) +
  geom_point(shape=21, size=2.5, fill="white") +
  geom_point(data=gf3, aes(fill=rank_ordinal), size=2.5, shape=21) +
  scale_fill_manual(values=c("#F50405","#F7C83A","#1CB4EB"), 
                    guide = guide_legend(order = 1)) +
  ggnewscale::new_scale_fill() +
  geom_point(data=gfl, aes(fill="Last"), size=2.5, shape=21) +
  scale_fill_manual(values=c("grey50")) +
  scale_x_continuous(position="top", breaks=seq(2005,2020,5),
                     expand = expansion(mult = c(.02, NA))) +
  coord_cartesian(clip = "off") +
  cowplot::theme_minimal_vgrid(12) +
  theme(text=element_text(family=f1),
        legend.title = element_blank(),
        axis.title=element_blank(),
        axis.line=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "top",
        plot.margin=margin(.4,1.4,.3,.4, unit="cm"),
        plot.title.position = "plot",
        plot.title=element_text(size=13),
        plot.caption.position = "plot",
        plot.caption=element_text(hjust=0, color="grey20", margin=margin(t=13), size=9)
        ) +
  labs(title="Eurovision Grand Final Rankings",
       subtitle="From 2004 to 2022, artist countries arranged in descending order of total years in grand final\nand highest rank achieved.",
       caption="#TidyTuesday week 20 | Data from Eurovision, credits to Tanya Shapiro and Bob Rudis") 
       
ggsave("2022_20v2.png",height=8, width=7, bg="white") 

# eurovision-votes.csv
# max points network graph for year 2021 and 2022
# reference: https://bjnnowak.netlify.app/2021/09/30/r-network-analysis-with-tidygraph/

library(tidygraph)
library(ggraph)
library(patchwork)

votes = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision-votes.csv') 

custom_theme <- theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    plot.title=element_text(hjust=.5)
  )
  
# y2021
edges_list = votes %>% 
  filter(semi_final=="f", jury_or_televoting=="J", is.na(duplicate)) %>%
  filter(points==12, year==2021) %>%
  mutate(from = countrycode(from_country, origin="country.name",destination = "iso3n"),
         to = countrycode(to_country, origin="country.name",destination = "iso3n")) %>%
  select(from_country, to_country, points)

network = as_tbl_graph(edges_list, directed = TRUE)

p21 = network %>%
  ggraph(layout="kk") +
  geom_node_point() +
  geom_edge_diagonal(color="dimgrey", 
                      arrow=arrow(angle=40, length=unit(0.1, "cm"))) +
  geom_node_text(aes(label=name), size=2, color="blue",repel=T) + 
  custom_theme +
  theme(plot.background = element_rect(fill="#e9ecef", color=NA)) +
  labs(title = '2021')

# y2022
edges_list = votes %>% 
  filter(semi_final=="f", jury_or_televoting=="J", is.na(duplicate)) %>%
  filter(points==12, year==2022) %>%
  mutate(from = countrycode(from_country, origin="country.name",destination = "iso3n"),
         to = countrycode(to_country, origin="country.name",destination = "iso3n")) %>%
  select(from_country, to_country, points)

network = as_tbl_graph(edges_list, directed = TRUE)

p22 = network %>%
  ggraph(layout="kk") +
  geom_node_point() +
  geom_edge_diagonal(color="dimgrey", 
                      arrow=arrow(angle=40, length=unit(0.1, "cm"))) +
  geom_node_text(aes(label=name), size=2, color="blue",repel=T) + 
  custom_theme +
  theme(plot.background = element_rect(fill="#f8f9fa", color=NA)) +
  labs(title = '2022')
  
# combine plot and save
p21 + p22
ggsave("network.png", height=4, width=8)  
