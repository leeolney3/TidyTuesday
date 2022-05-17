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
  group_by(artist_country) %>%
  summarise(years_participated = n_distinct(year),
            highest_rank=min(rank, na.rm = T),
            lowest_rank=max(rank, na.rm=T),
            highest_point = max(total_points, na.rm=T),
            lowest_point=min(total_points, na.rm=T))

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
  labs(title="Eurovision Grand Finale Rankings",
       subtitle="From 2004 to 2022, arranged in alphabetical order of artist country",
       caption="#TidyTuesday week 20 | Data from Eurovision, credits to Tanya Shapiro and Bob Rudis")
       
ggsave("2022_20.png",height=8, width=7, bg="white")
