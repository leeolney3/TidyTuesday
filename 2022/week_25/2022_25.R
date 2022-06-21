# 2022-06-21
# TidyTuesday week 25 American Slavery and Juneteenth
# Data source: US Census's Archives
# Plot colors from: WEB DuBois style by Anthony Starks

# Libraries
library(tidyverse)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Fonts
font_add_google("Fira Mono")
font_add_google("Fira Sans")
f1 ="Fira Sans"
f2 ="Fira Mono"

# Import data
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')

# p1: Percentage of free African Americans by year and region
c1 = census %>%
  filter(region!="USA Total", is.na(division)) %>%
  mutate(pct_free=black_free/black,
         region=glue::glue("{region}\nregion")) %>%
  mutate(col=case_when(pct_free==1~"#FE0101",
                       between(pct_free,.9,1)~"#FFBFCA",
                       TRUE~"black"))

c1 %>%
  ggplot(aes(x=0, y=0)) +
  geom_point(aes(size=pct_free, color=col),show.legend = FALSE) +
  scale_size_area(max_size=20) +
  scale_color_identity() +
  geom_text(data= c1 %>% filter(between(pct_free,.5,.9)), aes(label=scales::percent(pct_free, accuracy=0.01)), color="white", size=3, family=f1) +
  geom_text(data= c1 %>% filter(between(pct_free,.9,.9999)), aes(label=scales::percent(pct_free, accuracy=0.01)), color="black", size=3.5, family=f1) +
  geom_text(data= c1 %>% filter(pct_free==1), aes(label=scales::percent(pct_free, accuracy=1)), color="white", size=3.7, family=f1) +
  geom_text(data= c1 %>% filter(between(pct_free,.3,.5)), aes(label=scales::percent(pct_free, accuracy=0.01)), color="white", size=2.5, family=f1) +
  geom_text(data= c1 %>% filter(pct_free<.3), aes(label=scales::percent(pct_free, accuracy=0.01)), color="black", size=3, vjust=-1.5, hjust=0.4) +
  facet_grid(rows=vars(year), cols=vars(region)) +
  cowplot::theme_map(13) +
  theme(text=element_text(family=f2),
        axis.title = element_blank(),
        axis.text=element_blank(),
        strip.text.y=element_text(angle=0),
        panel.spacing = unit(0,"lines"),
        plot.caption.position = "plot",
        plot.caption=element_text(family=f1, color="grey30", size=9, hjust=0, margin=margin(t=20, r=100)),
        plot.title.position = "plot",
        plot.title = element_text(hjust=.5, size=13.5),
        plot.subtitle = element_text(hjust=.5, size=11.5, family=f1, margin=margin(b=17)),
        plot.margin=margin(.5,1.2,.5,.5, unit="cm") 
        ) +
  labs(caption="Note: The racial categories used in the decennial census have reflected social usage rather than an attempt to define\nrace biologically or genetically.From 1790 to 1850, the only categories recorded were White and Black (Negro), with\nBlack designatedas free and slave.\n\nTidyTuesday week 25  •  Source: US Census's Archives  •  Plot colors from WEB DuBois style by Anthony Starks",
       title=str_to_title("HISTORICAL CENSUS STATISTICS ON POPULATION BY REGION"),
       subtitle="Percentage of free African Americans in the U.S. by year, from 1790 to 1870")
       
ggsave("2022_25.png", height=8, width=7.5, bg="#fafafa")