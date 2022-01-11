# TidyTuesday week 2, 2022-01-11, 
# Bee Colony losses, Data from USDA and Georgios Karamanis.

library(tidyverse)
library(ggtext)
library(geofacet)
library(ggh4x)

library(showtext)
font_add_google("Lato") 
showtext_auto()
f1 = "Lato"

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

# Conditional area fill and geofacet 
# method from Georgios Karamanis (https://www.r-graph-gallery.com/web-time-series-and-facetting.html)

# Wrangle
s1 = stressor %>%
  filter(state!=c("United States","Other States")) %>% #47 states 
  filter(stressor %in% c("Varroa mites","Other pests/parasites")) %>%
  mutate(stress_pct = stress_pct/100) %>%
  pivot_wider(names_from = "stressor", values_from="stress_pct") %>%
  group_by(state) %>%
  mutate(number= row_number()) %>%
  ungroup()

# Select geofacet states  
my_grid = us_state_grid1 %>% filter(name %in% s1$state)

# Plot
s1 = stressor %>%
  filter(state!=c("United States","Other States")) %>% #47 states 
  filter(stressor %in% c("Varroa mites","Other pests/parasites")) %>%
  mutate(stress_pct = stress_pct/100) %>%
  pivot_wider(names_from = "stressor", values_from="stress_pct") %>%
  group_by(state) %>%
  mutate(number= row_number()) %>%
  ungroup()
```

```{r, warning=F, message=F, fig.width=4.8, fig.height=3.5}
my_grid = us_state_grid1 %>% filter(name %in% s1$state)

s1 %>% janitor::clean_names() %>%
  ggplot(aes(x=number)) +
  #geom_vline(xintercept=13, color="grey90", size=.3) +
  geom_line(aes(y=varroa_mites, color="varroa_mites")) +
  geom_line(aes(y=other_pests_parasites, color="other_pests_parasites")) +
  stat_difference(aes(ymin = other_pests_parasites, ymax = varroa_mites), alpha = 0.3) +
  scale_color_manual(name=NULL, values=c("#6930c3","#ee9b00"), labels=c("Other pest parasites","Varroa mites")) +
  scale_fill_manual(name=NULL,values=c("#ee9b00","#6930c3","#2a9d8f"),labels = c("More varroa mites", "More other pest parasites", "Same")) +
  scale_y_continuous(breaks=c(0,.5,1), limits=c(0,1), labels=scales::percent) +
  scale_x_continuous(breaks=c(1,13,25), labels=c("'15","'18","'21")) +
  coord_cartesian(expand=F, clip="off") +
  facet_geo(~state,grid=my_grid) +
  cowplot::theme_minimal_grid(8.5) +
  theme(text=element_text(family=f1),
        panel.grid.major=element_line(size=.2, color="grey90"),
        legend.position="top",
        legend.justification = "center",
        panel.spacing = unit(.75, "lines"),
        axis.text.x=element_text(color="grey50", size=6, margin=margin(t=2)),
        axis.text.y=element_text(color="grey50", size=6),
        plot.margin=margin(.75,.75,.5,.75, unit="cm"),
        plot.title=element_markdown(hjust=.5, size=12, margin=margin(b=7)),
        plot.caption.position = "plot",
        plot.caption=element_text(margin=margin(t=15), color="grey25", hjust=0, lineheight = 1.4, size=7),
        plot.subtitle=element_text(hjust=.5, size=9.5, margin=margin(b=10))) +
  guides(color=guide_legend(reverse=T, order=1)) +
  labs(x=NULL, y=NULL,
       caption="Note: Percent of colonies affected by stressors anytime during the quarter, colony can be affected by multiple stressors during same quarter. \n#TidyTuesday Week 2  |  Data from USDA  |  Method from Georgios Karamanis",
       title="Percentage of Bee colonies affected by <span style='color:#ee9b00'>Varroa mites</span> vs other pest parasites",
       subtitle = "In 45 States, from 2015Q1 to 2021Q2") 