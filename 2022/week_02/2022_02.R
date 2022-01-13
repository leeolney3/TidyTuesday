# TidyTuesday week 2, 2022-01-11
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



# Section 1: stressor.csv -------------------------------------------------

# Percent of Bee colonies affected by varroa mites vs other pest parasites (shared on Twitter)
# Conditional area fill and geofacet method from Georgios Karamanis and R Graph Gallery (https://www.r-graph-gallery.com/web-time-series-and-facetting.html)

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

my_grid = us_state_grid1 %>% filter(name %in% s1$state)

geo_theme = cowplot::theme_minimal_grid(8.5) +
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
        plot.subtitle=element_text(hjust=.5, size=9.5, margin=margin(b=10)))

p1 = s1 %>% janitor::clean_names() %>%
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
  geo_theme +
  guides(color=guide_legend(reverse=T, order=1)) +
  labs(x=NULL, y=NULL,
       caption="Note: Percent of colonies affected by stressors anytime during the quarter, colony can be affected by multiple stressors during same quarter. \n#TidyTuesday Week 2  |  Data from USDA  |  Method from Georgios Karamanis",
       title="Percentage of Bee colonies affected by <span style='color:#ee9b00'>Varroa mites</span> vs other pest parasites",
       subtitle = "In 45 States, from 2015Q1 to 2021Q2")
p1

# Section 2: colony.csv ---------------------------------------------------

# Facetted line plot, Changes in colonies 
c1 = colony %>% 
  filter(state =="United States") %>%
  select(-colony_max) %>%
  mutate(number = row_number()) %>%
  pivot_longer(!c(1:3,number)) %>%
  mutate(quarter = case_when(months=="January-March"~"1",
                             months=="April-June" ~ "2",
                             months=="July-September" ~"3",
                             months=="October-December"~"4")) %>%
  mutate(name = case_when(name=="colony_n"~"Number of colonies",
                          name=="colony_lost"~"Colonies lost",
                          name=="colony_lost_pct"~"Percent of total colonies lost",
                          name=="colony_added"~"Colonies added",
                          name=="colony_reno"~"Colonies renovated",
                          name=="colony_reno_pct"~"Percent of colonies renovated")) %>%
  mutate(name=factor(name, levels = c("Number of colonies",
                                      "Colonies lost",
                                      "Percent of total colonies lost",
                                      "Colonies added",
                                      "Colonies renovated",
                                      "Percent of colonies renovated"
  )))

highlight = c1 %>% select(year, number,quarter, name, value) %>% distinct() %>%
  group_by(year, name) %>%
  summarise(xmin=min(number),
            xmax=max(number),
            ymin=min(value),
            ymax=max(value)) %>%
  filter(year!="6/")

mytheme1 = cowplot::theme_minimal_grid(10) +
  theme(text=element_text(family=f1),
        panel.spacing = unit(.75, "lines"),
        axis.text=element_text(color="grey50",size=7),
        legend.position = "top",
        legend.justification = "center",
        plot.title=element_text(hjust=.5),
        plot.margin=margin(.75,1.25,.75,.75, unit="cm")) 

p2a = c1 %>%
  ggplot(aes(number, value)) +
  geom_rect(data=highlight, inherit.aes=FALSE, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="transparent", fill="grey", alpha=.2) +
  geom_line() +
  geom_point(aes(color=quarter)) +
  facet_wrap(~name, scales="free") +
  scale_x_continuous(breaks=c(1,13,25), labels=c("'15","'18","'21")) +
  scale_y_continuous(labels=scales::comma) +
  scale_color_manual(values=MetBrewer::met.brewer("Egypt"), na.translate = FALSE) +
  mytheme1 +
  labs(x="Year", y="Value", color="Quarter", title="Bee Colonies in United States")

# Facetted line plot, Seasonality 
p2b = c1 %>%
  ggplot(aes(number, value)) +
  geom_rect(data=highlight, inherit.aes=FALSE, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="transparent", fill="grey", alpha=.2) +
  geom_line(aes(color=quarter)) +
  geom_point(aes(color=quarter)) +
  facet_wrap(~name, scales="free") +
  scale_x_continuous(breaks=c(1,13,25), labels=c("'15","'18","'21")) +
  scale_y_continuous(labels=scales::comma) +
  scale_color_manual(values=MetBrewer::met.brewer("Egypt"), na.translate = FALSE) +
  cowplot::theme_minimal_grid(9) +
  mytheme1 +
  labs(x="Year", y="Value", color="Quarter", title="Bee Colonies in United States")

# Line and area plot
# Number of Colonies, Colonies Lost, Colonies Added, % total colonies lost, % total colonies renovated

mytheme2 = 
  cowplot::theme_minimal_grid(9) +
  theme(legend.position = "top",
        legend.box = "vertical",
        legend.justification = "center",
        legend.spacing.y = unit(.3, 'lines'),
        plot.title=element_text(hjust=.5, margin=margin(b=-4)),
        plot.background=element_rect(fill="#e9ecef", color=NA),
        plot.margin = margin(.5,.5,.3,.3, unit="cm"))

c2 = colony %>% select(year, months, state, colony_added, colony_lost, colony_n) %>%
  filter(state=="United States") %>%
  mutate(number = row_number()) %>%
  pivot_longer(colony_added:colony_n) %>%
  mutate(name = fct_rev(name))

p3a = c2 %>%
  ggplot(aes(number, value)) +
  geom_area(data = c2 %>% filter(name=="colony_n", number>18), aes(fill="Number of Colonies"), alpha=.7) +
  geom_area(data = c2 %>% filter(name=="colony_n", number<18), aes(fill="Number of Colonies"), alpha=.7) +
  scale_fill_manual(name=NULL,values=c("#2E3642")) +
  geom_point(data = c2 %>% filter(name!="colony_n"), aes(color=name), size=1.5)+
  geom_line(data = c2 %>% filter(name!="colony_n"), aes(color=name)) +
  scale_y_continuous("Value",limits=c(0,NA), labels = scales::unit_format(unit = "M", scale = 1e-6, accuracy = 1), expand=c(.005,.005)) +
  scale_x_continuous("Year",limits=c(1,26), breaks=seq(1,26,4), labels=seq(2015, 2021,1),expand=c(.01,.01)) +
  scale_color_manual(name=NULL, values=c("#ffb703","#01dca6"), labels=c("Colonies Lost","Colonies Added"))

p3b = colony %>% 
  filter(state=="United States") %>%
  mutate(number = row_number(),
         colony_reno_pct = colony_reno_pct/100,
         colony_lost_pct = colony_lost_pct/100) %>%
  ggplot(aes(x=number)) +
  geom_line(aes(y=colony_reno_pct, color="colony_reno_pct"), size=.8) +
  geom_line(aes(y=colony_lost_pct, color="colony_lost_pct"), size=.8) +
  stat_difference(aes(ymin = colony_reno_pct, ymax = colony_lost_pct), alpha = 0.3) +
  scale_color_manual(name=NULL, values=c("#fc7a1e","#009E73"), 
                     labels=c("% total colonies lost","% total colonies renovated")) +
  scale_fill_manual(name=NULL, values=c("#fc7a1e","#009E73"), 
                    labels=c("Loss > renovated (%)", "Renovated > loss (%)")) +
  scale_x_continuous("Year",limits=c(1,26), breaks=seq(1,26,4), labels=seq(2015, 2021,1),expand=c(.01,.01)) +
  scale_y_continuous("Percentage", labels=scales::percent, limits=c(0,NA), expand=c(.005,.005))  +
  guides(color=guide_legend(order=1)) 

library(patchwork)
p3 = (p3a + p3b) + plot_annotation(title='Bee Colonies in United States, from 2015 Q1 to 2021 Q2') & mytheme2

# Colonies loss percent (acceptable/not acceptable) in winter 
# Inspired by @notquitemygrey (https://twitter.com/quite_grey/status/1481218044768751617)

colony %>% 
  filter(!(state=="United States"|state=="Other States")) %>%
  filter(months=="January-March") %>%
  mutate(group = ifelse(colony_lost_pct >= 17.8, "Unacceptable (more or equal to 17.8%)", "Acceptable (less than 17.8%)")) %>%
  group_by(state) %>%
  mutate(number=row_number()) %>%
  ggplot(aes(number, colony_lost_pct, fill=group)) +
  #geom_hline(yintercept=17.8, color="#23212C", linetype = "dotted") +
  geom_col() +
  scale_fill_manual(name=NULL,values=c("#333533","#ffba08")) +
  scale_y_continuous(name=NULL, breaks=c(0,25,50)) +
  scale_x_continuous(name=NULL, breaks = c(2,4,6), labels=c("'16","'18","'20")) +
  facet_geo(~state,grid=my_grid) +
  geo_theme +
  theme(legend.text=element_text(size=9),
        plot.background = element_rect(fill="#fefcfb", color=NA)) +
  labs(title="Bee Colonies Losses in Winter, 2015 to 2021",
       subtitle="In 45 States, January to March")




