# 2022-08-09
# TidyTuesday week 32, data from ferriswheels package by Emil Hvitfeldt

# Load libraries
library(tidyverse)
library(ggh4x)
library(ggrepel)
library(showtext)

# Load font
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
font_add_google("Outfit")
f1 = "Outfit"

# Data
wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')

# Wrangle
w1 = wheels %>% 
  select(name, diameter,number_of_cabins, height,status, country) %>% drop_na() %>%
  mutate(r=diameter/2,
         h=height-r,
         status1= fct_lump(status,2,other_level = "Delayed/In development/Under Construction"),
         lab=glue::glue("{name}, {country}"))

s1= w1 %>% arrange(desc(diameter), desc(height)) %>% slice(1:10) %>% pull(name)
s2 = w1 %>% arrange(height) %>% slice(1:2) %>% pull(name)
s3 = w1 %>% arrange(diameter) %>% slice(1) %>% pull(name)
w2b = w1 %>% filter(name %in% s1 | name %in% s2 | name %in% s3 | status1=="Defunct")

# Plot
w1 %>%
  ggplot() +
  ggforce::geom_circle(aes(x0=0, y0=h, r=r, color=factor(status1, levels=c("Operating","Defunct","Delayed/In development/Under Construction"))), alpha=0, show.legend=FALSE) +
  ggrepel::geom_text_repel(data=w2b, aes(x=0, y=height, label=str_wrap(lab,14), color=factor(status1, levels=c("Operating","Defunct","Delayed/In development/Under Construction"))), hjust=0, size=3,direction="y", xlim=c(375,700), lineheight=.8, segment.linetype="dotted", segment.size=.3, key_glyph=draw_key_point, family=f1) +
  scale_y_continuous("Height (in feet)",breaks=seq(0,750,175), expand = expansion(mult = c(.01,.03)),guide = "axis_minor", minor_breaks = seq(0, 750, by = 25)) +
  scale_x_continuous("Radius (in feet)",breaks=seq(-350,350,175), guide = "axis_minor",minor_breaks = seq(-350, 350, by = 25), limits=c(-350,460), expand=c(.01,.01)) +
  coord_fixed(clip="off") +
  scale_color_manual(name="Status:",values=c("#008F94","#E44D46","#E3AB03")) +
  cowplot::theme_minimal_grid(9) +
  theme(text=element_text(family=f1),
        panel.grid.major = element_blank(),
        axis.title.x=element_text(hjust=.42),
        axis.ticks.length = unit(.3,"lines"),
        axis.ticks=element_line(color="black", size=.3),
        axis.title=element_text(size=9),
        legend.position = "top",
        legend.title=element_text(size=9.3),
        legend.text=element_text(size=9.3),
        legend.margin=margin(l=-30),
        plot.title.position = "plot",
        plot.title=element_text(size=14),
        plot.subtitle=element_text(size=9.3),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust=0, size=8.5, color="grey40", margin=margin(t=8)),
        plot.margin=margin(10,10,10,0)
        ) +
  labs(caption="TidyTuesday week 32  •  Source: {ferriswheels} package by Emil Hvitfeldt",
       title="Ferris Wheels",
       subtitle="Height in feet and diameter in feet of 35 ferris wheels located in 12 countries")
       
ggsave("2022_32.png", height=7, width=7, bg="#faf9f9")       


