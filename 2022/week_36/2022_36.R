# 2022-09-06
# TidyTuesday week 26 LEGO database, rebrickable courtesy of Georgios Karamanis.
# Lego color plots, inspired by [Edurne Morillo](https://blog.datawrapper.de/lego-sets-colors-history/)

# Load libraries
library(tidyverse)
library(ggalluvial)
library(patchwork)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load font
font_add_google("DM Sans")
f1 = "DM Sans"

# Read in data
inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
inventory_parts = readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz")
colors = readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz")

# Wrangle
df= inventories %>% left_join(inventory_parts, by=c("id"="inventory_id")) %>%
  left_join(colors, by=c("color_id"="id")) %>%
  filter(is_spare==FALSE) %>%
  left_join(sets %>% select(-num_parts,-img_url), by="set_num") %>%
  filter(between(year, 1990,2021))
  
## soild bricks colors
selected_colors1= df %>% filter(is_trans==FALSE, color_id!=9999) %>%
  group_by(year,color_id, name.x, rgb, is_trans) %>%
  tally(quantity) %>%
  ungroup() %>%
  group_by(color_id) %>%
  summarise(n_year=n_distinct(year)) %>%
  arrange(desc(n_year)) %>%
  filter(n_year==max(n_year)) %>%
  pull(color_id)
df1 = df %>% 
  group_by(year,color_id, name.x, rgb, is_trans) %>%
  tally(quantity) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct=n/sum(n)) %>%
  ungroup() %>%
  filter(color_id %in% color_id) %>%
  mutate(col=glue::glue("#{rgb}")) %>%
  filter(color_id %in% selected_colors1) 
df1c= df1 %>% select(name.x, col) %>%
  distinct() %>% arrange(name.x)
  
## translucent
selected_colors2= df %>% filter(is_trans==TRUE, color_id!=9999) %>%
  group_by(year,color_id, name.x, rgb, is_trans) %>%
  tally(quantity) %>%
  ungroup() %>%
  group_by(color_id) %>%
  summarise(n_year=n_distinct(year)) %>%
  arrange(desc(n_year)) %>%
  filter(n_year==max(n_year)) %>%
  pull(color_id)
df2 = df %>% 
  group_by(year,color_id, name.x, rgb, is_trans) %>%
  tally(quantity) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct=n/sum(n)) %>%
  ungroup() %>%
  filter(color_id %in% color_id) %>%
  mutate(col=glue::glue("#{rgb}")) %>%
  filter(color_id %in% selected_colors2) 
df2c= df2 %>% select(name.x, col) %>%
  distinct() %>% arrange(name.x)  
  
# Plot
theme1= cowplot::theme_minimal_grid(10) +
  theme(text=element_text(family=f1),
        panel.grid=element_blank(),
        axis.ticks.length=unit(.2, "cm"),
        axis.ticks=element_line(color="black", size=.25),
        plot.background = element_rect(fill="#BEC1C3",color=NA),
        plot.title.position = "plot",
        plot.title=element_text(size=13, face="bold.italic"),
        plot.margin=margin(t=12))
p1 = df1 %>% ggplot(aes(x=year, y=pct, alluvium=name.x)) +
  ggalluvial::geom_alluvium(aes(fill=name.x),decreasing = FALSE, alpha=1, show.legend = FALSE) +
  scale_fill_manual(values=df1c$col) +
  scale_y_continuous("Percentage", labels=scales::percent, expand=c(0.01,0.01), breaks=seq(0,.8,.1)) +
  scale_x_continuous("Year", breaks=seq(1990,2020,5),expand=c(0.02,0.02)) +
  theme1 +
  labs(title="Soild Colors")

p2 = df2 %>% ggplot(aes(x=year, y=pct, alluvium=name.x)) +
  ggalluvial::geom_alluvium(aes(fill=name.x, color=name.x),decreasing = FALSE, alpha=.7, show.legend = FALSE, size=.7) +
  scale_fill_manual(values=df2c$col) +
  scale_color_manual(values=df2c$col) +
  scale_y_continuous("Percentage", labels=scales::percent, expand=c(0.001,0.001)) +
  scale_x_continuous("Year", breaks=seq(1990,2020,5),expand=c(0.02,0.02)) +
  theme1 +
  labs(title="Translucent colors")
  
p1/p2 +
    plot_annotation(title = 'Most common LEGO colors',
                    subtitle= 'Percentage of color by set year, from 1990 to 2021',
                    caption='TidyTuesday week 35  •  Data from rebrickable',
                    theme = 
                      theme(plot.title = element_text(hjust=.5, size = 16, family=f1, face="bold"),
                      plot.subtitle=element_text(size=9,hjust=.5,family=f1, margin=margin(b=-5)),
                      plot.caption=element_text(size=8.5,family=f1,margin=margin(t=10), color="grey20"),
                      plot.margin=margin(.5,.5,.4,.5,unit="cm"),
                      plot.background = element_rect(fill="#BEC1C3",color=NA))
                    )    
  
ggsave("2022_36.png", height=9, width=6.6)