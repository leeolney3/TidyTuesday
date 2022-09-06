# 2022-09-06
# TidyTuesday week 26 LEGO database, rebrickable courtesy of Georgios Karamanis.


# Load libraries
library(tidyverse)
library(ggtext)
library(ggimage)
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

# Merge data
df= inventories %>% left_join(inventory_parts, by=c("id"="inventory_id")) %>%
  left_join(colors, by=c("color_id"="id")) %>%
  filter(is_spare==FALSE) %>%
  left_join(sets %>% select(-num_parts,-img_url), by="set_num") %>%
  filter(is_trans==FALSE, color_id!=9999,between(year, 2002,2021))

# Most common soild colors from 2002 to 2021
s1= df %>% 
  group_by(year,color_id, name.x, rgb, is_trans) %>%
  tally(quantity) %>%
  ungroup() %>%
  group_by(name.x) %>%
  summarise(n_year=n_distinct(year)) %>%
  filter(n_year==20) 
  
s2= df %>% filter(name.x %in% s1$name.x) %>%
  group_by(name.x) %>%
  tally(quantity, sort=TRUE)%>%
  filter(n>100000) #7 colors with more than 100000 bricks
  
# Wrangle
d1= df %>% 
  group_by(year, color_id, name.x, rgb, is_trans) %>%
  tally(quantity) %>%
  mutate(grp=case_when(!name.x %in% s2$name.x~"Other", TRUE~paste(name.x))) %>%
  ungroup() %>%
  group_by(year,grp) %>%
  tally(n) %>%
  mutate(prop=round(n/sum(n)*100)) %>%
  select(-n) %>%
  pivot_wider(names_from = grp, values_from =prop) %>%
  select(-Other) %>%
  mutate(Other=100-Black-Blue-`Light Bluish Gray`-Red-Tan-White-Yellow) %>%
  ungroup() %>%
  pivot_longer(!year) 

d2= d1 %>% uncount(value) %>%
  mutate(name=factor(name, levels=s2$name.x)) %>%
  group_by(year) %>%
  arrange(name, .by_group = TRUE) %>%
  mutate(x=rep(1:10,10),
         id=row_number(),
         y=cut(id, breaks = seq(0,100,10), label=seq(1,10,1))) %>%
  select(-id) %>%
  ungroup() %>%
  mutate(image=case_when(name=="Black"~"bricks/Black.png",
                       name=="Blue"~"bricks/Blue.png",
                       name=="Tan"~"bricks/Tan.png",
                       name=="Red"~"bricks/Red.png",
                       name=="White"~"bricks/White.png",
                       name=="Yellow"~"bricks/Yellow.png",
                       name=="Light Bluish Gray"~"bricks/Light.png",
                       ))
                       
# Waffle plot
# inspired by Abdoul Madjid @issa_madjid https://twitter.com/issa_madjid/status/1526685250994286592/photo/1
d2 %>% drop_na(image) %>%
  ggplot(aes(x=x, y=y)) +
  geom_image(aes(image=image), size=.1) +
  facet_wrap(~year, ncol=5, strip.position = "bottom") +
  cowplot::theme_map(10) +
  theme(text=element_text(family=f1),
        panel.spacing.y = unit(.6, "lines"),
        panel.spacing.x = unit(.8, "lines"),
        strip.text=element_text(hjust=0, face="bold", color="grey50", size=8.9),
        plot.title=element_text(face="bold.italic", size=14),
        plot.subtitle=element_markdown(lineheight=1.2, size=8.8),
        plot.margin=margin(.5,.5,.4,.5,unit="cm"),
        plot.caption=element_text(margin=margin(t=10))) +
  labs(title=str_to_upper("Lego Colors"),
       subtitle="**One brick represents 1% of bricks by set year** from 2002 to 2021, including seven most common soild lego colors <br>(Black, Light Bluish Gray, White, Red, Blue, Tan, and Yellow).",
       caption="TidyTuesday week 35  •  Data from rebrickable.com")
       
ggsave("2020_36.png", height=7, width=7, bg="white")                                