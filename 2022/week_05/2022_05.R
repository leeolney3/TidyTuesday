# TidyTuesday Week 5 2022-02-01 Dog Breeds
# Data from American Kennel Club courtesy of KKakey
# Plot style inspired by Washington Post 

library(tidyverse)
library(ggbump)
library(ggtext)
library(ggimage)

library(showtext)
font_add_google("Lato")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
f1= "Lato"

# Load data
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv') %>% janitor::clean_names()
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv') %>% janitor::clean_names()
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv') %>% janitor::clean_names()

# Plot data
rank_data = breed_rank_all %>% pivot_longer(x2013_rank:x2020_rank) %>%
  filter(value<=10) %>%
  mutate(name = parse_number(name)) %>%
  mutate(y = trimws(str_replace_all(breed, "\\*|\\(|\\)", ""))) %>%
  mutate(drool_level = case_when(breed == "Retrievers (Labrador)" ~2,
                           breed == "Retrievers (Golden)" ~ 2,
                           breed == "German Shepherd Dogs" ~ 2,
                           breed == "Pointers (German Shorthaired)" ~2,
                           breed=="French Bulldogs" ~3,
                           breed=="Bulldogs"~3,
                           breed=="Poodles"~1,
                           breed=="Beagles"~1,
                           breed=="Rottweilers"~3,
                           breed=="Dachshunds"~2,
                           breed=="Pembroke Welsh Corgis"~1,
                           breed=="Yorkshire Terriers"~1,
                           breed=="Boxers"~3))
                           
# Bump plot
p1 = rank_data %>%
  ggplot(aes(name,value)) +
  geom_bump(aes(group=breed,color=factor(drool_level)), size=5, smooth = 8, alpha=.8) +
  geom_vline(xintercept = seq(2013, 2020,1), color="white", size=.2) +
  scale_y_reverse() +
  scale_x_continuous(breaks=seq(2013, 2020,1)) +
  scale_color_manual("Drooling Level (1 to 5)",values=c("#15607a","#00dca6","#fa8c00")) +
  coord_cartesian(clip="off") +
  theme_void(base_size = 10, base_family = f1) +
  theme(legend.position = "top",
        legend.margin=margin(t=6,b=-2),
        legend.justification = "left",
        plot.margin = unit(c(.5, 1, .5, .5), "cm"),
        plot.title=element_text(face="bold"),
        axis.line.x.bottom = element_line(),
        axis.text.x=element_text(size=8, margin=margin(t=3)),
        axis.ticks.x = element_line(),
        axis.ticks.length=unit(.15, "cm"),
        plot.caption.position = "plot",
        plot.caption=element_text( hjust=0, margin=margin(t=10)),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8)
        ) +
    labs(title="10 most popular dog breeds and their drooling level",
         subtitle="Popularity of dog breeds by AKC registration statistics from 2013-2020",
         caption="TidyTuesday Week 5 | Data from American Kennel Club courtesy of KKakey")
         
p1 + 
  geom_image(data = rank_data %>% filter(name == min(name)),aes(image=image, x=name-.3), asp=2) +
  geom_image(data = rank_data %>% filter(name == max(name)),aes(image=image, x=name+.3), asp=2) +
  geom_text(data= rank_data %>% filter(name==2019, value<=5), 
            aes(label=breed, x= 2018.5), size=3, family=f1) + 
  geom_text(data= rank_data %>% filter(name==2020, between(value,6,7)), 
            aes(label=breed, x=2019.8), size=3, color="white",family=f1, hjust=1) +
  geom_text(data= rank_data %>% filter(name==2020, between(value,8,10)), 
            aes(label=breed, x=2019.8), size=3,family=f1, hjust=1) +
  geom_text(data= rank_data %>% filter(name==2014, value==6), 
            aes(label=breed, x=2014.25), size=3, color="white", hjust=1,family=f1) +
  geom_text(data= rank_data %>% filter(name==2014, value==8), 
            aes(label=breed, x=2014.25), size=3, hjust=1,family=f1) 
            
# Save
ggsave("2022_05.png", bg="white", height=5, width=7.4)            