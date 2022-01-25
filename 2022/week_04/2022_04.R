# TidyTuesday Week 4, 2022-01-25, Board Games
# Data from Kaggle by way of Board Games Geek, shared by David and Georgios

# Load libraries
library(tidyverse)
library(ggtext)
library(showtext)

# Fonts
font_add_google("Archivo Narrow","arch")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load Data
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
#details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

# Wrangle
ratings2 = ratings %>% 
  filter(between(year, 1900, 2021)) %>%
  mutate(decade = floor(year/10)*10,
         decade = glue::glue("{decade}s"))
    
ratings3 = ratings2 %>%
  group_by(decade) %>%
  summarise(n=n(),
            min_rating=min(average),
            max_rating=max(average),
            min_rank= min(rank),
            max_rank=max(rank)) %>%
  pivot_longer(min_rating:max_rating) %>%
  mutate(ylab = glue::glue("<span style='color:black'>**{decade}**</span> (n={scales::comma(n, accuracy=1)})"))
 
labs_df = ratings2 %>%
  group_by(decade) %>%
  filter(average==min(average) | average==max(average)) %>%
  left_join(ratings3, by="decade") %>% 
  select(name.x, ylab, average, decade) %>% distinct()
  
# Plot
ratings3 %>%
  ggplot(aes(value, ylab)) +
  geom_line(aes(group=ylab), color="#f0c808") +
  geom_point(aes(color=name), show.legend=F, size=2.5) +
  geom_text(data = labs_df, aes(average,ylab, label= name.x),size=2.5, vjust=-1, family="arch", fontface="italic") +
  scale_color_manual(values=c("#dd1c1a","#07a0c3")) +
  scale_x_continuous(limits=c(0,10), expand=c(0,0)) +
  scale_y_discrete(expand = expansion(mult = c(.05, .07))) +
  coord_cartesian(clip="off") +
  cowplot::theme_minimal_grid(9.5,line_size = 0.3) +
  theme(text=element_text(family="arch"),
        panel.grid.major.y=element_line(linetype = "dotted"),
        axis.title =element_text(color="grey15", size=8),
        axis.text.y=element_markdown(hjust=0,color="grey40"),
        axis.text.x = element_text(color="grey20"),
        plot.margin=margin(.5,.75,.5,.5, unit="cm"),
        plot.title=element_text(size=12, hjust=.5),
        plot.subtitle = element_markdown(hjust=.5, size=9, margin=margin(b=7), color="grey20"),
        plot.background = element_rect(fill="#fcfcfc"),
        plot.caption=element_text(margin=margin(t=7), color="grey40"),
        plot.title.position = "plot",
        axis.ticks.y=element_blank()
        ) +
  labs(x="Average rating", y="Decade",
       title="Board games ratings by decade",
       subtitle="Board games with the <span style='color:#07a0c3'>**lowest**</span> and <span style='color:#dd1c1a'>**highest**</span> average rating by decade, published between 1900 and 2021.",
       caption="TidyTuesday Week 4  |  Data from Kaggle by way of Board Games Geek, shared by David and Georgios")
       
# Save plot
ggsave("2022_04.png", width=7, height=4.8)