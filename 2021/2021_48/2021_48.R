#TidyTuesday Week 48
#https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-11-23/readme.md

library(tidyverse)
library(showtext)
font_add_google("Lato")
font_add_google("Libre Franklin")
showtext_auto()

directors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/directors.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')
writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/writers.csv')
imdb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/imdb.csv')

colSums(is.na(episodes))
colSums(is.na(imdb))

imdb %>%
  ggplot(aes(y=fct_rev(factor(season)), x=factor(ep_num))) +
  geom_point(aes(color=rating, size=rating_n)) +
  scale_color_gradientn(colors=LaCroixColoR::lacroix_palette("PeachPear", type="continuous"),
                        breaks=c(3.9,6,8,9.8)) +
  scale_size_area(breaks=c(2808, 10000, 19688), label=scales::comma) +
  coord_fixed() +
  theme_minimal(base_family = "Lato") +
  theme(plot.margin=margin(.5,.5,.5,.5, unit="cm"),
        panel.grid=element_line(linetype = "dotted", color="grey60", size=.2),
        legend.title=element_text(size=8, margin=margin(b=2)),
        legend.text=element_text(size=7.5),
        axis.text=element_text(color="black", size=9),
        axis.title.x=element_text(color="black", size=9, face="bold", margin=margin(t=3)),
        axis.title.y=element_text(color="black", size=9, face="bold"),
        plot.caption.position = "plot",
        plot.caption = element_text(size=7, hjust=0, margin=margin(t=10)),
        plot.title.position = "plot",
        plot.title=element_text(face="bold",family="Libre Franklin", size=11),
        plot.subtitle=element_text(size=7.5, margin=margin(b=10), lineheight = 1.2),
        plot.background = element_rect(fill="#f8f9fa", color=NA)
        ) +
  guides(color=guide_colorbar(barheight = unit(7, "lines"),
                              barwidth = unit(.5, "lines"),
                              order=1),
         size=guide_legend(override.aes = list(shape=21))) +
  labs(color="IMDb rating", size="IMDb\nrating count",
       x="Episode", y="Season",
       caption="#TidyTuesday Week 48 | Data from datardis package and IMDb",
       title="Dr. Who Ratings",
       subtitle="S03E10 Blink has the highest IMDb rating (9.8) and rating count (19688). S12E03 Orphan 55 has the\nlowest rating (3.9).")