#TidyTuesday Week 50 2021-12-07
#Spiders, data from World Spiders Database

# load libraries 
library(tidyverse)
library(wesanderson)
library(ggalluvial)

# load font
library(sysfonts)
library(showtext)
font_add_google("Libre Franklin")
showtext_auto()

# import data
spiders <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')

# wrangle
spiders1 = spiders %>% filter(year>=1980, year<2021)

ind = spiders1 %>%
  filter(grepl('India', distribution)) %>%
  count(year) %>% mutate(country="India")

usa = spiders1 %>%
  filter(grepl('USA', distribution)) %>%
  count(year) %>% mutate(country="USA")

sa = spiders1 %>%
  filter(grepl('South Africa', distribution)) %>%
  count(year) %>% mutate(country="South Africa")

br = spiders1 %>% 
  filter(grepl('Brazil', distribution)) %>%
  count(year) %>% mutate(country="Brazil")

spiders2 = bind_rows(ind,usa, sa, br)

# plot
spiders2 %>%
  ggplot(aes(x=year, y=n, alluvium=country)) +
  geom_alluvium(aes(fill=country, color=country),
                alpha=.75, decreasing=FALSE) +
  scale_fill_manual(values=wes_palette("BottleRocket2")) +
  scale_color_manual(values=wes_palette("BottleRocket2")) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0.01,0.01)) +
  cowplot::theme_minimal_hgrid(11.5) +
  theme(text=element_text(family="Libre Franklin"),
        legend.position = "top",
        legend.margin=margin(l=-39),
        plot.margin=margin(.75,1,.5,.75, unit="cm"),
        plot.background = element_rect(fill="#f8f9fa",color=NA),
        axis.title=element_text(face="bold", size=10),
        axis.text=element_text(size=8.7),
        plot.caption = element_text(margin=margin(t=8), size=8),
        plot.title.position = "plot",
        plot.title=element_text(size=12)
        ) +
  labs(title="Number of currently valid spider species described per year",
       subtitle="by distribution region from 1980 to 2020, according to World Spider Database",
       fill="", color="", x="Year", y="Species count",
       caption="#TidyTuesday Week 50 | Data: World Spider Database")
