# 2022-03-22
# TidyTuesday week 12, Baby names
# Data source {babynames} R package from Hadley Wickham

library(tidyverse)
library(showtext)
font_add_google("Outfit")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

selected = babynames %>%
  group_by(name) %>%
  tally(prop) %>% 
  arrange(desc(n)) %>%
  slice(1:15) %>%
  pull(name)
  
babynames %>% filter(name %in% selected) %>%
  ggplot(aes(year, prop)) +
  geom_col(aes(fill=prop),show.legend = F) +
  facet_wrap(~name, ncol=3) +
  scale_y_continuous(labels=scales::percent, breaks=c(0.035,0.07)) +
  scico::scale_fill_scico(palette="bamako") +
  theme_minimal() +
  theme(text=element_text(family="Outfit"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_line(size=.3),
        plot.margin=margin(.5,.5,.5,.5, unit = "cm"),
        plot.title.position = "plot",
        plot.title=element_text(hjust=.5, size=16, margin=margin(b=8)),
        plot.subtitle=element_text(hjust=.5, size=10.5),
        panel.spacing.y = unit(1.5, "lines"),
        strip.text=element_text(hjust=0, size=10.5),
        axis.text=element_text(size=8.5),
        axis.title=element_blank()
  ) +
  labs(title= "Names of newborns (1880-2017)",
       subtitle="15 most common names by proportion of total births in the US, arranged in alphabetical order\n",
       caption="\n#TidyTuesday week 12 | Data source: babynames R package from Hadley Wickham")
       
ggsave("2022_12.png", height=8, width=8, unit="in", bg="white")