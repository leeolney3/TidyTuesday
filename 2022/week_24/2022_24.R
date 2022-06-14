# 2022-06-14
# TidyTuesday week 24 US Drought
# Data source: National Integrated Drought Information System

# Libraries
library(tidyverse)
library(geofacet)
library(ggnewscale)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("IBM Plex Sans")
f1 = "IBM Plex Sans"

# Data
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')
drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')

df1 = drought %>%
  mutate(DATE=str_remove(DATE,"d_"),
         date=lubridate::ymd(DATE),
         year=lubridate::year(date)) %>%
  filter(year>=2010) %>%
  mutate(state=str_to_title(gsub("-", " ", state)),
         st = state.abb[match(state,state.name)])
         
range(df1$date)

df1a = df1 %>% select(state, st, date, 3:7) %>%
  pivot_longer(!1:3)

df1b = df1 %>% select(state, st, date, 9:13) %>%
  pivot_longer(!1:3) %>%
  mutate(value=value*-1) 

# Grid geofacet
mygrid = us_state_grid1 %>% filter(!code %in% c("DC","HI","AK"))

# Plot
# inspired by TidyTuesday 2021w30 submissions: Christophe Nicault (https://twitter.com/cnicault/status/1417858091190792192/photo/1), Kaustav Sen (https://twitter.com/kustav_sen/status/1419341345072631811), Maxwel C. Oliveira (1417454551423193091), Johnny Lillis (https://twitter.com/johnny_c_lillis/status/1422222351761092612/photo/1)
df1a %>% ggplot() +
  geom_col(data=df1a, aes(x=date, y=value, fill=name)) +
  scale_fill_brewer("",palette = "OrRd", labels=c("Abnormally dry","Moderate drought","Severe drought","Extreme drought","Exceptional drought")) +
  guides(fill=guide_legend(nrow=2)) +
  ggnewscale::new_scale_fill() +
  geom_col(data=df1b, aes(x=date, y=value, fill=name)) +
  scale_fill_brewer("",palette = "Blues", labels=c("Abnormally wet","Moderate wet","Severe wet","Extreme wet","Exceptional wet")) +
  guides(fill=guide_legend(nrow=2)) +
  scale_x_date(date_labels = "'%y") +
  facet_geo(~st, grid=mygrid, label = "name") +
  cowplot::theme_minimal_grid(9.5) +
  theme(text=element_text(color="white", family=f1),
        panel.grid = element_blank(),
        legend.position = "top",
        axis.text.y=element_blank(),
        axis.text.x = element_text(color="white", size=rel(.7)),
        axis.title=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x = element_line(color="white", size=.2),
        plot.title=element_text(size=rel(1.2), hjust=0.5),
        plot.subtitle = element_text(hjust=0.5, margin=margin(b=8)),
        strip.text = element_text(size=rel(.7)),
        legend.justification = "center",
        plot.caption=element_text(hjust=0, color="grey90"),
        plot.margin=margin(.5,.5,.5,.5, unit="cm"),
        plot.background=element_rect(fill="#212529", color=NA)) +
  labs(caption="\n#TidyTuesday week 24 |  Source: National Integrated Drought Information System",
       title="Drought Conditions in the US",
       subtitle="January 01 2010 to April 01 2022")         

ggsave("2022_24.png", height=7, width=9)         
