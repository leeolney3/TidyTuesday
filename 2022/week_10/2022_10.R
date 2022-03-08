library(tidyverse)
library(tidygeocoder)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(cowplot)

erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

# participants from Luxembourg
df = erasmus %>% filter(sending_city=="Luxembourg") %>%
  mutate(receiving_city =case_when(receiving_city=="Arrent�s-de-Corcieux"~"Arrentès-de-Corcieux",
                                   receiving_city=="Bruxelles"~"Brussels",
                                   TRUE~receiving_city)) %>%
  group_by(receiving_city, receiving_country_code) %>%
  tally(participants)
  
# get coordinates for cities
df1 = df %>% mutate(address=paste(receiving_city,receiving_country_code)) %>%
  geocode(address, method="osm", lat = rec_latitude , long = rec_longitude)
  
df2 = df1 %>% 
  mutate(send_latitude = 49.61128,
         send_longitude = 6.129799) %>%
  filter(receiving_city!="Luxembourg")
  
# map
p1 =ggplot() +
  geom_sf(data=Europe, size=.2, fill="grey95") +
  geom_segment(data=df2, aes(x=send_longitude, y=send_latitude, xend=rec_longitude, yend=rec_latitude, size=n),
               color="#3a0ca3",alpha=.4, show.legend = F) +
  geom_text(data=dflab,aes(x=rec_longitude, y=rec_latitude, label=address), size=3, color="#3a0ca3") +
  coord_sf(ylim=c(41, 60), xlim=c(-11,26)) +
  cowplot::theme_map(10) +
  theme(plot.caption=element_text(size=7.5)) +
  labs(title="Erasmus student mobility: Students from Luxembourg",
       subtitle="From academic year 2014-2015 to 2019-2020, 121 out of 2739 participants from Luxembourg went to other cities for the\nERASMUS program.",
       caption="TidyTuesday Week 10 | Data from Data.Europa by way of Data is Plural")
  
# bar 
p2 = df2 %>%
  ggplot(aes(n, y=reorder(address,-n))) +
  geom_col(fill="#3a0ca3", alpha=.9, width=.7) +
  geom_text(aes(label=n), color="white", size=1.8, hjust=1.2) +
  scale_x_continuous(expand=c(0,0)) +
  cowplot::theme_minimal_grid(6.6) +
  theme(panel.grid = element_blank(),
        axis.title=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks = element_blank())
        
# combine plot
ggdraw() +
  draw_plot(p1,0,0,1,1) +
  draw_plot(p2,0.02,0.07,0.35,0.38)

ggsave("2022_10.png",height=7, width=7,unit="in", bg="white")