# TidyTuesday week 31 Oregon Spotted Frog, data from USGS
# Voronoi map inspired by [@VictimOfMaths](https://twitter.com/VictimOfMaths/status/1323627698141515776) and [@_ansgar](https://twitter.com/_ansgar/status/1556282682115497987)

# Load Libraries
library(tidyverse)
library(osmdata)
library(sf)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load font
font_add_google("Inter")
f1 = "Inter"

# Load data
frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/Oregon_spotted_frog_telemetry_at_Crane_Prairie_OR.csv', skip=2)

# Convert UTM coordinates to long and lat
# code from @KittJonathan https://twitter.com/KittJonathan/status/1554455567539834882
utm_coords <- frogs %>% 
  select(UTME_83, UTMN_83)

longlat <- st_as_sf(x = utm_coords,
                    coords = c("UTME_83", "UTMN_83"),
                    crs = "+proj=utm +zone=10") %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
  as_tibble()

list_long <- list()
list_lat <- list()

for (i in 1:nrow(longlat)) {
  list_long[[i]] <- longlat$geometry[[i]][1]
  list_lat[[i]] <- longlat$geometry[[i]][2]
}

frogs <- frogs %>% 
  mutate(long = unlist(list_long),
         lat = unlist(list_lat)) %>% 
  select(long, lat, Habitat = HabType)
  
# Get Crane Prairie Reservoir sf
# code from https://rpubs.com/scolando/Tidy-Tuesday-08-02-2022 via @geokaramanis
crane_prairie <- opq(bbox = c(-121.7920729, 43.7938767, -121.76501, 43.81433)) %>%
  add_osm_feature(key = 'natural', value = 'water') %>%
  osmdata_sf()

# Polygons and points  
outline = crane_prairie$osm_polygons
data = frogs %>% st_as_sf(coords=c("long", "lat"), crs = "EPSG:4326")

# Generate voronoi polygons 
# Code from @VictimOfMaths https://twitter.com/VictimOfMaths/status/1323627698141515776 and @_ansgar https://twitter.com/_ansgar/status/1556282682115497987

voronoi <- data %>% 
  st_union() %>%
  st_voronoi() %>%
  st_collection_extract()

voronoi <- voronoi[unlist(st_intersects(data,voronoi))] %>%
  st_intersection(outline)

voronoi <- data %>% 
  st_combine() %>% 
  st_voronoi() %>% 
  st_cast() %>% 
  st_intersection(outline) %>%
  st_cast() %>% 
  st_sf()

voronoi_join = st_join(voronoi, data)

# Map
ggplot() +
  geom_sf(data=outline, aes(geometry=geometry), fill="white", colour=NA) +
  geom_sf(data=voronoi, aes(geometry=geometry, fill=rownames(voronoi)), colour="white",
          size=0.2, show.legend=FALSE, alpha=.9) +
  geom_sf(data=data, alpha=.4, color="#E54C47") +
  scale_fill_manual(values=pnw_palette("Cascades",length(rownames(voronoi)))) +
  theme_void() +
  #cowplot::theme_map(10) +
  theme(text=element_text(family=f1),
        plot.caption = element_text(hjust=.5,size=6, color="grey40",margin=margin(t=-5)),
        plot.title=element_text(hjust=.5, size=13, face="bold"),
        plot.subtitle = element_text(size=8.5, lineheight = 1, hjust=.5),
        plot.margin=margin(.3,0,.3,0,unit="cm")) +
  labs(title="Oregon Spotted Frogs",
       subtitle="Voronoi map of 311 frogs observed at Crane Prairie Reservoir\nbetween Sep 12, 2018 and Nov 29,2018",
       caption="TidyTuesday week 31 | Data source: USGS | Plot inspired by @VictimOfMaths and @_ansgar")
       
ggsave("2022_31.png", height=6, width=6, bg="white")       
       



