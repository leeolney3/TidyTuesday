# 2022-07-05
# TidyTuesday week 27, San Francisco Rentals
# Data source: Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts, 2000-2018. Retrieved from https://github.com/katepennington/historic_bay_area_craigslist_housing_posts/blob/master/clean_2000_2018.csv.zip.
# Shape file source: https://data.sfgov.org/Geographic-Locations-and-Boundaries/Realtor-Neighborhoods/5gzd-g9ns

# Load Libraries
library(tidyverse)
library(sf)

# Import data 
rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

# Import shp 
shp1= read_sf("data/RealtorNeighborhoods/geo_export_a1971932-cf60-4647-be46-313756284495.shp")

# Wrangle
rent1 = rent %>% filter(city=="san francisco") %>%
  mutate(nhood= str_to_title(nhood),
         nhood1=case_when(nhood=="Buena Vista Park / Ashbury Hts / Corona Hts"~"Buena Vista Park/Ashbury Heights",
                          nhood=="Financial District"~"Financial District/Barbary Coast",
                          nhood=="West Portal / Forest Hills"~"West Portal",
         TRUE~nhood))

d1 = rent1 %>% filter(year>2002) %>% 
  group_by(nbrhood=nhood1, year) %>%
  summarise(n=n(),
            min=min(price),
            max=max(price),
            mean=mean(price),
            median=median(price)) %>%
  ungroup() %>%
  filter(n>=10) 
            
# Plot
shp1 %>% right_join(d1,by = "nbrhood") %>%
  ggplot() +
  geom_sf(data=shp1, fill=NA, size=.1) +
  geom_sf(aes(fill=median), size=.1) +
  scale_fill_stepsn("Median rent prices in USD by year and neighborhood",colors=rev(MetBrewer::met.brewer("Johnson")), labels=scales::dollar)+
  facet_wrap(~year) +
  cowplot::theme_map(11) +
  theme(legend.position="top",
        legend.title=element_text(size=10),
        legend.text=element_text(size=9.5),
        plot.caption=element_text(color="#37363A",hjust=0, margin=margin(t=10)),
        plot.margin=margin(.4,.4,.4,.4,unit="cm"),
        strip.text = element_text(face="bold", size=11.5),
        ) +
  labs(title="Rent in the City of San Francisco ",
       caption="•  #TidyTuesday week 27  •\nNote: Median rent prices from a total of 54,236 listings, with 10 or more listings by year and neighborhood\nSource: data.sfgov.org & Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts, 2000-2018. Retrieved from\nhttps://github.com/katepennington/historic_bay_area_craigslist_housing_posts/blob/master/clean_2000_2018.csv.zip.") +
  guides(fill=guide_colorbar(title.position="top",barwidth = unit(80, units="mm"), barheight = unit(2.5, unit="mm")))
  
ggsave("p1.png", bg="white", height=8.2, width=7)  