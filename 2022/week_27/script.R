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

# Plot 1
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

# Plot 1, v2: clean neighborhood names 

## nbrhood names that are not in shape file
#anti_join(rent %>% filter(city=="san francisco") %>% mutate(nhood= str_to_title(nhood)) %>% rename(nbrhood=nhood), shp1) %>% count(nbrhood)

rent1 = rent %>% filter(city=="san francisco") %>%
  mutate(nhood= str_to_title(nhood),
         nhood1=case_when(nhood=="Buena Vista Park / Ashbury Hts / Corona Hts"~"Buena Vista Park/Ashbury Heights",
                          nhood=="Financial District"~"Financial District/Barbary Coast",
                          nhood=="West Portal / Forest Hills"~"West Portal",
                          nhood=="Bernal"~"Bernal Heights",
                          nhood=="Civic / Van Ness"~"Van Ness/Civic Center",
                          nhood=="Cole Valley"~"Cole Valley/Parnassus Heights",
                          nhood=="Lower Pac Hts"~"Lower Pacific Heights",
                          nhood=="Marina / Cow Hollow"~"Marina",
                          nhood=="Soma / South Beach"~"South Beach",
                          nhood=="Usf / Anza Vista"~"Anza Vista",
                          nhood=="Lakeshore"~"Lake Shore",
         TRUE~nhood)) %>%
  rename(nbrhood=nhood1) 
  
x1 = rent1 %>% filter(!(nbrhood %in% c("Excelsior / Outer Mission","North Beach / Telegraph Hill","Presidio Hts / Laurel Hts / Lake St")))
x2 = rbind(rent1 %>% filter(nbrhood=="Excelsior / Outer Mission") %>% mutate(nbrhood="Excelsior"),
      rent1 %>% filter(nbrhood=="Excelsior / Outer Mission") %>% mutate(nbrhood="Outer Mission"),
      rent1 %>% filter(nbrhood=="North Beach / Telegraph Hill") %>% mutate(nbrhood="North Beach"),
      rent1 %>% filter(nbrhood=="North Beach / Telegraph Hill") %>% mutate(nbrhood="Telegraph Hill"),
      rent1 %>% filter(nbrhood=="Presidio Hts / Laurel Hts / Lake St") %>% mutate(nbrhood="Presidio Heights"),
      rent1 %>% filter(nbrhood=="Presidio Hts / Laurel Hts / Lake St") %>% mutate(nbrhood="Jordan Park / Laurel Heights"),
      rent1 %>% filter(nbrhood=="Presidio Hts / Laurel Hts / Lake St") %>% mutate(nbrhood="Lake Street"))

x3 = rbind(x1,x2)
x4 = x3 %>% filter(year>2002) %>%
  group_by(nbrhood, year) %>%
  summarise(n=n(),
            median=median(price)) %>%
  ungroup() %>%
  filter(n>=10)
  
shp1 %>% right_join(x4,by = "nbrhood") %>%
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
  
ggsave("p1_v2.png", bg="white", height=8.2, width=7)      