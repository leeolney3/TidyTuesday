#TidyTuesday week 51 2021-12-14 
#Spice Girls, data from Spotify and Genius

library(tidyverse)
options(dplyr.summarise.inform = FALSE)
library(ggtext)
library(MetBrewer)

# lyrics data 
tuesdata <- tidytuesdayR::tt_load('2021-12-14')
lyrics = tuesdata$lyrics

# wrangle
# number of lines per track
lines_df = lyrics %>% 
  group_by(album_name, track_number) %>%
  summarise(line_n= n_distinct(line_number))

# section artist by line  
lyrics1 = lyrics %>%
  mutate(artist = strsplit(section_artist, ",")) %>%
  unnest(artist) %>%
  mutate(artist = str_to_lower(artist)) %>%
  group_by(album_name, track_number, artist) %>%
  summarise(line = n_distinct(line_number)) %>%
  ungroup()
  
fa = function(artist1) {
  lyrics1 %>% filter(str_detect(artist, artist1)) %>%
    group_by(album_name, track_number) %>% 
  tally(line) %>%
  mutate(artist=artist1)
}

artist_df = bind_rows(fa("all"),fa("scary"),fa("sporty"),fa("baby"),fa("ginger"),fa("posh")
          ) %>%
  arrange(album_name, track_number) %>%
  left_join(lines_df, by=c("album_name","track_number")) %>% 
  mutate(n1 = ifelse(n==79, 47,n)) %>%
  mutate(prop = (n1/line_n)*100,
         track_number=as.character(track_number)) 

# average proportion         
overall_df = artist_df %>%
  group_by(album_name, artist) %>%
  summarise(prop = mean(prop)) %>%
  mutate(track_number="Average")

df1 = rbind(artist_df, overall_df)

# theme
theme1 = theme_minimal(base_family="roboto condensed", base_size = 13) +
  theme(strip.placement = "outside",
        strip.text.y.left=element_markdown(angle=0, lineheight = 1.4, size=11.5),
        panel.grid=element_blank(),
        axis.title.x=element_text(size=11),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=11),
        axis.text.y=element_text(size=11, color="grey10"),
        plot.title=element_text(family="oswald", face="bold", margin=margin(b=7)),
        plot.margin=margin(.85,1.5,.6,.5, unit="cm"),
        legend.position = "top",
        legend.justification = "left",
        legend.title=element_text(size=11.8),
        panel.spacing = unit(1, "lines"),
        plot.caption=element_text(size=9.1, color="grey20", margin=margin(t=14)),
        plot.background = element_rect(fill="#e9ecef", color=NA),
        legend.margin=margin(b=0)
        ) 
        
# plot
df1 %>% 
  filter(album_name!="Forever") %>%
  mutate(album_name = case_when(album_name=="Spiceworld" ~ "**Spiceworld**<br>Album",
                                album_name=="Spice" ~ "**Spice**<br>Album")
         ) %>%
  ggplot(aes(fct_relevel(track_number, "10", after = 9),
             factor(str_to_title(artist), levels=c("All","Sporty","Scary","Posh","Ginger","Baby")),
             fill=prop)) +
  geom_tile(color="#e9ecef", size=.8) +
  geom_text(aes(label=round(prop,1)), family="roboto condensed", size=3.6, color="white") +
  scale_fill_gradientn(colors=met.brewer(name="Robert", 100, type = "continuous"),limits=c(0,100)) +
  scale_x_discrete(position="top") +
  facet_wrap(~album_name, ncol=1, strip.position = "left", scales="free_x") +
  theme1 +
  guides(fill = guide_colorbar(title.position = "top", 
                                barwidth = unit(20, "lines"), 
                                barheight = unit(.55, "lines"))) +
  labs(caption="#TidyTuesday Week 51 | Data from Spotify and Genius",
       title="Which Spice Girl sang the most lines?",
       x="Track number",
       fill="Proportion of total lines by album track (%)")
       
# save
ggsave(p1, "2021_51.png", width=8, height=7, unit="in")