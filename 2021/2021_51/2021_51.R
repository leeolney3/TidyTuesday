# TidyTuesday week 51 2021-12-14 
# Spice Girls, data from Spotify and Genius

library(tidyverse)
options(dplyr.summarise.inform = FALSE)
library(ggtext)
library(MetBrewer)

library(showtext)
font_add_google("Oswald", "oswald")
font_add_google("Roboto Condensed", "roboto condensed")
font_add_google("Fira Sans Condensed")
font_add_google("Fira Sans")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

tuesdata <- tidytuesdayR::tt_load('2021-12-14')
lyrics = tuesdata$lyrics
studio_album_tracks = tuesdata$studio_album_tracks
related_artists = tuesdata$related_artists

# Section One -------------------------------------------------------------

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

artist_df = bind_rows(fa("all"),fa("scary"),fa("sporty"),fa("baby"),fa("ginger"),fa("posh") %>%
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


# tile plot
p1 = df1 %>% 
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
  theme_minimal(base_family="roboto condensed", base_size = 13) +
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
        ) +
  guides(fill = guide_colorbar(title.position = "top", 
                                barwidth = unit(20, "lines"), 
                                barheight = unit(.55, "lines"))) +
  labs(caption="#TidyTuesday Week 51 | Data from Spotify and Genius",
       title="Which Spice Girl sang the most lines?",
       x="Track number",
       fill="Proportion of total lines by album track (%)")
       
# save
ggsave("2021_51.png", height=7, width=8, unit="in")


# Section Two -------------------------------------------------------------

# add track names to lyrics df
track_df = studio_album_tracks %>% 
  select(album_name, track_name, track_number) %>%
  mutate(track_name= fct_inorder(track_name))
  
df2 = artist_df %>% 
  filter(album_name!="Forever") %>%
  mutate(track_number= as.numeric(track_number)) %>%
  left_join(track_df, by=c("album_name","track_number"))  %>%
  mutate(album_name = case_when(album_name=="Spiceworld" ~ "*Spiceworld Album (1997)*",
                                album_name=="Spice" ~ "*Spice Album (1996)*"))

# dot plot
f2 = "Fira Sans Condensed"
f3 = "Fira Sans"   

p2 = df2 %>% 
  ggplot(aes(x=str_to_title(artist), y=fct_rev(track_name), color=artist)) +
  geom_line(aes(group=artist), show.legend = F) +
  geom_point(aes(size=prop/100)) +
  geom_text(data = df2 %>% filter(prop>50), aes(label=round(prop,1)), 
            color="white", size=3, family=f2) +
  facet_wrap(~album_name, ncol=2, scales="free_y") +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 18)) +
  scale_color_manual(values=met.brewer(name="Veronese")) +
  scale_x_discrete(position="top") +
  scale_size_area(max_size = 13, labels=scales::percent) +
  cowplot::theme_minimal_hgrid(10) +
  guides(color="none") +
  theme(strip.placement = "outside",
        text=element_text(family=f2),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin=margin(r=5), size=9),
        axis.text.x.top=element_text(size=10),
        axis.text.y=element_text(margin=margin(r=0)),
        strip.text.x.top=element_markdown(size=12, family=f3, color="grey40", margin=margin(b=3)),
        plot.margin=margin(.5,.75,.5,.5, unit="cm"),
        plot.title.position = "plot",
        plot.title = element_markdown(size=14, family=f3, face="plain"),
        legend.position = "top",
        legend.margin=margin(l=-84, b=-2),
        legend.title=element_text(size=10.5, family=f3),
        legend.text=element_text(size=10.5,family=f3),
        plot.caption=element_text(size=8.5,color="grey30"),
        panel.grid.major.y = element_line(size=.4),
        panel.spacing.x = unit(1.2, "lines")
        )  +
  labs(y="Track name (ascending album track order)",
       title="**Spice Girls** lines in Spice and Spiceworld albums",
       caption="\n#TidyTuesday Week 51 | Data from Spotify and Genius",
       size="Proportion of total lines by album track")
       
# save
ggsave("2021_51_p2.png", height=6, width=8, unit="in", bg="#fafafa")                          

# Section Three -------------------------------------------------------------

# related artists followers and popularity 
rel = related_artists %>%
select(artist_name, popularity, followers_total) %>%
  unique() %>%
  mutate(artist_name = fct_reorder(artist_name, popularity),
         fol_lab = scales::number(followers_total, scale = 1e-3, suffix="k", big.mark = "")) 

rel %>% 
  ggplot(aes(popularity, artist_name, color=I(ifelse(artist_name=="Spice Girls","#E3120B","#333333")))) +
  geom_segment(aes(x=0, xend = popularity, y=artist_name, yend=artist_name)) +
  geom_point(aes(size=followers_total),show.legend = F) +
  geom_richtext(data=rel %>% filter(artist_name=="Spice Girls"), 
                aes(label=glue::glue("{fol_lab}<br> followers")),
                size=2.9, nudge_x=4.5, fill = NA, family=f3, 
                label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_richtext(data=rel %>% filter(artist_name!="Spice Girls"), 
                aes(label=fol_lab),
                size=2.9, nudge_x=3.5, fill = NA, family=f3,
                label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_text(aes(x=0, label=case_when(artist_name=="Spice Girls"~glue::glue("{artist_name}: {popularity} popularity"),TRUE~glue::glue("{artist_name}: {popularity}"))),
            size=3, nudge_y = 0.35, hjust=0, nudge_x = .5, family=f3) +
  geom_vline(xintercept=0, color="#1A1A1A") +
  scale_x_continuous(limits=c(0,80), expand=c(0,.0)) +
  scale_y_discrete(expand = expansion(mult = c(.001, .035))) +
  scale_size_area(max_size = 8) +
  scale_color_identity() +
  coord_cartesian(clip="off") +
  theme_void(base_family = f3) +
  theme(plot.margin=margin(1,1.5,1,1, unit="cm"),
        plot.title=element_markdown(margin=margin(b=13), color="#333333", size=12),
        plot.caption=element_text(size=8, color="grey30")
  ) +
  labs(title ="<span style = 'color:#E3120B;'>**Spice Girls**</span> popularity and followers compared to **related artists**",
       caption="\n#TidyTuesday Week 51 | Data from Spotify and Genius")

ggsave("2021_51_p3.png", height=6, width = 8, unit="in", bg="#fafafa")
