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
  
babynames %>% 
  filter(name %in% selected)  %>%
  group_by(name, year) %>%
  tally(prop) %>%
  ggplot(aes(year, n)) +
  geom_col(aes(fill=n),show.legend = F) +
  facet_wrap(~name, ncol=3) +
  scale_y_continuous(labels=scales::percent, breaks=c(0.04,0.08)) +
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

# relative change
df2 = babynames %>% 
  filter(name %in% selected) %>%
  group_by(name,year) %>%
  tally(prop) %>%
  ungroup() %>%
  group_by(name) %>%
  mutate(relative = (n-lag(n))/lag(n))
  
df2 %>% 
  ggplot(aes(year, relative)) +
  geom_line(aes(color=relative)) +
  facet_wrap(~name, ncol=3) +
  scale_y_continuous(labels=scales::percent) +
  scico::scale_color_scico(palette="tofino", midpoint = 0, labels=scales::percent) +
  theme_minimal() +
  theme(text=element_text(family="Outfit"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_line(size=.3),
        plot.margin=margin(.5,.5,.5,.5, unit = "cm"),
        plot.title.position = "plot",
        plot.title=element_text(hjust=.5, size=16, margin=margin(b=3)),
        plot.subtitle=element_text(hjust=.5, size=10.5),
        panel.spacing.y = unit(1.3, "lines"),
        strip.text=element_text(hjust=0, size=10.5),
        axis.text=element_text(size=8.5),
        axis.title=element_blank(),
        legend.position="top",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust=0, lineheight = 1.2),
        axis.ticks.x=element_line(color="grey30")
        ) +
  labs(title="Names of newborns, relative change (1880-2017)",
       color="Relative change in the proportion of total births by year",
       #subtitle="15 most common names by proportion of total births in the US, arranged in alphabetical order",
       caption="\nNote: Includes 15 most common names by proportion of total births in the US, arranged in alphabetical order. \n#TidyTuesday week 12 | Data source: babynames R package from Hadley Wickham") +
  guides(color=guide_colorbar(title.position = "top", 
                              barwidth = unit(19, "lines"),
                              barheight = unit(.5, "lines")))
                              
ggsave("2022_12_p2.png", width=8, height=8, unit="in", bg="white")  

# gt table
library(gt)
library(gtExtras)

df3a = babynames %>% 
  filter(name %in% df3$name) %>%
  group_by(name, year) %>%
  tally(prop) %>%
  mutate(n=n*100)
  
df3max = df3a %>% group_by(name) %>%
  filter(n==max(n)) %>%
  select(name, max_year=year, max_prop=n) 
  
df3min = df3a %>% group_by(name) %>%
  filter(n==min(n)) %>%
  select(name, min_year=year, min_prop=n)     
  
df3c = df3a %>% filter(year==2017) %>%
  select(name, p17=n)

df3d = df3a %>%
  group_by(name) %>%
  summarise(mean_prop=mean(n),
            timeline=list(n),
            density=list(n)) %>%
  left_join(df3max, by="name") %>%
  left_join(df3min, by="name") %>%
  left_join(df3c, by="name") %>%
  arrange(desc(mean_prop)) %>%
  select(name, mean_prop, min_year, min_prop, max_year, max_prop, density, timeline, p17)
  
p3 = df3d %>%
  gt() %>%
  gt_theme_538() %>%
  opt_table_font(
      font = list(
        google_font("Roboto"),
        default_fonts()
      ),
      weight = 400
    ) %>%
  gt_sparkline(column="timeline", type="sparkline") %>%
  gt_sparkline(column="density", type="density") %>%
  tab_spanner(label="Min", columns=c("min_year","min_prop")) %>%
  tab_spanner(label="Max", columns=c("max_year","max_prop")) %>%
  cols_label(mean_prop=md("Mean %"),
             min_year="Year",
             min_prop="%",
             max_year="Year",
             max_prop="%",
             p17=md("% in 2017")) %>%
  fmt_number(column=c(mean_prop, min_prop, max_prop, p17),
             decimals=2) %>%
  tab_options(data_row.padding = px(7),
              column_labels.font.size = px(13)) %>%
  cols_width(3:6~px(55),
             2~px(65)) %>%
  cols_align(align="center", columns=c(2:6,9)) %>%
  gt_color_rows(columns="max_prop", palette="ggsci::green_material") %>%
  gt_color_rows(columns="p17", palette="ggsci::lime_material") %>%
  tab_header(title="Names of newborns (1880-2017)",
             subtitle="15 most common names by proportion of total births in the US, arranged in descending order of mean proportion.") %>%
  tab_source_note(source_note = "#TidyTuesday week 12 | Data source: babynames R package from Hadley Wickham") %>%
  tab_style(
      style = cell_text(weight = 550),
      locations = cells_title(groups = "title")) %>%
  tab_style(
      style = cell_text(size=14.5),
      locations = cells_body()) %>%
    tab_style(
      style = cell_text(
        color = "grey40",
        transform = "capitalize"
      ),
      locations = cells_column_labels(everything())
    ) %>%
  tab_style(
      style = cell_text(
        color = "grey40",
        size=13,
        transform = "capitalize"
      ),
      locations = cells_column_spanners(everything())
    )
    
p3                        