# 2022-05-03
# TidyTuesday week 18 Solar/Wind utilities
# Data from Berkeley Lab

# Libraries
library(tidyverse)
library(ggalluvial)
library(patchwork)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Barlow")
f1 = "Barlow"


# Data
capacity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')
wind <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv')
solar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv')
average_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/average_cost.csv')

# theme
theme1 = cowplot::theme_minimal_grid(12) +
  theme(legend.position = "top",
        text=element_text(family=f1),
        legend.justification = "center",
        plot.title.position = "plot",
        plot.title=element_text(hjust=.5, size=rel(1.55), margin=margin(b=10)),
        plot.margin=margin(.2,.3,.2,.2, unit="cm"),
        panel.spacing = unit(1.5, "lines"),
        strip.text=element_text(face="bold", size=rel(1)),
        axis.title=element_blank())
        
# capacity
p1 = capacity %>%
  rename("Standalone prior"=standalone_prior,
         "Hybrid prior" =hybrid_prior,
         "Standalone new" =standalone_new,
         "Hybrid new" =hybrid_new,) %>%
  pivot_longer(3:6) %>%
  filter(type=="Solar" | type=="Wind") %>%
  ggplot(aes(x=year, y=value, alluvium=name)) +
  geom_alluvium(aes(fill=fct_rev(name), color=fct_rev(name)), 
                alpha=.75, decreasing=FALSE) +
  scale_fill_manual("Capacity (gigawatts)",values=c("#204AD4","#008F91","#D5C44F","#C92828")) +
  scale_color_manual("Capacity (gigawatts)",values=c("#204AD4","#008F91","#D5C44F","#C92828")) +
  facet_wrap(~type) +
  theme1 +
  labs(title="US Solar/Wind")
  
# projected
s1 = solar %>% rename("Solar projected price in $/MWh"=solar_mwh, "Solar projected capacity in Gigawatts"=solar_capacity) %>% pivot_longer(2:3) %>% mutate(grp="Solar")
w1 = wind %>% rename("Wind projected price in $/MWh"=wind_mwh, "Wind projected capacity in Gigawatts"=wind_capacity) %>% pivot_longer(2:3) %>% mutate(grp="Wind") 
df1 = rbind(s1,w1)

p2 =df1 %>%
  ggplot(aes(x=date, y=value, color=name, fill=name)) +
  geom_point(shape=21, size=.7, key_glyph = draw_key_rect, alpha=.75) +
  geom_smooth(show.legend=F) +
  scale_color_manual("Projected",values=c("#204AD4","#D5C44F","#008F91","#C92828")) +
  scale_fill_manual("Projected",values=c("#204AD4","#D5C44F","#008F91","#C92828")) +
  scale_x_date(expand=c(0,0)) +
  facet_wrap(~grp, ncol=2) +
  theme1 +
  guides(color=guide_legend(nrow=2)) +
  labs(caption="\nTidyTuesday week 18  |  Source: Berkeley Lab")
  
# combine plots
p1/p2
ggsave("2022_18_2.png", width=8, height=8)
