# 2022-07-26
# Data from Santiago Casanova's {f1dataR} package

# Load libaries
library(f1dataR) # https://scasanova.github.io/f1dataR/
library(tidyverse)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load fonts
font_add_google("Barlow")
f1 = "Barlow"

# 2022 Austrian Grand Prix data
#code from: Meghan Hall @MeghanMHall https://twitter.com/MeghanMHall/status/1549183367769366537
drivers <- load_drivers(season = 2022) %>% 
  filter(code != "HUL")
fastestlaps <- function(dr) {
  get_driver_telemetry(2022, 10, driver = dr, fastest_only = TRUE)
}
austria <- purrr::map_df(unique(drivers$code), fastestlaps)

# Driver name and lap times
x = load_laps(season = "current", 11)
x2 = x %>% group_by(driverId) %>% filter(time==min(time)) %>%
  arrange(time_sec) %>%
  left_join(drivers, by="driverId") %>%
  mutate(name_long=glue::glue("{givenName} {familyName}"))%>%
  mutate(lab=glue::glue("{name_long}<br><span style = 'font-size:8pt'>{time_sec} sec</span>"),
         lab=fct_inorder(lab)) %>%
  select(code, lab)
  
# Plot
austria %>% left_join(x2, by=c("driverCode"="code")) %>%
  ggplot(aes(X,Y, group=lab, color=factor(nGear))) +
  geom_path(size=1.8) +
  scale_color_viridis_d("Gear:",option="turbo") +
  scale_fill_viridis_d("Gear:",option="turbo") +
  facet_wrap(~factor(lab, levels=x2$lab), ncol=4) +
  coord_fixed() +
  theme_void() +
  theme(text=element_text(family=f1),
        legend.position = "top",
        legend.justification = "left",
        legend.title=element_text(size=10),
        legend.text=element_text(size=9.5),
        legend.margin=margin(b=5),
        plot.margin=margin(.4,.4,.3,.4,unit="cm"),
        plot.background = element_rect(fill="grey98", color=NA),
        plot.subtitle=element_text(size=10),
        plot.title=element_text(size=13, face="bold"),
        plot.caption = element_text(color="grey30",size=8,margin=margin(t=12)),
        panel.spacing = unit(.9, "lines"),
        strip.text=element_markdown(size=9),
        ) +
  guides(color=guide_legend(nrow=1)) +
  labs(caption="#TidyTuesday week 30 BYOD •  Source: {f1dataR} package",
       title="2022 Austrian Grand Prix",
       subtitle="Fastest lap by driver") 
       
ggsave("2022_30.png", height=7.5, width=6.5)        