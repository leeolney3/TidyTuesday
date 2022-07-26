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
font_add_google("Lato")
f2 = "Lato"

# 2022 round 10: FORMULA 1 LENOVO BRITISH GRAND PRIX 2022 
# https://www.formula1.com/en/results.html/2022/races/1114/great-britain/fastest-laps.html
drivers = load_drivers(season = 2022) %>% filter(code %in% c("HAM","SAI","PER","LEC","ALO","NOR","MSC","VER","STR","VET","RIC","MAG","LAT","OCO","TSU","GAS","BOT")) 

# function adapted from Meghan Hall @MeghanMHall https://twitter.com/MeghanMHall/status/1549183367769366537
fastestlaps <- function(dr) {
  get_driver_telemetry(2022, race = 10,  session="R",driver=dr,fastest_only = TRUE)
}
r10 <- purrr::map_df(unique(drivers$code), fastestlaps)

# Driver name and lap times
x2 = load_laps(season = "current", 10) %>%
  group_by(driverId) %>% filter(time==min(time)) %>%
  arrange(time) %>%
  left_join(drivers,by="driverId") %>%
  mutate(name_long=glue::glue("{givenName} {familyName}"))%>%
  mutate(lab=glue::glue("{name_long}<br><span style = 'font-size:7.5pt'>{time}</span>"),
         lab=fct_inorder(lab)) %>%
  select(code, lab)
  
# Plot
r10 %>% left_join(x2, by=c("driverCode"="code")) %>%
  ggplot(aes(X,Y, group=lab, color=factor(nGear))) +
  geom_path(size=1.8) +
  scale_color_viridis_d("Gear:",option="turbo") +
  scale_fill_viridis_d("Gear:",option="turbo") +
  facet_wrap(~factor(lab, levels=x2$lab), ncol=6) +
  coord_fixed() +
  theme_void() +
  theme(text=element_text(family=f2),
        legend.position = "top",
        legend.justification = "left",
        legend.title=element_text(size=9.5),
        legend.text=element_text(size=9),
        legend.margin=margin(b=5, t=7),
        plot.margin=margin(.4,.4,.3,.4,unit="cm"),
        plot.background = element_rect(fill="grey98", color=NA),
        plot.subtitle=element_text(size=9.5),
        plot.title=element_text(size=12, face="bold"),
        plot.caption = element_text(color="grey30",size=7.7,margin=margin(t=12)),
        panel.spacing = unit(.9, "lines"),
        strip.text=element_markdown(size=8.4, family=f1),
        ) +
  guides(color=guide_legend(nrow=1)) +
  labs(caption="#TidyTuesday week 30  •  Source: {f1dataR} package",
       title="FORMULA 1 LENOVO BRITISH GRAND PRIX 2022",
       subtitle="Fastest lap by driver, arranged in ascending order of fastest lap time")
       
ggsave("2022_30.png", height=7, width=6.8)        