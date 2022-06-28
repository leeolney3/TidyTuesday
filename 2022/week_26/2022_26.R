# 2022-06-28
# TidyTuesday week 26, data from gender-pay-gap.service.gov.uk

# Load libraries
library(tidyverse)
library(lubridate)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Import fonts
font_add_google("Libre Franklin")
f2 = "Libre Franklin"

# Import data
paygap <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv')

# p1: Bonus gap, percentage of reports by year submitted
# ggforce::geom_circle method from Abdoul Madjid @issa_madjid (https://twitter.com/issa_madjid/status/1526685250994286592/photo/1)

d1 = paygap %>%
  mutate(year = year(as_date(date_submitted))) %>%
  mutate(grp = case_when(female_bonus_percent==male_bonus_percent~"=",
                         female_bonus_percent>male_bonus_percent~">",
                         female_bonus_percent<male_bonus_percent~"<")) %>%
  count(year, grp) %>%
  group_by(year) %>%
  mutate(prop=round_percent(n)) %>%
  select(-n) %>%
  ungroup()
  
d1 %>% rename(n=prop) %>%
  group_by(year) %>% 
  mutate(indice_start = lag(cumsum(n))) %>% 
  replace_na(list(indice_start = 0)) %>% 
  rowwise() %>% 
  mutate(indice = list(indice_start:(indice_start + n -1))) %>% 
  unnest_longer(indice) %>% 
  select(-c(n, indice_start)) %>% 
  mutate(
    xind = indice %% 10,
    yind = indice %/% 10) %>%
  ggplot() + 
  ggforce::geom_circle(aes(x0 = xind, y0=yind, r = .48, fill = grp), alpha=.9,size=.2, key_glyph=draw_key_dotplot) +
  geom_text(aes(x=xind, y=yind, label=grp), color="white", size=3.5, fontface="bold") +
  facet_wrap(~year) +
  coord_equal(expand=FALSE, clip="off") +
  scale_fill_manual(values=c("#DB9124","#37363A","#0A7C6E"),labels=c("Percentage of female employees\npaid a bonus is less than male","Same","Percentage of female employees\npaid a bonus is more than than male")) + #colors from MetBrewer
  theme_void(10) +
  theme(text=element_text(family=f2),
        legend.position="top",
        legend.key.size = unit(.9,"cm"),
        legend.justification = "left",
        legend.title=element_blank(),
        legend.text=element_text(size=8.5, lineheight = 1),
        legend.margin=margin(t=3),
        panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1, "lines"),
        strip.text=element_text(face="bold", margin=margin(b=5), size=10),
        plot.title=element_text(face="bold"),
        plot.subtitle = element_text(size=9),
        plot.caption=element_text(hjust=0, color="grey20", size=7.2),
        plot.margin=margin(.3,.4,.3,.4,unit="cm")
        ) +
  labs(title=("UK Gender Bonus Pay Gap"),
       subtitle="Porportion of submissions by year",
       caption="\nTidyTuesday week 26  |  Data from gender-pay-gap.service.gov.uk  |  Method from Abdoul Madjid @issa_madjid")

ggsave("2022_26_p1.png", height=5.5, width=6.5, bg="#f7f7f7")
