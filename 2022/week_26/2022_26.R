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
  scale_fill_manual(values=c("#DB9124","#37363A","#0A7C6E"),labels=c("Percentage of female employees\npaid a bonus is less than male","Same","Percentage of female employees\npaid a bonus is more than male")) + #colors from MetBrewer
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

# p2: gap in top hourly pay quarter, 2022
## plot inspired by Cédric Scherer @CedScherer (https://twitter.com/CedScherer/status/1539227456703541248)

d2 = paygap %>% 
  mutate(year = year(as_date(date_submitted))) %>%
  filter(year==2022) %>%
  group_by(employer_size) %>%
  mutate(employer_size=case_when(employer_size=="1000 to 4999"~"1,000 to 4,999",
                                 employer_size=="5000 to 19,999"~"5,000 to 19,999",
                                 TRUE~employer_size),
         lab=glue::glue("{employer_size}<br>(n={scales::comma(n())})"),
         lab = case_when(lab=="Less than 250<br>(n=522)"~glue::glue("Company employees:<br>{lab}"), TRUE~lab),
         lab=factor(lab, levels=rev(c("Company employees:<br>Less than 250<br>(n=522)","250 to 499<br>(n=4,293)","500 to 999<br>(n=2,485)","1,000 to 4,999<br>(n=2,117)","5,000 to 19,999<br>(n=433)","20,000 or more<br>(n=57)","Not Provided<br>(n=207)"))) )

d3 = d2 %>% group_by(lab) %>%
  summarise(male_top_quartile = median(male_top_quartile, na.rm=TRUE),
            female_top_quartile = median(female_top_quartile,na.rm=TRUE))
            
d2 %>%
  ggplot(aes(x=male_top_quartile, y=lab)) +
  #geom_violin(aes(color = factor(employer_size), fill = after_scale(colorspace::lighten(color, .7))),bw = .3, show.legend=FALSE) +
  ggdist::stat_halfeye(aes(x=male_top_quartile, y=lab, color = "male", fill = after_scale(colorspace::lighten(color, .3))),adjust = .2, .width = 0,position = position_nudge(y = .2), slab.alpha=.4, point.alpha=1) +
  ggdist::stat_halfeye(aes(x=female_top_quartile, y=lab, color = "female", fill = after_scale(colorspace::lighten(color, .3))),adjust = .2, .width = 0,position = position_nudge(y = .2), slab_alpha=.4, point.alpha=1) +
  ggtext::geom_richtext(data=d2 %>% count(employer_size, lab) %>% filter(lab!="Company employees:<br>Less than 250<br>(n=522)"), aes(x=100, label=lab), size=3.2, hjust=0, vjust=-.7,fill = NA, label.color = NA, family=f1) +
  ggtext::geom_richtext(data=d2 %>% count(employer_size, lab) %>% filter(lab=="Company employees:<br>Less than 250<br>(n=522)"), aes(x=100, label=lab), size=3.2, hjust=0, vjust=-.5,fill = NA, label.color = NA, family=f1) +
  scale_y_discrete(expand=c(0.01,0.01)) +
  geom_text(data=d3, aes(x=female_top_quartile, y=lab, label=female_top_quartile), size=3, nudge_y = .32, color="white", family=f1) +
  geom_text(data=d3 %>% filter(lab=="5,000 to 19,999<br>(n=433)"), aes(x=female_top_quartile, y=lab, label="Median rate: "), size=3, nudge_y = .32, color="white", family=f1, hjust=1.2) +
  geom_text(data=d3, aes(x=male_top_quartile, y=lab, label=male_top_quartile), size=3, nudge_y = .32, color="white", family=f1) +
  scale_x_continuous(expand=c(0,0), limits=c(0,125), breaks=seq(0,100,25), labels=scales::percent_format(scale=1)) +
  scale_color_manual(values=c("#9F0000","#858DA0")) +
  coord_cartesian(clip="off") +
  cowplot::theme_minimal_grid(11) +
  theme(text=element_text(family=f1),
        axis.text.y=element_blank(),
        legend.position="none",
        axis.title=element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title=element_markdown(),
        plot.caption=element_text(hjust=0, size=8, lineheight = 1),
        plot.margin=margin(.5,.5,.5,.5,unit="cm")
        ) +
  labs(title="Percentage of <span style='color:#9F0000'>female</span> and <span style='color:#858DA0'>male</span> in the top hourly pay quarter, UK, 2022",
       caption="\nNote: Submissions from 2022-01-02 to 2022-06-27\nTidyTuesday week 26  •  Data from gender-pay-gap.service.gov.uk  •  Plot inspired by Cédric Scherer @CedScherer")
       
ggsave("2022_26_p2.png", height=7, width=7, bg="white")                   