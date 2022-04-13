# 2022-04-12
# TidyTuesday week 15 Indoor Air Pollution
# Data from ourworldindata.org/indoor-air-pollution

library(tidyverse)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

font_add_google("Source Sans Pro")
f1 = "Source Sans Pro"

indoor_pollution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/indoor_pollution.csv') 

df = indoor_pollution %>% 
  janitor::clean_names() %>%
  rename(value=deaths_cause_all_causes_risk_household_air_pollution_from_solid_fuels_sex_both_age_age_standardized_percent) %>%
  filter(is.na(code)) %>%
  filter(grepl("Bank",entity) | entity %in% c("Middle East & North Africa","North America")) %>%
  mutate(lab = str_remove(entity," - World Bank region"),
         lab = str_remove(lab,"World Bank "),
         col = case_when(lab %in% c("Low Income","High Income","Upper Middle Income","Lower Middle Income")~"2", TRUE~"1")) 
         
 df %>%
  ggplot(aes(x=year, y=value, color=col, group=entity)) +
  geom_segment(
    data = tibble(y = seq(0, 15, by = 5), x1 = 1989, x2 = 2020),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "#ACBABD",
    size = .3
  ) +
  ggrepel::geom_text_repel(data = df %>% filter(year==max(year)),
                           aes(label=lab, color=col), 
                           direction="y", xlim=c(2019.5, NA), size=3.2, family=f1, fontface="bold",
                           segment.linetype="dotted", min.segment.length = .3, segment.color="grey70") +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits=c(0,15), expand=c(0.02,0.02), 
                     labels=scales::percent_format(scale=1, accuracy=1)) +
  scale_x_continuous(limits=c(1989, 2030), breaks=c(1990,2000,2010,2019), expand=c(0,0)) +
  scale_color_manual(values=c("#53B0AF","#A31414")) +
  cowplot::theme_minimal_vgrid(13) +
  theme(legend.position="none",
        text=element_text(family=f1),
        axis.title=element_blank(),
        plot.title.position = "plot",
        axis.line.y=element_blank(),
        panel.grid.major.x=element_line(color="#ACBABD", size=.3),
        plot.margin=margin(.5,.5,.5,.5, unit="cm"),
        plot.title=element_text(size=18,hjust=.5),
        plot.subtitle = element_markdown(size=11.5, hjust=.5, lineheight=1.2, margin=margin(b=10)),
        plot.caption = element_text(color="grey30", size=10, margin=margin(t=15))
        ) +
  labs(title="Indoor Air Pollution",
       subtitle="Share of deaths from any cause which are attributed to indoor air pollution<br>from 1990 to 2019, by World Bank <span style='color:#53B0AF'>**region**</span> and <span style='color:#A31414'>**income**</span> group",
       caption="#TidyTuesday week 15 | Data from Our World in Data")
       
ggsave("2022_15.png", bg="#fafafa")

# Indoor vs outdoor pollution (2019)
dfin = indoor_pollution %>% filter(!is.na(code)) %>% filter(year==max(year)) %>% rename(indoor=value)

dfout = read_csv("data/share-deaths-outdoor-pollution.csv",show_col_types = FALSE) %>%
  janitor::clean_names() %>% 
  filter(!is.na(code)) %>%
  filter(year==max(year)) %>% 
  rename(outdoor=4)
  
dfc = dfin %>% left_join(dfout) %>%
  pivot_longer(4:5) %>%
  mutate(region=countrycode(code, origin="iso3c", destination="region")) %>%
  filter(!is.na(region)) %>%
  group_by(entity) %>%
  mutate(diff=diff(value),
         col=case_when(diff>0~"Outdoor > Indoor", TRUE~"Indoor > Outdoor"))
         
font_add_google("Lato")
f2 = "Lato"

dfc %>%
  ggplot(aes(y=str_to_title(name), x=value, color=col)) +
  geom_point(alpha=.7, show.legend=F) +
  geom_line(aes(group=entity), alpha=.5, key_glyph="rect") +
  scale_x_continuous(labels=scales::percent_format(scale=1, accuracy=1)) +
  scale_color_manual(values=c("#E38E2D","#178080")) +
  scale_y_discrete(expand=c(0.02,0.02)) +
  facet_wrap(~region, ncol=2) +
  cowplot::theme_minimal_grid(13) +
  theme(legend.position = c(0.6,0.1),
        axis.title=element_blank(),
        panel.spacing = unit(1.8, "lines"),
        strip.text=element_text(face="bold"),
        text=element_text(family=f2),
        plot.title.position = "plot",
        legend.title=element_blank(),
        plot.margin=margin(.5,.5,.5,.5, unit="cm"),
        plot.subtitle=element_text(margin=margin(b=10)),
        plot.caption=element_text(size=9)
        ) +
  guides(color=guide_legend(override.aes = list(size=5, alpha=.9))) +
  labs(subtitle="Share of deaths from any cause which are attributed to indoor and outdoor air pollution in 2019.",
       title="Air pollution",
       caption="TidyTuesday week 15 | Data from Our World in Data")
       
ggsave("2022_15_2.png", bg="#fafafa")                  