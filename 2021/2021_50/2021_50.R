#TidyTuesday Week 50 2021-12-07
#Spiders, data from World Spiders Database

# load libraries 
library(tidyverse)
library(wesanderson)
library(ggalluvial)

# load font
library(sysfonts)
library(showtext)
font_add_google("Libre Franklin")
showtext_auto()

# import data
spiders <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')

# bump/alluvial 
## wrangle
spiders1 = spiders %>% filter(year>=1980, year<2021)

ind = spiders1 %>%
  filter(grepl('India', distribution)) %>%
  count(year) %>% mutate(country="India")

usa = spiders1 %>%
  filter(grepl('USA', distribution)) %>%
  count(year) %>% mutate(country="USA")

sa = spiders1 %>%
  filter(grepl('South Africa', distribution)) %>%
  count(year) %>% mutate(country="South Africa")

br = spiders1 %>% 
  filter(grepl('Brazil', distribution)) %>%
  count(year) %>% mutate(country="Brazil")

spiders2 = bind_rows(ind,usa, sa, br)

## plot
spiders2 %>%
  ggplot(aes(x=year, y=n, alluvium=country)) +
  geom_alluvium(aes(fill=country, color=country),
                alpha=.75, decreasing=FALSE) +
  scale_fill_manual(values=wes_palette("BottleRocket2")) +
  scale_color_manual(values=wes_palette("BottleRocket2")) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0.01,0.01)) +
  cowplot::theme_minimal_hgrid(11.5) +
  theme(text=element_text(family="Libre Franklin"),
        legend.position = "top",
        legend.margin=margin(l=-39),
        plot.margin=margin(.75,1,.5,.75, unit="cm"),
        plot.background = element_rect(fill="#f8f9fa",color=NA),
        axis.title=element_text(face="bold", size=10),
        axis.text=element_text(size=8.7),
        plot.caption = element_text(margin=margin(t=8), size=8),
        plot.title.position = "plot",
        plot.title=element_text(size=12)
        ) +
  labs(title="Number of currently valid spider species described per year",
       subtitle="by distribution region from 1980 to 2020, according to World Spider Database",
       fill="", color="", x="Year", y="Species count",
       caption="#TidyTuesday Week 50 | Data: World Spider Database")

## updated plot: color change (thank you @bac3917 for the color contrast comment)
spiders2 %>%
  ggplot(aes(x=year, y=n, alluvium=country)) +
  geom_alluvium(aes(fill=country, color=country),
                alpha=.75, decreasing=FALSE) +
  scale_fill_manual(values=c("#FAD510", "#CB2314", "#3B9AB2", "#354823")) +
  scale_color_manual(values=c("#FAD510", "#CB2314", "#3B9AB2", "#354823")) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0.01,0.01)) +
  cowplot::theme_minimal_hgrid(11.5) +
  theme(text=element_text(family="Libre Franklin"),
        legend.position = "top",
        legend.margin=margin(l=-39),
        plot.margin=margin(.75,1,.5,.75, unit="cm"),
        plot.background = element_rect(fill="#f8f9fa",color=NA),
        axis.title=element_text(face="bold", size=10),
        axis.text=element_text(size=8.7),
        plot.caption = element_text(margin=margin(t=8), size=8),
        plot.title.position = "plot",
        plot.title=element_text(size=12)
        ) +
  labs(title="Number of currently valid spider species described per year",
       subtitle="by distribution region from 1980 to 2020, according to World Spider Database",
       fill="", color="", x="Year", y="Species count",
       caption="#TidyTuesday Week 50 | Data: World Spider Database")

# gt 
library(gt)
library(gtExtras)

## wrangle
fg = spiders %>% group_by(family, genus) %>%
  summarise(n1 = n_distinct(species),
            min_year = min(year),
            max_year = max(year),
            density = list(year),
            .groups="drop"
            ) %>%
  ungroup() %>%
  arrange(desc(n1)) %>%
  slice(1:20) %>%
  mutate(n2 = n1,
         histogram = density) %>%
  select(family,genus,n1,n2, min_year, max_year, density, histogram)
  
range(fg$max_year)
range(fg$min_year)

## table 
## updated big.mark="" (thank you @geokaramanis for pointing out separators)
gt(fg) %>%
  gt_theme_538() %>%
  gt_plt_bar(column = n2, width=40, color="#606c38") %>%
  gt_sparkline(density, type="density", bw=.75, same_limit = TRUE, width = 30,) %>%
  gt_sparkline(histogram, type="histogram", bw=.75, same_limit = TRUE, width = 30,) %>%
  gt_color_box(columns=min_year, domain=1757:1936, palette = "wesanderson::Zissou1",
               big.mark = "") %>%
  gt_color_box(columns=max_year, domain=2015:2021, palette = "wesanderson::Zissou1",
               big.mark = "") %>%
  tab_spanner(
    label = "Year",
    columns = min_year:histogram
  ) %>%
  cols_label(
    n1 = "",
    n2 = "species count",
    min_year= "min",
    max_year= "max"
  ) %>%
  cols_align(column = 5:6, align ="center") %>%
  tab_header(
    title=md("**Spiders: 20 family and genus with the most species described**"),
    subtitle=md("Total number of currently valid spider species described from 1757 to 2021, according to World Spider Database.")
  ) %>%
  tab_source_note(source_note="#TidyTuesday Week 50 | Data: World Spider Database") %>%
  tab_options(source_notes.padding = px(14))