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

# bump/alluvial chart
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

# Table of 20 family and genus with the most species described
## updated big.mark="" (thank you @geokaramanis for pointing out separators)
library(gt)
library(gtExtras)

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
  
  
# Table of 10 regions with most species described

reg = spiders %>% count(distribution, sort=T) %>%
  slice(1:11) %>% 
  mutate(distribution = case_when(str_detect(distribution, "Australia")~"Australia",
                            TRUE~distribution)) %>% 
  distinct(distribution) %>%
  pull(distribution)
  
ft = function(region) {
  spiders %>% filter(grepl(region, distribution)) %>%
    summarise(speciesId=n_distinct(speciesId),
              species= n_distinct(species),
              genus = n_distinct(genus),
              family=n_distinct(family),
              year = list(year),
            .groups="drop") %>%
    mutate(region_name=region)
}

region_df = bind_rows(ft(reg[1]),ft(reg[2]),ft(reg[3]),ft(reg[4]),ft(reg[5]),
          ft(reg[6]),ft(reg[7]),ft(reg[8]),ft(reg[9]),ft(reg[10])
          ) %>%
  arrange(desc(speciesId)) %>%
  mutate(rank = row_number()) %>%
  mutate(region = case_when(rank==1~"https://hatscripts.github.io/circle-flags/flags/cn.svg",
                          rank==2~"https://hatscripts.github.io/circle-flags/flags/au.svg",
                          rank==3~"https://hatscripts.github.io/circle-flags/flags/br.svg",
                          rank==4~"https://hatscripts.github.io/circle-flags/flags/us.svg",
                          rank==5~"https://hatscripts.github.io/circle-flags/flags/mx.svg",
                          rank==6~"https://hatscripts.github.io/circle-flags/flags/in.svg",
                          rank==7~"https://hatscripts.github.io/circle-flags/flags/za.svg",
                          rank==8~"https://hatscripts.github.io/circle-flags/flags/jp.svg",
                          rank==9~"https://hatscripts.github.io/circle-flags/flags/nz.svg",
                          rank==10~"https://hatscripts.github.io/circle-flags/flags/mg.svg",
                          )) %>%
  select(region, region_name, 1:5)
  
summary(region_df %>% select(3:6))

gt(region_df) %>%
  gt_theme_538() %>%
  gt_sparkline(year, type="histogram", bw=.75, same_limit = TRUE, width = 40, line_color = "#1e6091") %>%
  gt_img_rows(region) %>%
  gt_color_box(columns=speciesId, domain=755:5192) %>%
  gt_color_box(columns=species, domain=610:3864) %>%
  gt_color_box(columns=genus, domain=239:817) %>%
  gt_color_box(columns=family, domain=52:78) %>%
  cols_label(
    region_name="",
    year="year histogram"
  ) %>%
  cols_width(region_name~px(120),
             3:6 ~px(90)) %>%
  cols_align(column = 3:6, align ="center") %>%
  tab_header(
    title=md("10 regions with the most currently valid spider species described"),
    subtitle=md("Count of species id, species, genus and family, from 1757 to 2021, according to World Spider Database.")
    ) %>%
  tab_source_note(source_note="#TidyTuesday Week 50 | Data: World Spider Database") %>%
  tab_options(source_notes.padding = px(14),
              source_notes.font.size = px(13))