# 2022-03-15
# TidyTuesday week 11, CRAN/BIOC Vignettes
# Data from Robert Flight GitHub

library(tidyverse)
library(ggtext)
library(MetBrewer)
library(showtext)
font_add_google("Lato")
font_add_google("Libre Franklin")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')

# Wrangle
cran2 = cran %>% 
  mutate(date2=as.Date(date, "%Y-%m-%d")) %>%
  filter(!is.na(date2)) %>%
  filter(date>2016, date<2021) %>%
  count(date2) %>%
  mutate(yr = year(date2),
         yd = yday(date2)) 

cran3 = cran2 %>% filter(n==max(n))

# Plot
cran2 %>% 
  ggplot(aes(x=date2, y=n, color=fct_rev(factor(yr)))) +
  geom_line(show.legend=F, alpha=.95)  +
  geom_richtext(data=cran3, aes(x=as.Date("2019-09-15"), 
                                label="**123 packages** were<br>uploaded/updated<br>on **Jan 7, 2020**"), 
                size=3.5, hjust=1, vjust=0.9, family="Lato",lineheight=1.4,
                label.padding = grid::unit(rep(0, 4), "pt"), fill = NA, label.color = NA) +
  scale_color_manual(values=met.brewer("Renoir", 5)) +
  scale_x_date(breaks="1 year", labels=scales::date_format("%Y"), expand=c(0.02,0.02)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,125)) +
  cowplot::theme_minimal_hgrid(10,line_size = 0.3) +
  theme(plot.margin=margin(.5,1,.5,.5, unit="cm"),
        text=element_text(family="Libre Franklin"),
        plot.subtitle=element_text(size=11.5, margin=margin(t=4, b=8), hjust=.5, face="bold"),
        plot.title=element_text(size=15, hjust=.5),
        axis.title=element_blank(),
        axis.line.x = element_line(color="grey50"),
        axis.ticks.x=element_line(color="grey50"),
        axis.ticks.length.x=unit(.2, "cm"),
        plot.caption=element_text(size=8.5),
        legend.position="none",
        plot.title.position = "plot") +
  labs(title="Number of uploads/updates on CRAN by date",
       subtitle="2016 to 2021\n",
       caption="\n#TidyTuesday Week 11  |  Data from Robert Flight GitHub") +
  annotate(geom="segment", x=as.Date("2019-09-25"), xend=as.Date("2019-12-25"), y=123, yend=123,
           color="#14114E", arrow=arrow(length = unit(.2,"cm")), size=.4)
           
# Save plot
ggsave("2022_11.png", height=6.4, width=8, bg="white")