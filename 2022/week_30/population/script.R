# 2022-07-26
# TidyTuesday week 30 Bring your own data
# Data source: United Nations [World Population Prospects 2022](https://population.un.org/wpp/Download/Standard/CSV/)

# Load libraries
library(tidyverse)
options(dplyr.summarise.inform = FALSE)
library(ggtext)
library(ggshadow)
library(ggnewscale)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Import data
pop = readr::read_csv("data/WPP2022_Demographic_Indicators_Medium.csv")

# Recreating The Economist's chart on annual change in population in The pecking order of the world’s population is soon to change (14 July 2022), https://blog.datawrapper.de/wp-content/uploads/2022/07/image6-2.png

## p1 data
df1 = pop %>% filter(!is.na(ISO3_code)) %>%
  mutate(grp=case_when(Location %in% c("India","Nigeria","Ethiopia","Pakistan","Democratic Republic of the Congo")~1,
                       Location %in% c("China","United States of America","Brazil","Japan")~2,TRUE~3)) %>%
  mutate(PopChange=PopChange/1000)

lab1 = tibble(x=c(1960,1980,1994,2013,2020.5,2024),
       y=c(-1.8,11,-3.8,-3,8,-6.5),
       label=c("Great Leap<br>Forward","One-child<br>policy begins","Rwandan<br>genocide","Syrian<br>civil war","Covid-19<br>pandemic","War in<br>Ukraine")
)

lab2 = tibble(
  x=c(1957.5,2043,2070,2083),
  y=c(17.5,-1.4,1.4,-2),
  label=c("China","Japan","US","Brazil")
)

lab3 = tibble(
  x=c(2000,2031,2038,2059,2100),
  y=c(17.5,5.9,3.2,3.8,4.2),
  label=c("India","Nigeria","Ethiopia","Pakistan","Congo")
)

## Fonts
font_add_google("Fira Sans")
font_add_google("Fira Sans Condensed")
f1 = "Fira Sans"
f2 = "Fira Sans Condensed"

## Plot
p1 = df1 %>% filter(grp==3) %>%
ggplot(aes(Time,PopChange,group=Location)) +
  #y-axis
  geom_segment(data=tibble(x=1942, xend=2111, y=seq(-15,25,5)),aes(x=x, xend=xend, y=y, yend=y),inherit.aes = FALSE, size=.35, color=c("#514D49",rep("#cdcccc",2),"#514D49",rep("#cdcccc",5))) +
  geom_text(data=tibble(x=2111, y=seq(-15,25,5)), aes(x=x, y=y+.7, label=y), inherit.aes = FALSE, size=3, hjust=1, color="#464645", family=f1) +
  # estimate forecast line
  annotate(geom="segment", x=2022, xend=2022, y=14, yend=25, linetype="dotted", color="#464645") +
  annotate(geom="segment", x=2022, xend=2022, y=-8, yend=-15, linetype="dotted", color="#464645") +
  annotate(geom="text",x=c(2023.5,2020.5), y=c(24,24), label=c("Forecast →","← Estimate"), hjust=c(0,1), color="#464645", size=3.5, family=f2) +
  #lines
  geom_line(color="#DCDBD7") +
  ggshadow::geom_shadowline(data=df1 %>% filter(grp==2), aes(color="Other countries"), size=.65, shadowsize=1.5) +
  scale_color_manual(values="#999789", labels="<span style='color:#777567'>Other countries</span>") +
  ggnewscale::new_scale_color() + #to create space between legend key/text
  ggshadow::geom_shadowline(data=df1 %>% filter(grp==1), aes(color="Top five countries contributing to population growth"), size=.65, shadowsize=1.5) +
  scale_color_manual(values="#EF563A", labels="<span style='color:#EF563A'>Top five countries<br>contributing to<br>population growth</span>") +
  guides(color=guide_legend(reverse=TRUE)) +
  scale_x_continuous(breaks=seq(1950,2100,10),labels=c("1950","60","70","80","90","2000","10","20","30","40","50","60","70","80","90","2100")) +
  coord_cartesian(ylim=c(-15,25), expand=FALSE,clip="off") +
  cowplot::theme_minimal_grid(10) +
  theme(text=element_text(family=f1),
        axis.title=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(color="#464645"),
        axis.ticks.length = unit(.3,"lines"),
        axis.ticks.x=element_line(color="#464645", size=.35),
        axis.ticks.y=element_blank(),
        legend.position = c(.65,.75),
        legend.title=element_blank(),
        legend.text=element_markdown(lineheight = 1.2, size=9, face="bold"),
        legend.spacing.y = unit(.2,"lines"),
        legend.box.background = element_rect(fill="white",color=NA),
        panel.grid=element_blank(),
        plot.margin=margin(.7,.8,.3,.6,"cm"),
        plot.title=element_text(color="#322D28",size=10.7),
        plot.subtitle = element_markdown(lineheight=1.3, color="#322D28", margin=margin(b=10)),
        plot.caption = element_text(color="grey30",margin=margin(t=8))
        ) +
  geom_richtext(data=lab1, aes(x=x, y=y, label=label), inherit.aes = FALSE, family=f2, size=3, lineheight=1,hjust=c(rep(.5,4),rep(0,2)), color="#464645", label.padding = grid::unit(rep(0, 4), "pt"),label.color = NA,fill="white") +
  geom_text(data=lab2, aes(x=x, y=y, label=label), inherit.aes = FALSE, family=f1, size=3, color="#7F7E6F") +
  geom_richtext(data=lab3, aes(x=x, y=y, label=label), inherit.aes = FALSE, family=f1, size=2.9, color="#EF563A",label.padding = grid::unit(rep(0, 4), "pt"),label.color = NA,fill="white", hjust=c(rep(.5,4),1)) +
  labs(title="Annual change in population, m",
       caption="#TidyTuesday week 30 Bring Your Own Data",
       subtitle="Recreation of The Economist's chart in *The pecking order of the world’s population is soon to change*, July 14<br>Data source: United Nations World Population Prospects 2022")
       
## Add line and save plot
png("p1.png", width=7.5, height=6.5,unit='in',res=300)

#function reference: https://stackoverflow.com/questions/64656234/how-does-the-economist-make-these-lines-near-the-title-using-using-ggplot
annotate_npc <- function(x, y, height, width, ...) {
  grid::grid.draw(grid::rectGrob(
    x = unit(x, "npc"), y = unit(y, "npc"), height = unit(height, "npc"), width = unit(width, "npc"),
    gp = grid::gpar(...)
  ))
}

p1
annotate_npc(x = 0.055, y = 0.967, height = 0.002, width = 0.03, fill = "black", col = NA)
dev.off()