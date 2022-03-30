# 2022-03-29
# Week 13 Collegiate Sports Budgets
# Data from Equity in Athletics Data Analysis, hattip to Data is Plural.

library(tidyverse)
library(ggtext)
library(ggdist)

library(tidyverse)
library(showtext)
font_add_google("Outfit")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

## Profitability of college football and other sports
# Prepare data
df = sports %>%
  filter(!str_detect(sports, 'Track and Field') )  %>%
  mutate(profit=total_rev_menwomen-total_exp_menwomen,
         sum_partic = sum_partic_men+sum_partic_women,
         col= case_when(sports=="Football"~"Football", TRUE~"Other Sports")
         )

# Plot         
df %>%
  ggplot(aes(fct_rev(col), profit)) +
  stat_interval(.width=c(.95,1), size=3, alpha=.8) +
  scale_color_manual(name="Level:",values=c("#B0B6BA","black"),
                     labels = function(x) paste0(as.numeric(x)*100, "%")) +
  ggnewscale::new_scale_color() +
  geom_point(aes(color=col),shape=21, size=2, position=position_nudge(x=-0.3, y=0), show.legend = F) +
  scale_color_manual(name="Group:",values=alpha(c("#F28F16","#496c4c"),.8)) +
  scale_y_continuous(labels=scales::label_number(suffix="M", scale=1e-6), expand=c(0.02,0.02)) +
  coord_flip() +
  facet_wrap(~year, ncol=1, strip.position="left") +
  theme_minimal() +
  theme(text=element_text(family="Outfit"),
        panel.grid=element_line(size=.3),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle=0, color="black", size=10),
        legend.position = "top",
        axis.title.y=element_blank(),
        axis.title.x=element_text(size=10),
        plot.margin=margin(.5,.5,.5,.5, unit="cm"),
        legend.justification = "left",
        legend.title=element_text(size=9),
        legend.text=element_text(size=9),
        legend.margin=margin(l=-88),
        plot.caption.position="plot",
        plot.caption=element_text(hjust=0, size=8.3, color="grey20"),
        plot.title.position="plot",
        plot.title=element_markdown(size=15),
        plot.subtitle = element_text(size=9),
        panel.grid.major.y=element_blank()
        ) +
  labs(y="Profit (in USD)",
       caption="\n#TidyTuesday week 13 | Data from Equity in Athletics Data Analysis",
       title="Profitability of college <span style='color:#F28F16'>football</span> and <span style='color:#496c4c'>other sports</span>",
       subtitle="From 2015 to 2019, each point represents one record of every sport per institution")

# Save plot       
ggsave("2022_13.png", bg="white")


## Add average point and annotation
df_lab = df %>% group_by(col, year) %>%
  summarise(profit = mean(profit, na.rm = T)) %>%
  ungroup() %>%
  mutate(mean_profit=case_when(year==2015 & col=="Football"~paste("Average:",scales::number(profit, accuracy=1, big.mark = ",", prefix="$")),TRUE~paste(scales::number(profit, accuracy=1, big.mark = ",", prefix="$"))))
  
df %>%
  ggplot(aes(fct_rev(col), profit)) +
  stat_interval(.width=c(.95,1), size=3, alpha=.8) +
  scale_color_manual(name="Level:",values=c("#B0B6BA","black"),
                     labels = function(x) paste0(as.numeric(x)*100, "%")) +
  ggnewscale::new_scale_color() +
  geom_point(aes(color=col),shape=21, size=2, position=position_nudge(x=-0.3, y=0), show.legend = F) +
  scale_color_manual(name="Group:",values=alpha(c("#176746","#7c5eae"),.8)) +
  stat_summary(geom="point", fun=mean, shape=18, size=3,position=position_nudge(x=-0.3, y=0), color="#ffff3f") +
  geom_text(data=df_lab, aes(label=mean_profit, color=col), show.legend = F,
            size=2.5, family="Outfit", position=position_nudge(x=-0.6, y=0), alpha=1) +
  scale_y_continuous(labels=scales::label_number(suffix="M", scale=1e-6), expand=c(0.02,0.02)) +
  scale_x_discrete(expand = expansion(mult = c(.8,0))) +
  coord_flip() +
  facet_wrap(~year, ncol=1, strip.position="left") +
  theme_minimal() +
  theme(text=element_text(family="Outfit"),
        panel.grid=element_line(size=.3),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle=0, color="black", size=10),
        legend.position = "top",
        axis.title.y=element_blank(),
        axis.title.x=element_text(size=10),
        plot.margin=margin(.5,.5,.5,.5, unit="cm"),
        legend.justification = "left",
        legend.title=element_text(size=9),
        legend.text=element_text(size=9),
        legend.margin=margin(l=-88, b=-7),
        plot.caption.position="plot",
        plot.caption=element_text(hjust=0, size=8.3, color="grey20"),
        plot.title.position="plot",
        plot.title=element_markdown(size=15),
        plot.subtitle = element_text(size=9),
        panel.grid.major.y=element_blank()
        ) +
  labs(y="Profit (in USD)",
       caption="\n#TidyTuesday week 13 | Data from Equity in Athletics Data Analysis",
       title="Profitability of college <span style='color:#176746'>football</span> and <span style='color:#7c5eae'>other sports</span>",
       subtitle="From 2015 to 2019, each point represents one record of every sport per institution")
       
ggsave("2022_13b.png", bg="white")

## Participation 
library(ggh4x)
font_add_google(font="Dosis",family="dosis")
font_add_google("Roboto")
font_add_google("Oswald")

sports %>% #filter(sports %in% selected) %>%
  filter(!str_detect(sports, 'Track and Field')) %>% 
  filter(!str_detect(sports, 'Hand')) %>%
  group_by(year, sports) %>%
  summarise(sum_partic_men=sum(sum_partic_men),
            sum_partic_women=sum(sum_partic_women)
            ) %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y=sum_partic_women, color="Women")) +
  geom_line(aes(y=sum_partic_men, color="Men")) +
  stat_difference(aes(ymin = sum_partic_men, ymax = sum_partic_women), alpha = 0.3) +
  scale_color_manual(values = c("#7c5eae","#176746")) +
  scale_fill_manual(
    values = c(
      colorspace::lighten("#176746"), 
      colorspace::lighten("#7c5eae"), 
      "#e0b700"
    ),
    labels = c("More women", "More men", "Same")
  ) +
  facet_wrap(~sports, scales="free") +
  scale_x_continuous(breaks=c(2015, 2017, 2019)) +
  scale_y_continuous(labels=scales::label_number_si()) +
  cowplot::theme_half_open(9) +
  theme(legend.position = "top",
        text=element_text(family="dosis"),
        plot.margin=margin(.5,.75,.5,.5, unit="cm"),
        axis.line = element_line(size=.2),
        strip.background = element_rect(fill="#e9ecef"),
        strip.text=element_text(family="Oswald"),
        legend.title=element_blank(),
        legend.text=element_text(size=11),
        legend.box = "vertical",
        legend.box.just = "left",
        axis.title.x=element_blank(),
        plot.title=element_text(size=16, family="Roboto"),
        plot.caption=element_text(size=9),
        ) +
  guides(color=guide_legend(reverse = T, order=1)) +
  labs(y="Sum of participation",
       title="College Sports Participation (2015 - 2019)",
       caption="\n#TidyTuesday week 13 | Data from Equity in Athletics Data Analysis | Method from Georgios Karamanis")
       
ggsave("2022_13c.png", width=8, height=8, units="in", bg="white")

# participation and expenditure by school classification, women and men
sports %>%
  mutate(parti = partic_men + partic_women) %>%
  ggplot() +
  geom_point(aes(x=partic_men, y=exp_men),color="#EE5A45", size=1, alpha=.3, stroke=.1) +
  geom_point(aes(x=partic_women, y=exp_women),color="#1E8F89", size=1, alpha=.3, stroke=.1) +
  facet_wrap(~classification_name, scales="free", ncol=4) +
  scale_y_continuous(labels=scales::label_number_si()) +
  cowplot::theme_half_open(10) +
  theme(legend.position = "top",
        text=element_text(family="dosis"),
        plot.margin=margin(.5,.75,.5,.5, unit="cm"),
        axis.line = element_line(size=.2),
        strip.background = element_rect(fill="#e9ecef"),
        plot.title=element_text(size=15, family="Roboto"),
        plot.subtitle=element_markdown(size=11, family="Roboto"),
        plot.caption=element_text(size=8, family="Roboto"),
        ) +
  labs(x="Sum of participation", y="Expenditures in USD",
       title="Collegiate sports (2015 - 2019)",
       subtitle="Expenditure in USD and sum of participation by school classification, <span style='color:#1E8F89'>**women**</span> and <span style='color:#EE5A45'>**men**</span><br>",
       caption="\n#TidyTuesday week 13 | Data from Equity in Athletics Data Analysis")
       
ggsave("2022_13d.png", width=8, height=8, unit="in", bg="white")

# Average expenditure of collegiate sport by year and school classification
sports %>% group_by(classification_name, year) %>%
  summarise(avg_exp=mean(total_exp_menwomen, na.rm=T)) %>%
  ungroup() %>%
  group_by(classification_name) %>%
  mutate(col=case_when(avg_exp==max(avg_exp)~"#ee9b00", TRUE~"#89b0ae")) %>%
  ggplot(aes(x=year, y=avg_exp, fill=col)) +
  geom_col(width=.7, alpha=.85) +
  facet_wrap(~classification_name, scales="free_y", ncol=4) +
  scale_fill_identity() +
  scale_y_continuous(labels=scales::label_number_si()) +
  cowplot::theme_minimal_hgrid(10) +
  theme(text=element_text(family="dosis"),
        plot.margin=margin(.5,.75,.5,.5, unit="cm"),
        axis.line.x = element_blank(),
        axis.title.x=element_text(size=11),
        axis.title.y=element_text(size=11, margin=margin(r=7)),
        plot.title.position = "plot",
        strip.text=element_text(size=8.5),
        plot.title=element_text(size=14, family="Roboto"),
        plot.subtitle=element_markdown(size=10, lineheight = 1.3,family="Roboto"),
        plot.caption=element_text(family="Roboto", margin(t=8)),
        panel.spacing.y = unit(1.2, "lines")
        ) +
  labs(title="Average expenditure of collegiate sports (2015 - 2019)",
       subtitle="By year and school classification, <span style='color:#ee9b00'>**yellow**</span> bars show the highest average expenditure.<br>From 2015 to 2019, 11 Out of 19 school classifications had the highest average expenditure in 2018<br>",y="Average expenditure in USD", x="Year",
       caption="#TidyTuesday week 13 | Data from Equity in Athletics Data Analysis")
       
ggsave("2022_13e.png", width=8, height=8, unit="in", bg="white")