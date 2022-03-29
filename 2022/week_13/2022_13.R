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
