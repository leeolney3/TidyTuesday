# 2022-02-08
# TidyTuesday Week 6 
# Tuskegee Airmen, Data from Commemorative Airforce (CAF) by way of the VA-TUG

library(tidyverse)
library(ggtext)

# Data
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

# Fonts
library(showtext)
font_add_google("Teko","teko")
font_add_google("EB Garamond")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

f1 ="teko"
f2 ="EB Garamond"


# Wrangle
df1 = airmen %>%
  filter(number_of_aerial_victory_credits>0) %>%
  count(graduation_date, number_of_aerial_victory_credits, sort=T)

lab3 = airmen %>% filter(number_of_aerial_victory_credits==3) %>% select(name, graduation_date)
df2 = df1 %>% left_join(lab3, by="graduation_date")

lab4 = airmen %>% filter(number_of_aerial_victory_credits==4) %>%
  select(name, graduation_date,number_of_aerial_victory_credits) %>%
  mutate(name = case_when(graduation_date==as.Date("1942-09-06")~"Elsberry, Joseph D.<br>Toppins, Edward L.", TRUE~name)) %>%
  distinct(graduation_date, name,number_of_aerial_victory_credits) %>%
  inner_join(df1, by=c("number_of_aerial_victory_credits","graduation_date"))
  
# Plot
df2 %>%
  ggplot(aes(y=graduation_date, x=number_of_aerial_victory_credits, size=n)) +
  geom_point(aes(fill=factor(number_of_aerial_victory_credits)), shape=21, color="black") +
  geom_text(data = df2 %>% filter(number_of_aerial_victory_credits==3),
            aes(label=name,color= factor(number_of_aerial_victory_credits)), 
            size=4.5, hjust=0, nudge_x = .03, family=f1, show.legend = F) +
  geom_richtext(data=lab4, aes(label=name, color=factor(number_of_aerial_victory_credits)), 
                size=4.5, family=f1, hjust=0,nudge_x = .04,
                fill = NA, label.color = NA, show.legend = F,
                label.padding = grid::unit(rep(0, 4), "pt")) +
  scale_fill_manual(values=c("#FFD700","#654320","#4682B4","#008000","#DC123C")) +
  scale_color_manual(values=c("#008000","#DC123C")) +
  scale_size_continuous(breaks=c(unique(df1$n))) +
  scale_x_continuous(limits=c(1,4.5)) +
  coord_cartesian(clip="off") +
  theme_minimal(base_family = f1, base_size = 15) +
  theme(legend.position ="top",
        axis.text=element_text(size=13, color="black"),
        plot.background = element_rect(fill="#E0D5C8", color=NA,),
        plot.margin=margin(l=1.5, r=1.8, t=1,b=.5,unit="cm"),
        #panel.grid=element_line(color="#D2B48C"),
        panel.grid=element_line(size=.4),
        panel.grid.minor.x=element_blank(),
        legend.title=element_text(size=12,color="grey40"),
        legend.text=element_text(size=12,color="grey40"),
        axis.title=element_text(size=13, color="grey40"),
        axis.text.x=element_text(size=16),
        plot.title=element_text(color="#7E6A52", family=f2, size=16, hjust=.5),
        plot.subtitle=element_text(color="#7E6A52", family=f2, size=10.5, hjust=.5),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption=element_text(family=f2, color="#7E6A52", size=9, hjust=0),
        legend.margin=margin(l=-40)
        ) +
  guides(fill="none",
         size=guide_legend(nrow = 1)) +
  labs(x="Number of aerial victory credits",
       y="Graduation Date",
       size="Number of Tuskegee Airmen",
       title="The Tuskegee Airmen",
       subtitle="Tuskegee Airmen with 1 or more aerial victory credits",
       caption="\n#TidyTuesday Week 5 | Data from Commemorative Airforce (CAF) by way of the VA-TUG")
       
# Save
ggsave("2022_06.png", unit="in")
