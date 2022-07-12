# 2022-07-12
# TidyTuesday week 28 European Flights, data from Eurocontrol by way of Data is Plural
# Plot inspired by [Lisa Charlotte Muth](https://blog.datawrapper.de/german-party-polls-vs-election-results/)

# Load ibraries
library(tidyverse)
options(dplyr.summarise.inform = FALSE)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Import font
font_add_google("Barlow")
f1 = "Barlow"

# Import Data
flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')

# Wrangle 
df1 = flights %>% select(1:7, FLT_TOT_1, FLT_TOT_IFR_2) %>%
  pivot_longer(8:9) %>%
  group_by(YEAR,MONTH_NUM, MONTH_MON, FLT_DATE,APT_ICAO, APT_NAME, STATE_NAME) %>%
  mutate(mean=mean(value,na.rm=TRUE)) %>%
  select(-name, -value) %>%
  distinct() %>%
  ungroup()
  
selected = df1 %>% 
  filter(STATE_NAME=="France") %>%
  group_by(APT_ICAO, APT_NAME, STATE_NAME) %>%
  tally(mean) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  slice(1:3) %>%
  pull(APT_NAME)
  
df2= df1 %>% filter(APT_NAME %in% selected) %>% complete(APT_NAME, FLT_DATE) %>%
  group_by(APT_NAME,YEAR, MONTH_NUM, FLT_DATE) %>% tally(mean) %>%
  ungroup() %>% group_by(APT_NAME,YEAR) %>% mutate(mean=mean(n,na.rm=TRUE)) %>%
  ungroup() %>% group_by(APT_NAME) %>% arrange(FLT_DATE, by.group=TRUE) %>% 
  mutate(id=row_number()) %>%
  ungroup() %>% mutate(grp=case_when(n>mean~1,TRUE~.5),n = ifelse(n == 0, NA, n))

df3 = df2 %>% group_by(APT_NAME, YEAR, mean) %>% summarise(x=min(id), xend=max(id+1))

breaks= df2 %>% filter(APT_NAME=="Nice-Côte d’Azur") %>% group_by(YEAR) %>%
  summarise(id=min(id)) %>%
  pull(id)
  
# Plot
p1 = ggplot() +
  geom_rect(data=df2, aes(xmin=id, xmax=id+1, ymin=mean, ymax=n, alpha=grp, fill=factor(APT_NAME, levels=selected), group=APT_NAME)) +
  scale_alpha_identity() +
  geom_segment(data=df3, aes(x=x, xend=xend, y=mean, yend=mean, color=factor(APT_NAME, levels=selected))) + 
  geom_step(data=df2, aes(group=APT_NAME, x=id, y=mean, color=factor(APT_NAME, levels=selected))) +
  geom_text(data=tibble(APT_NAME=selected, x=c(1850,300,300), y=c(1400,870,100)) %>%
  mutate(label=str_replace_all(APT_NAME,"-"," ")),
            aes(x=x, y=y,label=label, color=APT_NAME), family=f1, fontface="bold", size=4) +
  scale_fill_manual("Airport name:",values=c("#1D8414","#FFA900","#B62DA1")) +
  scale_color_manual("Airport name:",values=c("#1D8414","#FFA900","#B62DA1")) +
  scale_y_continuous(labels=scales::comma, expand=c(0,0), limits=c(0,NA)) +
  scale_x_continuous(breaks=c(breaks,2343), labels = c("Jan 01\n2016","Jan 01\n2017","Jan 01\n2018","Jan 01\n2019","Jan 01\n2020","Jan 01\n2021","Jan 01\n2022","May 31\n2022"), expand=c(0.02,0.02)) +
  cowplot::theme_minimal_hgrid(11) +
  theme(text=element_text(family=f1),
        axis.ticks.length = unit(.5,"lines"),
        axis.ticks.x=element_line(),
        axis.ticks.y=element_blank(),
        legend.position = "none",
        axis.text.x=element_text(size=8),
        axis.text.y=element_text(size=8.5, margin=margin(r=-3)),
        axis.title=element_text(size=9.2),
        plot.caption.position = "plot",
        plot.caption=element_text(color="grey20", hjust=0, lineheight=1, size=7.8),
        plot.title.position = "plot",
        plot.subtitle=element_text(lineheight =1.1),
        plot.margin=margin(.5,.5,.4,.5, unit="cm"),
        #plot.background = element_rect(fill="grey98", color=NA)
        ) +
  labs(y="Total Movement", x="Date of flight", 
       caption="\nNote: Number total IFR movement from Network Manager and/or Airport Operator, the average of both sources were used when available.\nYearly average movement for 2022 is calculated from Jan 01, 2022 to May 31, 2022.\nTidyTuesday week 28  •  Source: Eurocontrol",
       title="Movements of the 3 busiest airports in France",
       subtitle="Yearly average and daily total IFR movement (number of IFR arrivals minus number total IFR movements) by airport,\nfrom Jan 01, 2016 to May 31, 2022.\n")
              
ggsave("2022_28.png", p1, height=7, width=7, bg="white")       
          