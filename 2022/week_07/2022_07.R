# 2022-02-15
# TidyTuesday week 7 and DuBoisChallenge2022 
# Data from Anthony Starks as part of the #DuBoisChallenge2022. 

library(tidyverse)
library(tidytext)

# challenge02 
data = read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge02/data.csv") %>% janitor::clean_names()
data = data %>% distinct()

d1 = data %>% mutate(r1=5393885, r2=5764293, r3= 8153390, r4=12322003, r5=12941230, r6=13447423)

data %>%
  ggplot(aes(x=year, y=valuation_dollars, fill=factor(year))) +
  # circles
  geom_col(data=d1, aes(x=year, y=r6), fill="#C51334", width=7) +
  geom_col(data=d1, aes(x=year, y=r5), fill="#E0CAB0", width=7) +
  geom_col(data=d1, aes(x=year, y=r4), fill="#EFB700", width=7) +
  geom_col(data=d1, aes(x=year, y=r3), fill="#344D87", width=7) +
  geom_col(data=d1, aes(x=year, y=r2), fill="#A38067", width=7) +
  geom_col(data=d1, aes(x=year, y=r1), fill="#181818", width=7) +
  # columns
  geom_col(data=data %>% filter(year!=1875), aes(fill=factor(year)), width=1.3, show.legend=F) +
  scale_fill_manual(values=c("black","#A38067","#344D87","#EFB700","#D5C0A9","#C51334")) +
  # value labels
  geom_text(data = data %>% filter(year!=1875), aes(label=scales::dollar(valuation_dollars)), color="black", size=3.5, nudge_y = -1800000, angle=c(65,-60,0,60,-75)) +
  geom_text(data = data %>% filter(year==1885), aes(label=scales::dollar(valuation_dollars)), color="white", size=3.5, nudge_y = -1800000, angle=-60) +
  geom_text(data = data %>% filter(year==1875), aes(label=scales::dollar(valuation_dollars)), color="white", size=3.5, nudge_y = -2200000) +
  scale_x_reverse() +
  coord_polar(start=13447423) +
  theme_void(base_family = "mono") +
  theme(plot.background = element_rect(fill="#E9D8C5", color=NA),
        plot.title=element_text(hjust=.5, size=17),
        plot.margin=margin(.75,.5,.5,.5, unit="cm"),
        plot.caption=element_text(hjust=.5, color="grey30", size=9.5)
  ) +
  labs(title=str_to_upper("Assessed Valuation of all Taxable Property\nOwned by Georgia Negroes"),
       caption="#TidyTuesday #DuboisChallenge2022 | Data source: Anthony Starks") +
  # year labels
  geom_text(data= data %>% filter(year!=1875 & year!=1885),aes(x=1882, y=valuation_dollars-240000, label=year), color="black", size=2.9) +
  geom_text(data= data %>% filter(year==1875 | year==1885),aes(x=1882, y=valuation_dollars-500000, label=year), color="white", size=3.2) 

ggsave("challenge02.png", height=9, width=8, unit="in")

# challenge 07
data = read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge07/data.csv",show_col_types = FALSE)

library(showtext)
font_add_google("Teko","teko")
font_add_google("EB Garamond")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
f1 ="teko"
f2 ="EB Garamond"

df = data %>% 
  mutate(Group=case_when(Group=="Over 65"~"Ages.<br>Over 65", TRUE~Group),
         Group=fct_inorder(Group)) %>%
  pivot_longer(!Gender:Group) %>%
  mutate(value= case_when(Gender=="Male"~-1*value, TRUE~value)) 

dflabs1 = levels(df$Group)
dflabs2 = levels(df$Group)

c07 = df %>%
  ggplot(aes(y=value,x=as.numeric(Group))) +
  geom_col(aes(fill=factor(name, levels=c("Widowed","Married","Single"))), show.legend=F,width=1, alpha=.95) +
  geom_hline(yintercept=seq(-100,100,2), color="black", alpha=.1, size=.3) +
  geom_hline(yintercept=seq(-100,100,10), color="black", alpha=.2, size=.4) +
  geom_hline(yintercept=0, color="black", alpha=.5, size=.4) +
  geom_vline(xintercept=seq(0.5,9.5,1), color="black", alpha=.2, size=.4) +
  scale_x_continuous(breaks = 1:length(dflabs1),
                     labels = str_to_upper(dflabs1),
                     sec.axis = dup_axis()) +
  scale_fill_manual(values=c("#3D8861","#CF1A42","#0058AC")) +
  scale_y_continuous("PER CENTS.",breaks=seq(-100,100,10), labels=abs(seq(-100,100,10))) +
  coord_flip(expand=F, clip="off") +
  theme_minimal() +
  theme(plot.background=element_rect(fill="#E0D5C8", color=NA),
        plot.title=element_markdown(hjust=.5, lineheight = 2.2, color="#7E6A52", family=f2, size=12),
        plot.margin=margin(.8,.4,1.2,.4, unit="cm"),
        axis.text.y.left =element_markdown(family="mono", size=10.5, color="black", 
                                           face="bold", vjust=-0.57),
        axis.text.y.right =element_markdown(family="mono", size=10.5, color="black",
                                            face="bold", vjust=-0.57),
        axis.text.x=element_text(family=f1, color="black", size=12, margin=margin(t=0)),
        axis.title.y=element_blank(),
        axis.title.x=element_text(family="mono", face="bold", margin=margin(t=4.5)),
        panel.grid = element_blank(),
        plot.caption=element_text(color="#7E6A52", family=f2, hjust=.5, size=10)
  ) +
  labs(title="<span style = 'font-size:17pt'>Conjugal condition of American Negroes according to age periods.</span><br>Condition conjugale des Negres Americains au point de vue de l'age.<br><span style = 'font-size:10pt'>Done by Atlanta University.</span><br>",
       caption="\n#TidyTuesday #DuboisChallenge2022 | Data source: Anthony Starks") +
  annotate(geom="rect", ymin=-100.2, ymax=100.2, xmin=9.5, xmax=9.7, color=NA, fill="#E0D5C8") +
  annotate(geom="text", y=c(-50,50), x=c(9.62,9.62), label=c("MALES.","FEMALES."), 
           family="mono", fontface="bold", size=4.4) +
  annotate(geom="text", y=c(-38,-55,-93), x=c(1.8,5.5,8.5), label=c("SINGLE","MARRIED","WIDOWED"),
           angle=c(52,52,65), family=f1, size=c(7.2,7.2,5.3)) +
  annotate(geom="text", y=c(38,55,89), x=c(1.8,5.5,7.5), label=c("SINGLE","MARRIED","WIDOWED"),
           angle=c(-52,-52,-63), family=f1, size=c(7.2,7.2,5.5)) 

library(cowplot)
cowplot::set_null_device("png")
cowplot::plot_grid(c07) +
  # add lines in title
  draw_line(x=c(0.42,0.58),y=c(0.905,0.905), size=.3, color="#7E6A52", alpha=.3) +
  draw_line(x=c(0.42,0.58),y=c(0.9405,0.9405), size=.3, color="#7E6A52", alpha=.3)

ggsave("challenge07.png",height=10, width=8.5)

# challenge 05
data5 = read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge05/data.csv",show_col_types = FALSE)

data5 = data5 %>%
  add_row(Year=1863, Slave=97, Free=3)

lab5 = data5 %>% 
  mutate(Free=ifelse(Free==100.0,"100%",Free),
                        Free=ifelse(Free==1.3,"1.3%",Free)) %>%
  filter(Year!=1863)

c05 = data5 %>% pivot_longer(Slave:Free) %>%
  ggplot(aes(x=Year, y=value)) +
  geom_area(aes(fill=name), show.legend=F, color="#E3D4C3") +
  geom_vline(xintercept=lab5$Year, color="#E3D4C3") +
  scale_x_reverse(breaks=lab5$Year, expand=c(0,0), 
                  sec.axis = sec_axis(~ . * 1, breaks = lab5$Year, labels = lab5$Free)
  ) +
  scale_y_continuous(expand=c(0,0), position="right",
                     breaks=c(97,98,99,100), labels=c("3%","2%","1%","")) +
  scale_fill_manual(values=c("#CD2642","#1E1B19")) +
  coord_flip(ylim=c(97,100)) +
  theme_minimal(base_family = "mono") +
  theme(plot.margin=margin(.8,3.4,.5,3.4,"cm"),
        axis.title=element_blank(),
        axis.text.y.left =element_text(margin=margin(r=17), size=8.3,color="grey40"),
        axis.text.y.right =element_text(margin=margin(l=17), size=8.3,color="grey40"),
        plot.background = element_rect(fill="#E3D4C3", color=NA),
        plot.title=element_text(hjust=.5, margin=margin(b=30)),
        plot.caption.position="plot",
        plot.caption=element_text(hjust=.5, margin=margin(t=40), size=7),
        axis.ticks.x.top = element_line(size=.3, color="grey50"),
        axis.text.x.top = element_text(size=7, color="grey40")
  ) +
  labs(title=str_to_upper("#DuBoisChallenge2022 Challenge 05"),
       caption="#TidyTuesday #DuboisChallenge2022 | Data source: Anthony Starks")

plot_grid(c05) + draw_text("PERCENT", x=.76, y=.89, size=7, color="grey40", family="mono")

ggsave("challenge05.png",width=6, height=7.5, unit="in")


