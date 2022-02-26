# 2022-02-22
# TidyTuesday Week 8 World Freedom index
# Data from UN and Freedom House

library(tidyverse)
library(ggtext)
library(showtext)
library(countrycode)
font_add_google("Outfit", db_cache = FALSE)
font_add_google("Lato")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
f1="Outfit"

# Load data
freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv',show_col_types = FALSE)

range(freedom$CL)
range(freedom$PR)

df1 = freedom %>% 
  filter(year==2020, is_ldc==1) %>%
  pivot_longer(PR:CL) %>%
  mutate(code = countrycode(country, origin="country.name", destination="iso3c"),
         name=recode(name, 
                     "CL"="Civil<br>Liberties",
                     "PR" ="Political<br>Rights")) %>%
  mutate(Status = recode_factor(Status,
                         "F"="Free",
                         "PF"="Partially Free",
                         "NF"="Not Free"))
                         
# Beeswarm plot   
# Inspired by Fiona Lees @Fi_Lees <https://twitter.com/Fi_Lees/status/1496902332239552521/photo/1>
p1 = df1 %>%
  ggplot(aes(x=value, y=name, color=Status)) +
  geom_beeswarm(size=3,cex = 3.8, groupOnX = FALSE) +
  geom_text(aes(label=code, x=value+.1), size=3.7,  hjust=0, family=f1,
            position = position_beeswarm(groupOnX = FALSE, cex = 3.8),) +
  scale_color_manual("Status",values=c("#009e90","#023e8a","#b86092"))+
  scale_x_continuous(name="Rating",breaks=seq(1,7,1)) + 
  scale_y_discrete(expand = expansion(mult = c(.5, 0))) +
  coord_cartesian(clip="off") +
  theme_minimal(base_family = f2, base_size = 14) +
  theme(legend.position="top",
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(size=.3),
        plot.margin=margin(.5,1.1,.5,.5, unit="cm"),
        legend.margin=margin(l=-332),
        legend.text=element_text(size=11.5),
        legend.title=element_text(size=11.5),
        axis.text.y=element_markdown(lineheight = 1.3, color="black", size=13),
        axis.title.x=element_text(size=13),
        axis.title.y=element_blank(),
        plot.title=element_text(face="bold"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption=element_text(hjust=0, color="grey30", size=10)
        ) +
  labs(title="Political Rights and Civil Liberties, 2020",
       subtitle="Least Developed Countries",
       caption="\n#TidyTuesday Week 8 | Data from UN and Freedom House | Plot inspired by Fiona Lees @Fi_Lees")
       
ggsave("2020_08_p1.png", p1, height=8, width=8, unit="in", bg="white")


# Facetted area plot
# Inspired by Nicola Rennie @nrennie35 <https://twitter.com/nrennie35/status/1496038550860492801/photo/1>
library(geofacet)
library(ggnewscale)

df2 <- freedom %>%
  filter(Region_Name == "Africa") %>% 
  mutate(country = recode(country, 
                          "CÃƒÂ´te dÃ¢â‚¬â„¢Ivoire" = "Côte d'Ivoire", 
                          "Sao Tome and Principe" = "São Tomé and Principe", 
                          "United Republic of Tanzania" = "Tanzania", 
                          "Congo" = "Republic of the Congo"))  %>%
  mutate(Status = recode_factor(Status,
                         "F"="Free",
                         "PF"="Partially Free",
                         "NF"="Not Free")) 

rect_df = df2 %>% filter(year==2020) %>% select(country, Status)
area_df = df2 %>% 
  select(country, year, PR, CL) %>%
  pivot_longer(PR:CL) %>%
  mutate(value=case_when(name=="PR"~-1*value, TRUE~value),
         name=recode(name, 
                     "CL"="Civil Liberties",
                     "PR" ="Political rights"))
df3 = area_df %>% left_join(rect_df, by="country") 

p2 = df3 %>%
  ggplot(aes(year, value)) +
  geom_rect(data=df3 %>% filter(year==2020, name=="Political rights"), 
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,fill = Status),
      alpha = .75, color = NA
      ) +
  scale_fill_manual("Status in 2020",values=c("#84a59d","#F2B138","#F25A38")) +
  ggnewscale::new_scale_fill() +
  geom_area(aes(x=year, y=value, fill=name, color=name)) +
  scale_fill_manual("",values=c("#343a40","#ced4da")) +
  scale_color_manual("",values=c("#adb5bd","#343a40")) +
  facet_geo(~ country, grid = africa_countries_grid1, label = "code") +
  theme_minimal(base_size = 10, base_family = f1) +
  scale_x_continuous(breaks=c(2000,2020)) +
  scale_y_continuous(labels=abs, breaks=c(-7,0,7), limits=c(-7,7)) +
  theme(axis.title=element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        plot.margin=margin(.8,.8,.5,.8, unit="cm"),
        axis.ticks=element_line(color="grey30"),
        legend.text=element_text(size=10),
        legend.title = element_text(size=10),
        plot.title=element_text(hjust=.5, face="bold", size=16, margin=margin(b=10), family=f2),
        plot.subtitle = element_text(hjust=.5, size=13, face="bold", margin=margin(b=10), family=f2),
        plot.caption = element_text(hjust=.5, size=9, color="grey30")) +
  labs(caption="\n#TidyTuesday Week 8 | Data from UN and Freedom House | Plot inspired by Nicola Rennie @nrennie35",
       title="Civil Liberties and Political Rights Rating in Africa",
       subtitle="1995 to 2020")
       
ggsave("2022_08_p2.png", p2, height=11, width=8, unit="in", bg="white")