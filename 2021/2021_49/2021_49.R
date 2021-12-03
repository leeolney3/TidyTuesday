# Tidy Tuesday Week 49 2021-11-30 
# World Cup Cricket, data from ESPN Cricinfo by way of Hassanasir.

library(tidyverse)
library(lubridate)
library(gt)
library(gtExtras)

matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

# dates
m1 = matches %>%
  mutate(date = mdy(match_date)) %>%
  mutate(match_date2 = case_when(is.na(date)~ paste0(str_sub(match_date, start=1L, end=6L), 
                                       str_sub(match_date, start=10L, end=15L)),
                                 TRUE~match_date)) %>%
  mutate(mdate = mdy(match_date2),
         myear = year(mdate)) 
         
range(m1$mdate)

# table1: winners      

m2 = m1 %>% 
  mutate(winner=case_when(winner=="Pakistan awarded the match (opposition conceded)" ~"Pakistan",
                          TRUE~winner)) %>%
  count(winner, sort=T) %>%
  slice(1:10)

m3 = m1 %>% count(winner, mdate) %>%
  filter(!grepl('XI', winner),
         !grepl('Match tied', winner)) %>%
  filter(winner!="No result") %>%
  mutate(winner=case_when(winner=="Pakistan awarded the match (opposition conceded)" ~"Pakistan",
                          TRUE~winner)) 

m4 = m3 %>%
  complete(mdate, winner) %>%
  mutate(n=replace_na(n,0)) %>%
  group_by(winner) %>% 
  summarise(timeline = list(n)) %>%
  filter(winner %in% m3$winner)
  
tab = m2 %>% left_join(m4) 
  
p1 = tab %>%
  gt() %>%
  gt_theme_nytimes() %>%
  gt_sparkline(timeline,width=125, label=FALSE,
               range_colors = c("#353535", "blue"),
               line_color = "#353535") %>%
  cols_label(winner="Team", n="Total wins", timeline="Wins Timeline") %>%
  tab_header(title="Cricket Winners", subtitle="10 Teams with the most wins from Jan 01, 1996 to Dec 31, 2005") %>%
  tab_source_note(source_note="#TidyTuesday Week 49, data from ESPN Cricinfo by way of Hassanasir.") %>%
  tab_options(source_notes.padding = px(10),
              source_notes.font.size = px(11)) 
              


# table2: results
played = m1 %>%
  pivot_longer(cols=c(team1, team2), values_to="team") %>%
  count(team) %>% rename(played=n)

wins = m1 %>%
  count(winner) %>% rename(wins=n, team=winner)

tied_df = m1 %>%
  filter(winner=="Match tied" | winner=="Match tied (D/L method)") %>%
  pivot_longer(cols=c(team1, team2), values_to="team") %>%
  count(team) %>% rename(tied = n)

year_df = m1 %>%
  pivot_longer(cols=c(team1, team2), values_to="team") %>%
  group_by(team) %>%
  summarise(from=min(myear), to=max(myear)) %>%
  mutate(year= glue::glue("{from}-{to}")) %>%
  select(team, year)
  
joined = played %>%
  left_join(wins, by="team") %>%
  left_join(tied_df, by="team") %>%
  replace(is.na(.), 0) %>%
  mutate(losses=played-wins-tied) %>%
  left_join(year_df, by="team") %>%
  slice_max(played, n=9) %>%
   mutate("win%" = round(wins/played,4),
         "loss%" = round(losses/played,4),
         "tied%" = round(tied/played,4),
         ) 
         
pct = joined %>% select(team, 7:8) %>%
  pivot_longer(!team) %>%
  group_by(team) %>%
  summarise(list_pct=list(value))

count = joined %>% select(team, wins, losses) %>%
  pivot_longer(!team) %>%
  group_by(team) %>%
  summarise(list_count=list(value))
  
p2 = joined %>% 
  left_join(count,by = "team") %>%
  left_join(pct,by = "team") %>%
  select(team, played, list_count, list_pct, tied, "tied%") %>%
  gt() %>%
  gt_theme_nytimes() %>%
  fmt_percent(column="tied%") %>%
  gt_plt_bar_stack(column=list_count,
                   width=60,
                   labels = c("Wins", "Losses"),
                   palette=c("#219ebc","#fd9e02"),
                   position="stack") %>%
  gt_plt_bar_stack(column=list_pct,
                   width=60,
                   position="stack",
                   palette=c("#126782","#fb8500"),
                   labels = c("Wins %", "Losses %"),
                   fmt_fn = scales::percent_format(scale=,accuracy=0.1)) %>%
  gt_plt_dot(played, team,
             palette = "nord::polarnight",
             max_value = 319) %>%
  cols_align(c("tied", "played", "tied%"), align = "center") %>%
   tab_header(title="ICC Cricket World Cup ODI Results", 
             subtitle="Nine teams with most matches played, from 1996 to 2005") %>%
  tab_source_note(source_note="#TidyTuesday Week 49, data from ESPN Cricinfo by way of Hassanasir.") %>%
  tab_options(source_notes.padding = px(10),
              source_notes.font.size = px(12)) %>%
  gt_color_rows(columns="tied%", palette="ggsci::teal_material")


              
# Wins by team and year (10 teams with the most wins)
y1 = m1 %>% filter(winner %in% m2$winner) %>%
  count(myear, winner, sort=T) 
  
library(sysfonts)
library(showtext)
font_add_google("Lato") 
showtext_auto()
font_family = "Lato"

p3 = y1 %>%
  ggplot(aes(x=myear, y=n)) +
  geom_line(aes(group=winner, color=winner), 
            size=.8, alpha=.5) +
  geom_point(aes(color=winner), alpha=.9) + 
  geom_text(data=y1 %>% filter(myear==2005, n>10),
            aes(x=2005.13,label=winner, color=winner),
            size=rel(3), hjust=0, fontface="bold", family=font_family) + 
  geom_text_repel(data=y1 %>% filter(myear==2005, n<10), 
                  aes(x=2005.2,label=winner, color=winner), 
                  hjust=0, size=rel(3), direction="y",fontface="bold", family=font_family) +
  geom_text(data=y1 %>% filter(myear==2003, n==4), 
            aes(label=winner, color=winner,fontface="bold", family=font_family), 
            size=rel(3), nudge_y = -1) +
  scale_x_continuous(breaks=seq(1996, 2005,1),
                     expand = expansion(mult = c(.02, .07))) +
  scale_y_continuous(expand=c(0,0)) +
  coord_cartesian(clip="off",ylim=c(0,30),xlim=c(1996,2006)) +
  rcartocolor::scale_color_carto_d(palette="Prism") +
  cowplot::theme_minimal_hgrid(10) +
  theme(text=element_text(family=font_family),
        legend.position = "none",
        plot.title.position = "plot",
        plot.title=element_text(hjust=.5),
        plot.subtitle = element_text(margin=margin(b=10), hjust=.5),
        plot.margin=margin(.5,1.5,.5,1.5, unit="cm"),
        plot.caption=element_text(margin=margin(t=8)),
        plot.background = element_rect(fill="#fafafa", color=NA)
        ) +
  labs(x="Match Year", y="Number of Wins",
       title="Cricket Wins by Team and Year",
       subtitle="10 teams with the most wins from Jan 1, 1996 to Dec 31,2005",
       caption="#TidyTuesday Week 49, data from ESPN Cricinfo by way of Hassanasir.")
       
       
       
# Team South Africa: odi results by year
# code adapted from @JaredBraggins

## za dataframe 
za_df = m1 %>%  
  pivot_longer(c(team1, team2), names_to = "teams", values_to = "Teams") %>%
  mutate(score = case_when(teams == "team1" ~ score_team1,
                      TRUE ~ score_team2),
         home_away = case_when(teams == "team1" ~ team1_away_or_home,
                           TRUE ~ team2_home_away),
         result = case_when(winner == "South Africa" ~ "Won",
                          TRUE ~ "Lost")) %>%
  select(Teams, score, myear, mdate, home_away, winner, result) %>%
  filter(Teams == "South Africa" & !is.na(myear)) %>%
  group_split(myear) %>%
  map_dfr(~ .x %>%
            add_row(score = 0,  Teams = "South Africa",  .before = 1)) %>%
  fill(myear, .direction = "up")%>%
  mutate(rank = row_number(),
         result_filtered = case_when(result == "Lost" ~ "Lost",
                            TRUE ~ "Won")) %>%
  rename(year=myear, date=mdate)

## label dataframes
lab_df = za_df %>%
  group_by(year) %>%
  summarize(start=min(rank)+1, end=max(rank)) %>%
  rowwise() %>%
  mutate(title=mean(c(start, end)))

labs2 = za_df %>% arrange(desc(score)) %>% slice(1)
labs3 = za_df %>% arrange(desc(score)) %>% slice(2:5)

text_colour="white"

## plot circular bar plot
p4 = ggplot(za_df, aes(x = rank, y = score)) +
  geom_segment(data=lab_df, aes(x = start, y = -20, xend = end, yend = -20),
               colour = text_colour, size=0.6, inherit.aes = FALSE) +
  geom_text(data = lab_df, aes(x = title, y = -80, label = year),
            size = 3, family = font_family, parse = TRUE, color = text_colour) +
  geom_bar(data = za_df, aes(x = rank, y = score, fill = factor(result_filtered)),
           stat="identity") +
  geom_text(data= labs3, aes(label=score, y=score+35),color=text_colour, size=3) +
  geom_text(data= labs2, aes(label=glue::glue("Score: {score}"), y=score+35),color=text_colour, size=3) +
  scale_fill_manual(values=c("#191919","#FFB81C")) +
  ylim(-500, 400) +
  coord_polar(start = 0) +
  geom_text(x = 400, y = -550,
            label = "South Africa",
            family = font_family,
            size = 7.5,
            lineheight = 1,
            color = text_colour) +
  geom_text(x = 400, y = -420,
            label = "ICC Cricket World Cup\nODI Results By Year",
            family = font_family,
            size = 3.2,
            lineheight = 1.2,
            color = text_colour) +
  labs(caption="#TidyTuesday Week 49 | Data: ESPN Cricinfo") +
  theme_void() +
  theme(text = element_text(family = font_family),
        plot.background = element_rect(fill = "#007749", color = NA),
        plot.caption = element_text(size = 8, colour = text_colour, margin=margin(10,0,0,0), hjust = 0.5),
        plot.margin = unit(c(1, 0, .5, 0), "cm"),
        legend.text = element_text(size = 12, colour= text_colour),
                     legend.title = element_blank(),
                     legend.box = "horizontal",
                     legend.position="bottom",
                     legend.justification = "center"
        )



# Team South Africa: toss, toss decision, winner combination
m1 %>% 
  filter(team1=="South Africa" | team2=="South Africa") %>%
  mutate(winner_za = case_when(winner=="South Africa" ~ 1,TRUE~0),
         toss_za = case_when(toss=="South Africa"~1, TRUE~0)
         ) %>%
  count(toss_za, toss_decision,winner_za) 

