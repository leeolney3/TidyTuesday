# 2022-05-24
# TidyTuesday week 21 Women's Rugby
# Data from ScrumQueens by way of Jacquie Tran


# Libraries
library(tidyverse)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Libre Franklin")
f1 = "Libre Franklin"

# Data (Sevens)
sevens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/sevens.csv')

## Margin of victory
# Wrangle
dfs = sevens %>% pivot_longer(c("team_1","team_2")) %>%
  pivot_longer(score_1:score_2, names_to="team_score", values_to="score") %>%
  filter(!(name=="team_1"& team_score=="score_2"),!(name=="team_2"& team_score=="score_1")) %>%
  group_by(value) %>%
  mutate(id=row_number(),
         win_lose = case_when(value==winner~"win", TRUE~"lose"),
         score=parse_number(score),
         margin1 = case_when(win_lose=="lose"~margin*-1, TRUE~margin)) %>%
  rename(team = value)
  
selected = sevens %>% count(winner, sort=T) %>% slice(1:10) %>% pull(winner)

dfs2 = dfs %>% filter(team %in% selected) %>% count(team, win_lose) %>%
  pivot_wider(values_from = n, names_from = win_lose) %>%
  ungroup() %>%
  mutate(total = lose+win,
         win_pct=win/total,
         id=row_number(),
         team1 = glue::glue("<span style = 'font-size:10pt'>**{team}**</span><br>{win} wins ({scales::percent(win_pct, accuracy=.2)})<br>{total} games")) %>%
  select(team, team1)
  
dfs3 = dfs %>%
  filter(team %in% selected) %>%
  left_join(dfs2, by="team")

dfs4 = dfs3 %>% filter(team=="Australia", row_id==max(row_id)) 

# Plot

dfs3 %>%
  ggplot(aes(x=id, y=margin1)) +
  geom_point(aes(color=win_lose),size=.3, alpha=.9) +
  geom_segment(aes(x=id, xend=id, y=0, yend=margin1, color=win_lose), size=.3, alpha=.9)+
  geom_richtext(data = dfs4, aes(x=id +16, y= 30, label="Winning<br>margin"), color="#007352", size=2.8, label.color=NA, lineheight=.8, hjust=0, alpha=.8,label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_richtext(data = dfs4, aes(x=id +16, y= -37, label="Losing<br>margin"), color="#F09485", size=2.8, label.color=NA, lineheight=.8, hjust=0,label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_segment(data = dfs4, aes(x=id +15, xend=id+2, y=-20, yend=-7), size=.3, color="grey50", arrow=arrow(length = unit(0.07, "npc"))) +
  geom_segment(data = dfs4, aes(x=id +15, xend=id+2.5, y=38, yend=32), size=.3, color="grey50", arrow=arrow(length = unit(0.07, "npc"))) +
  facet_wrap(~factor(team1,levels=dfs2$team1), ncol=1, strip.position="left") +
  scale_color_manual(values=c("#F09485","#007352")) +
  scale_x_continuous(limits=c(-9,540)) +
  coord_cartesian(expand=F, clip="off") +
  theme_void() +
  theme(text=element_text(family=f1),
        plot.title.position = "plot",
        plot.title=element_text(hjust=.5, margin=margin(b=7), face="bold"),
        plot.subtitle=element_text(hjust=.5, margin=margin(b=10), size=9),
        plot.caption=element_text(margin=margin(t=10), color="grey30", size=8.5),
        strip.placement = "outside",
        legend.position = "none",
        axis.title.x = element_text(size=8),
        strip.text.y.left= element_markdown(angle=0, hjust=1, lineheight = 1.2),
        plot.margin=margin(.5,.5,.3,.5,unit="cm")) +
  labs(x="Game number",
       title="Women's Rugby Sevens margin of victory",
       subtitle="10 teams with the most wins from Mar 15, 1997 to Nov 28, 2022",
       caption="#TidyTuesday week 21 | Data from ScrumQueens by way of Jacquie Tran")
       
ggsave("2022_21.png", height=8, width=8, bg="white")

## Margin of victory v2
# Arranged in descending order of total wins, add min/max label
dfs2 = dfs %>% filter(team %in% selected) %>% count(team, win_lose) %>%
  pivot_wider(values_from = n, names_from = win_lose) %>%
  ungroup() %>%
  mutate(total = lose+win,
         win_pct=win/total,
         id=row_number(),
         team1 = glue::glue("<span style = 'font-size:10pt'>**{team}**</span><br><span style = 'color:#007352;'>{win}</span> wins ({scales::percent(win_pct, accuracy=.2)})<br>{total} games")) %>%
  arrange(desc(win)) %>%
  select(team, team1)
  
dfs3 = dfs %>%
  filter(team %in% selected) %>%
  left_join(dfs2, by="team")
  
dfs4 = dfs3 %>% filter(team=="Australia", row_id==max(row_id)) 

dfmin = dfs3 %>% ungroup() %>% group_by(team) %>% filter(margin1==min(margin1,na.rm=T))
dfmax = dfs3 %>% ungroup() %>% group_by(team) %>% filter(margin1==max(margin1,na.rm=T)) 

dfs3 %>%
  ggplot(aes(x=id, y=margin1)) +
  geom_point(aes(color=win_lose),size=.3, alpha=.9) +
  geom_segment(aes(x=id, xend=id, y=0, yend=margin1, color=win_lose), size=.3, alpha=.9)+
  # key label
  geom_richtext(data = dfs4, aes(x=id +16, y= 30, label="**Winning**<br>margin"), color="#007352", size=2.8, label.color=NA, lineheight=.8, hjust=0, alpha=.8,label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_richtext(data = dfs4, aes(x=id +16, y= -37, label="**Losing**<br>margin"), color="#F09485", size=2.8, label.color=NA, lineheight=.8, hjust=0,label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_segment(data = dfs4, aes(x=id +15, xend=id+2, y=-20, yend=-7), size=.3, color="grey50", arrow=arrow(length = unit(0.07, "npc"))) +
  geom_segment(data = dfs4, aes(x=id +15, xend=id+2.5, y=38, yend=32), size=.3, color="grey50", arrow=arrow(length = unit(0.07, "npc"))) +
  # label min max
  #geom_text(data= dfs3 %>% ungroup() %>%filter(margin1==max(margin1, na.rm=T) | margin1==min(margin1, na.rm=T)), aes(label=margin1,color=win_lose, y=ifelse(margin1==min(margin1),margin1-20,margin1+20)), size=2.5, family=f1)+
  geom_text(data=dfmin, aes(label=margin1, color=win_lose, y=margin1-20), size=2.5, family=f1) +
  geom_text(data=dfmax %>% filter(row_id==min(row_id)), aes(label=margin1, color=win_lose, y=margin1+20), size=2.5, family=f1) +
  facet_wrap(~factor(team1,levels=dfs2$team1), ncol=1, strip.position="left") +
  scale_color_manual(values=c("#F09485","#007352")) +
  scale_x_continuous(limits=c(-9,540)) +
  coord_cartesian(expand=F, clip="off") +
  theme_void() +
  theme(text=element_text(family=f1),
        plot.title.position = "plot",
        plot.title=element_text(hjust=.5, margin=margin(b=7), face="bold"),
        plot.subtitle=element_text(hjust=.5, margin=margin(b=10), size=9),
        plot.caption=element_text(margin=margin(t=10), color="grey30", size=8.5),
        strip.placement = "outside",
        legend.position = "none",
        axis.title.x = element_text(size=8, margin=margin(t=8)),
        panel.spacing = unit(0, "lines"),
        strip.text.y.left= element_markdown(angle=0, hjust=1, lineheight = 1.2),
        plot.margin=margin(.5,.5,.3,.5,unit="cm")) +
  labs(x="Game number",
       title="Women's Rugby Sevens margin of victory",
       subtitle="10 teams with the most wins from Mar 15, 1997 to Nov 28, 2022",
       caption="#TidyTuesday week 21 | Data from ScrumQueens by way of Jacquie Tran")

ggsave("2022_21_v2.png", height=9, width=8, bg="white")  

## New Zealand wins
# Streaks method from: https://data-and-the-world.onrender.com/posts/streaks-in-r/
font_add_google("IBM Plex Sans Condensed")
f2 = "IBM Plex Sans Condensed"

font_add_google("IBM Plex Sans")
f3 = "IBM Plex Sans"

sdf_nz = dfs %>% filter(team=="New Zealand") %>%
  mutate(lagged=lag(win_lose),
         start=(win_lose != lagged),
         start=case_when(id==1~TRUE, TRUE~start),
         streak_id=cumsum(start)) %>%
  group_by(streak_id) %>%
  mutate(streak=row_number()) %>%
  ungroup() %>%
  mutate(streak1 = streak * ifelse(win_lose == "win", 1, -1),
         x=row_number())
         
sdf_nz2 = sdf_nz %>% group_by(streak_id) %>%
  filter(streak==max(streak)) %>%
  arrange(desc(streak)) %>%
  ungroup() %>%
  slice(1:5) %>%
  mutate(lab = glue::glue("**{toOrdinal::toOrdinal(streak)}** consecutive win<br>on {date}"))
  
sdf_nz %>%
  ggplot(aes(x=x, y=streak)) +
  geom_col(aes(fill=win_lose), show.legend = F) +
  geom_richtext(data=sdf_nz2, aes(label=lab, y=streak+2.5), label.color=NA,fill=NA, family=f2,label.padding = grid::unit(rep(0, 4), "pt"), color="white") +
  scale_fill_manual(values=c("win"="#F6F6F6", "lose"="transparent"), guide="none",) +
  scale_y_continuous(limits=c(0,50), breaks=seq(-5,50,5),expand = expansion(mult = c(0.01, .05))) +
  scale_x_continuous(expand=c(0.01,0.01), breaks=seq(0,400,50), limits=c(0,385)) +
  theme_minimal(12) +
  theme(text=element_text(family=f3, color="#f8f9fa"),
        axis.text=element_text(color="#ced4da"),
        axis.title=element_text(size=11.5,color="#ced4da"),
        panel.grid=element_blank(),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks=element_line(color="#ced4da", size=.3),
        plot.background = element_rect(fill="black", color=NA),
        plot.title=element_text(size=16),
        plot.subtitle=element_text(size=10),
        plot.caption = element_text(color="#ced4da", size=9.5, margin=margin(t=13)),
        plot.margin=margin(.5,.5,.3,.5,unit="cm")) +
  labs(x="Game number", y="Streak number",
       title="New Zealand Women's Rugby Sevens Wins",
       subtitle="From Mar 15, 1997 to Nov 28, 2022",
       caption="#TidyTuesday week 21 | Data from ScrumQueens by way of Jacquie Tran | Method from Data and The World")           

ggsave("2022_21_p2.png", height=7, width=8)