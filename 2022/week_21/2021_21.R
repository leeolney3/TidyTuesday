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
dfs3 = dfs %>%
  filter(team %in% selected) %>%
  left_join(dfs2, by="team")

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

