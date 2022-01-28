# TidyTuesday Week 4, 2022-01-25, Board Games
# Data from Kaggle by way of Board Games Geek, shared by David and Georgios

# Load libraries
library(tidyverse)
library(ggtext)
library(showtext)

# Fonts
font_add_google("Archivo Narrow","arch")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load Data
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
#details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

# Ratings by decade
# Wrangle
ratings2 = ratings %>% 
  filter(between(year, 1900, 2021)) %>%
  mutate(decade = floor(year/10)*10,
         decade = glue::glue("{decade}s"))
    
ratings3 = ratings2 %>%
  group_by(decade) %>%
  summarise(n=n(),
            min_rating=min(average),
            max_rating=max(average),
            min_rank= min(rank),
            max_rank=max(rank)) %>%
  pivot_longer(min_rating:max_rating) %>%
  mutate(ylab = glue::glue("<span style='color:black'>**{decade}**</span> (n={scales::comma(n, accuracy=1)})"))
 
labs_df = ratings2 %>%
  group_by(decade) %>%
  filter(average==min(average) | average==max(average)) %>%
  left_join(ratings3, by="decade") %>% 
  select(name.x, ylab, average, decade) %>% distinct()
  
# Plot
ratings3 %>%
  ggplot(aes(value, ylab)) +
  geom_line(aes(group=ylab), color="#f0c808") +
  geom_point(aes(color=name), show.legend=F, size=2.5) +
  geom_text(data = labs_df, aes(average,ylab, label= name.x),size=2.5, vjust=-1, family="arch", fontface="italic") +
  scale_color_manual(values=c("#dd1c1a","#07a0c3")) +
  scale_x_continuous(limits=c(0,10), expand=c(0,0)) +
  scale_y_discrete(expand = expansion(mult = c(.05, .07))) +
  coord_cartesian(clip="off") +
  cowplot::theme_minimal_grid(9.5,line_size = 0.3) +
  theme(text=element_text(family="arch"),
        panel.grid.major.y=element_line(linetype = "dotted"),
        axis.title =element_text(color="grey15", size=8),
        axis.text.y=element_markdown(hjust=0,color="grey40"),
        axis.text.x = element_text(color="grey20"),
        plot.margin=margin(.5,.75,.5,.5, unit="cm"),
        plot.title=element_text(size=12, hjust=.5),
        plot.subtitle = element_markdown(hjust=.5, size=9, margin=margin(b=7), color="grey20"),
        plot.background = element_rect(fill="#fcfcfc"),
        plot.caption=element_text(margin=margin(t=7), color="grey40"),
        plot.title.position = "plot",
        axis.ticks.y=element_blank()
        ) +
  labs(x="Average rating", y="Decade",
       title="Board games ratings by decade",
       subtitle="Board games with the <span style='color:#07a0c3'>**lowest**</span> and <span style='color:#dd1c1a'>**highest**</span> average rating by decade, published between 1900 and 2021.",
       caption="TidyTuesday Week 4  |  Data from Kaggle by way of Board Games Geek, shared by David and Georgios")
       
# Save plot
ggsave("2022_04.png", width=7, height=4.8)


# Five most common categories (ratings and users) by decade
# clean category 
category = details %>% 
  filter(between(yearpublished, 1970, 2021)) %>% 
  select(id, yearpublished,boardgamecategory) %>%
  mutate(cat = strsplit(as.character(boardgamecategory), ",")) %>% 
    unnest(cat) %>%
  mutate(cat = gsub("\\[|\\]", "", cat),cat = gsub("'", "", cat),cat = gsub("\"", "", cat),
         cat = str_trim(cat, side="both"),
         cat_grp = fct_lump(cat, 5)) %>%
  mutate(decade = floor(yearpublished/10)*10,
         decade = glue::glue("{decade}s")) %>%
  drop_na(cat_grp)
  
# create label (decade, game count and category count )
category_lab = category %>% group_by(decade) %>%
  summarise(n_game = n(),
            n_cat = n_distinct(cat)) %>%
  mutate(lab = glue::glue("{decade} ({n_game} games, {n_cat} categories)")) %>%
  select(decade, lab)
  
# get top 5 categories
category_df = category %>% 
  group_by(decade,cat) %>%
  tally() %>%
  mutate(pct=round(n/sum(n)*100,2),
         cat_rank = dense_rank(desc(n))) %>%
  filter(cat_rank<=5) %>%
  arrange(decade, cat_rank) %>%
  left_join(category_lab, by="decade") %>%
  ungroup() %>%
  select(decade, lab, cat_rank, cat,n, pct)
  
# select variables from ratings.csv for joining 
ratings_join = ratings %>% select(id, average, users_rated)

# summarize rating by category and decade
cr_df = category %>% left_join(ratings_join, by="id") %>%
  group_by(decade, cat) %>%
  summarise(min= min(average),
            median=round(median(average),2),
            max= max(average),
            range=max-min,
            rate_list = list(average),
            user_sum = sum(users_rated),
            .groups="drop"
            ) 
            
# gt table
table = category_df %>% left_join(cr_df, by=c("decade","cat")) %>%
  select(-lab) %>%
  gt(groupname_col = "decade") %>%
  gt_theme_538() %>%
  gt_sparkline(rate_list, type="density", line_color = "#15616d", fill_color = "#d8f3dc") %>%
  fmt_symbol_first(column = pct, suffix = "%") %>%
  cols_label(cat_rank="rank",
             pct="percent",
             cat="category",
             rate_list = "density",
             user_sum = "users sum") %>%
  tab_spanner(label="Average rating",columns=min:user_sum) %>%
  gt_color_rows(columns=pct, palette="ggsci::indigo_material") %>%
  gt_color_rows(columns=median, palette="ggsci::pink_material") %>%
  fmt_number(columns=user_sum, decimals = 0, sep_mark = " ") %>%
  gt_color_rows(columns=user_sum, palette="ggsci::grey_material") %>%
  tab_source_note(source_note =html("<br>TidyTuesday Week 4  |  Data from Kaggle by way of Board Games Geek, shared by David and Georgios")) %>%
  tab_header(title="Board games categories and ratings",
             subtitle="Summary table of the five most common board game category by decade and their ratings, for board games released from 1970 to 2021.")
             
# save table
gtsave_extra(table, "2022_04_p2.png")
