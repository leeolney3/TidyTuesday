# 2022-08-16
# TidyTuesday week 33, data from Open-Source Psychometrics Project courtesy of Tanya Shapiro

# Load libraries
library(tidyverse)
library(gt)
library(gtExtras)

# Load data
characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/characters.csv')
psych_stats <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/psych_stats.csv")

# Table
selected_questions = psych_stats %>% count(question) %>% mutate(id=row_number()) %>%
  filter(between(id,2,36)) %>%
  filter(id %in% c(2,3,5,22:23,25,28:31))
  
show = characters %>% filter(uni_name=="Futurama")

psych_stats %>% filter(uni_name=="Futurama", question %in% selected_questions$question) %>%
  left_join(show %>% select(id, image_link), by=c("char_id"="id")) %>%
  select(image_link,char_name, question, personality) %>%
  pivot_wider(names_from = question, values_from = personality) %>%
  arrange(char_name) %>%
  gt() %>%
  gt_img_rows(column=image_link,img_source = "web") %>%
  cols_label(image_link="",
             char_name="Character") %>%
  cols_width(3:12~px(60),
             2~px(150)) %>%
  tab_options(column_labels.font.size = px(16),
              table.font.size = 16,
              table.background.color = "#fafafa") %>%
  cols_align(align="center", columns=3:12) %>%
  tab_spanner(columns=3:12, label="Personality Question") %>%
  tab_style(style = cell_text(size = px(13.5), color = "grey30", weight=300,transform = "uppercase"), 
        locations = cells_column_spanners()) %>%
  tab_style(style = cell_text(size = px(13.5), weight=300, color = "grey30", transform = "uppercase"), 
        locations = cells_column_labels(columns=char_name)) %>%
  tab_style(style = cell_text(size = px(14), weight=300, color = "grey30"), 
        locations = cells_source_notes()) %>%
  tab_style(style=cell_text(font = google_font("Libre Franklin"),transform = "uppercase", weight=700, size=px(20)),
            locations=cells_title(groups="title")) %>%
  tab_style(style=cell_text(size=px(14.5), weight=300),
            locations=cells_title(groups="subtitle")) %>%
  tab_style(style=cell_text(weight=400, size=px(15)),
            locations=cells_body(columns = char_name)) %>%
  opt_table_font(font = list(google_font("Lato"), default_fonts())) %>%
  tab_header(title="Futurama Characters Personalities",
             subtitle="Personalities of ten Futurama characters from 10 (out of 400) personality questions, arranged in alphabetical order of character name.") %>%
  tab_source_note(source_note = md("TidyTuesday week 23<br>Data from Open-Source Psychometrics Project courtesy of Tanya Shapiro")) %>%
  gtsave("2022_33.png")