# 2022-08-16
# TidyTuesday week 33, data from Open-Source Psychometrics Project courtesy of Tanya Shapiro

# Load libraries
library(tidyverse)
library(gt)
library(gtExtras)

# Load data
characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/characters.csv')
psych_stats <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/psych_stats.csv")

# {gt} table
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
  
# {reactable} table
library(reactable)
library(reactablefmtr)

psych_stats1= psych_stats %>% filter(uni_name=="Futurama", question %in% selected_questions$question) %>%
  left_join(show %>% select(id, image_link), by=c("char_id"="id")) %>%
  select(image_link,char_name, question, personality,avg_rating) %>%
  mutate(avg_rating=(avg_rating-50)*2/100)

pal = ggsci::rgb_material("blue-grey")[1:8]
f <- colorRamp(pal)
col=(colors <- rgb(f(psych_stats1$avg_rating)/255))

psych_stats1$avg_rating=col 

data = psych_stats1 %>% pivot_wider(names_from = question,values_from = c(personality,avg_rating)) %>%
   rename_with(~ str_remove(., "personality_"), everything())  %>%
  arrange(char_name) 
vec = colnames(data)

data1=  data %>% `colnames<-`(seq(1,22,1))
data1= data1 %>% mutate(col=c(.5,.6,.7,.8,.9,1,.5,.6,.7,.8)) # add column for legend

reactable(data1, theme = fivethirtyeight(centered = TRUE, header_font_size = 11),
          defaultColDef = colDef(minWidth = 45, align="center",sortable = FALSE),
          columnGroups = list(
            colGroup(name="Personality Question",
                     columns=as.character(seq(3,12,1)))
          ),
          columns=list(
            `1`=colDef(name = "", maxWidth = 32,style = background_img()),
            `2`=colDef(name = "Character",minWidth = 200,align="left",style = list(borderRight = "1px solid #777")),
            `3`=colDef(name = vec[3],cell=color_tiles(data1,color_ref = "13")),
            `4`=colDef(name = vec[4],cell=color_tiles(data1,color_ref = "14")),
            `5`=colDef(name = vec[5],cell=color_tiles(data1,color_ref = "15")),
            `6`=colDef(name = vec[6],cell=color_tiles(data1,color_ref = "16")),
            `7`=colDef(name = vec[7],cell=color_tiles(data1,color_ref = "17")),
            `8`=colDef(name = vec[8],cell=color_tiles(data1,color_ref = "18")),
            `9`=colDef(name = vec[9],cell=color_tiles(data1,color_ref = "19")),
            `10`=colDef(name = vec[10],cell=color_tiles(data1,color_ref = "20")),
            `11`=colDef(name = vec[11],cell=color_tiles(data1,color_ref = "21")),
            `12`=colDef(name = vec[12],cell=color_tiles(data1,color_ref = "22")),
            `13`=colDef(show=FALSE),
            `14`=colDef(show=FALSE),
            `15`=colDef(show=FALSE),
            `16`=colDef(show=FALSE),
            `17`=colDef(show=FALSE),
            `18`=colDef(show=FALSE),
            `19`=colDef(show=FALSE),
            `20`=colDef(show=FALSE),
            `21`=colDef(show=FALSE),
            `22`=colDef(show=FALSE),
            col= colDef(show=FALSE,cell=color_tiles(data1,pal)) #check color scales
            )) %>%
  google_font(font_family = "Lato") %>%
  add_legend(data = data1,
             col_name = "col",
             title="Average score",
             number_fmt= scales::label_number(scale=100),
             align="left",
             colors=pal) %>%
  add_title("Futurama Characters Personalities", margin = margin(0, 0, 8, 0), font_size = 22) %>% 
  add_subtitle("Personalities of ten Futurama characters from 10 (out of 400) personality questions, arranged in alphabetical order of character name.", align="left", font_size = 13, font_weight = "normal", font_color = "grey30",margin = margin(0, 0, 10, 0)) %>%
  add_source("TidyTuesday week 33 •  Data from Open-Source Psychometrics Project courtesy of Tanya Shapiro", font_size = 12, font_color = "grey30")  