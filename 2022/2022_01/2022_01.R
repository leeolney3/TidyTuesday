#TidyTuesday Week 1 2021-01-04 Bring your own data to start 2022!
#Data source: https://ourworldindata.org/grapher/production-consumption-energy-per-person?country=~SWE

library(tidyverse)
library(lubridate)
library(gt)
library(gtExtras)

# Import data
df = read_csv("data/production-consumption-energy-per-person.csv",
              show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  mutate(entity = case_when(entity =="Czechia" ~ "Czech Republic",
                            TRUE~ entity))
                            
skimr::skim(df) 

# Wrangle 
nord = c("Denmark","Norway","Sweden","Finland")
df1 = df %>% filter(entity %in% nord) %>%
  rename(consumption = consumption_per_capita,
              production = production_per_capita) 
              
ab =  df1 %>% pivot_longer(4:5, values_to="absolute")

rel = df1 %>%
  filter(entity %in% nord) %>%
  group_by(entity) %>%
  arrange(year, .by_group=TRUE) %>%
  mutate(consumption = (consumption/lag(consumption) - 1),
         production = (production/lag(production) - 1)) %>%
  replace(is.na(.), 0) %>%
  pivot_longer(4:5, values_to="relative")
  
data = ab %>% 
  left_join(rel) %>% 
  select(-code) %>% 
  mutate(rank= case_when(entity=="Finland" ~ 1,
                         entity=="Norway" ~ 2,
                         entity=="Sweden" ~ 3,
                         entity=="Denmark" ~4))
                         
# Line plot function
# code adapted from: https://twitter.com/BjnNowak/status/1472562727684124688
fun_plot <- function(data){
  trend <- ggplot(data,aes(x=year,y=absolute, color=fct_rev(name)))+
    geom_line(size=3.5) +
    scale_color_manual(values=c("#009A88","#EE7733")) +
    scale_y_continuous(limits=c(40,100), breaks=seq(40,100,20)) +
    coord_cartesian(clip="off") +
    cowplot::theme_minimal_grid(35) +
    theme(legend.position = "none",
          axis.title=element_blank())
  return(trend)
}

line_plot = data %>%
  group_by(rank) %>%
  nest() %>%
  mutate(gg=purrr::map(data, fun_plot)) %>%
  select(rank = rank, gg) %>%
  arrange(rank)
              
# Line plot function (relative change)
fun_plot2 <- function(data){
  trend <- ggplot(data,aes(x=year,y=relative, color=fct_rev(name)))+
    geom_line(size=3.5) +
    scale_color_manual(values=c("#009A88","#EE7733")) +
    scale_y_continuous(limits=c(-.25,.25),label=scales::percent, breaks=c(-.2,0,.2)) +
    coord_cartesian(clip="off") +
    cowplot::theme_minimal_grid(35) +
    theme(legend.position = "none",
          axis.title=element_blank())
  return(trend)
}

line_plot2 = data %>%
  group_by(rank) %>%
  nest() %>%
  mutate(gg=purrr::map(data, fun_plot2)) %>%
  select(rank = rank, gg) %>%
  arrange(rank)

# Base table data  
base = df1 %>% 
  filter(year==2020) %>% 
  mutate(fl=case_when(entity=="Denmark"~"https://hatscripts.github.io/circle-flags/flags/dk.svg",
                        entity=="Finland"~"https://hatscripts.github.io/circle-flags/flags/fi.svg",
                        entity=="Norway"~"https://hatscripts.github.io/circle-flags/flags/no.svg",
                        entity=="Sweden"~"https://hatscripts.github.io/circle-flags/flags/se.svg")) %>%
  select(fl, entity, production, consumption) 
  
# {gt} table
gt1 = base %>% 
  arrange(desc(production)) %>%
  mutate(absolute = NA, 
         relative = NA) %>%
  gt() %>%
  gt_theme_538() %>%
  # Add country flag 
  gt_img_rows(fl) %>%
  # Add line plot 
  gt::text_transform(
    locations = cells_body(columns=absolute),
    fn = function(x){
      purrr::map(
        line_plot$gg, gt::ggplot_image, 
        height = px(100), aspect_ratio = 1.6
  )}) %>%
  gt::text_transform(
    locations = cells_body(columns=relative),
    fn = function(x){
      purrr::map(
        line_plot2$gg, gt::ggplot_image, 
        height = px(100), aspect_ratio = 1.7
  )}) %>% 
  # Spanner
  tab_spanner(label="2020", columns=production:consumption) %>%
  tab_spanner(label="1995-2020", columns=absolute:relative) %>%
  # Column labels
  cols_label(
    production = html("<span style = 'color:#009A88;'>Production</span>"),
    consumption = html("<span style = 'color:#EE7733;'>Consumption</span>"),
    absolute = html("Trend"),
    relative = html("Relative change"),
    fl = html(""),
    entity=html("Country")
    ) %>%
  # Format numbers
  fmt_symbol_first(column = c("production","consumption"), suffix = " MWh") %>%
  # Format first column text 
  tab_style(
    style=list(cell_text(weight="normal")),
    location=cells_body(columns=entity)
  ) %>%
  # Header and source note
  tab_header(title="Production-based vs. consumption-based energy use per person",
             subtitle=md("Consumption-based (trade-adjusted) energy use measures domestic energy use minus energy used to produce exported goods, plus energy used to produce imported goods, in megawatt-hour (MWh).")) %>%
  tab_source_note(source_note = gt::html("Data source: ourworldindata.org<br>#TidyTuesday Week 1")) %>%
  # Adjust sub-title font
  tab_style(
    style = list(
      cell_text(
        weight="lighter"
      )
    ),
    locations = list(
      cells_title(groups = "subtitle")
    )
  )  %>%
  # Adjust source note font size
  tab_options(source_notes.font.size = "13px") %>%
  # Align column 
  cols_align(columns = c(1,3,4),
             align = "center") %>%
  cols_width(fl~px(25)) 
  
gt1