# TidyTuesday week 3, 2022-01-18
# Chocolate Bar Ratings, Data from Flavors of Cacao.
# Citation (Inline boxplot function) : Mock (2020, Oct. 31). The Mockup Blog: Embedding custom HTML in gt tables. Retrieved from https://themockup.blog/posts/2020-10-31-embedding-custom-features-in-gt-tables/

library(tidyverse)
library(gt)
library(gtExtras)

choco<- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

# Get manufacturers with more than 15 reviews
co = choco %>% count(company_manufacturer, sort=T) %>% filter(n>15)

# Table data
co1 = choco %>% 
  filter(company_manufacturer %in% co$company_manufacturer) %>%
  mutate(company_manufacturer = case_when(company_manufacturer=="Smooth Chocolator, The" ~ "The Smooth Chocolator",TRUE~company_manufacturer))

# Boxplot data 
rate_list <- split(co1$rating, co1$company_manufacturer)
rate_rng <- range(co1$rating)

# Box plot function from https://themockup.blog/posts/2020-10-31-embedding-custom-features-in-gt-tables/ 
gt_plot <- function(table_data, column, plot_data, plot_fun, ...){
  text_transform(
    table_data,
    # note the use of {{}} here - this is tidy eval
    # that allows you to indicate specific columns
    locations = cells_body(columns = vars({{column}})),
    fn = function(x){
      plot <- map(plot_data, plot_fun, width = 300, height = 70, same_lim = TRUE, ...)
      plot_svg <- map(plot, "svg_text")
      map(plot_svg, gt::html)
    }
  )
  
 # Table
table1 = co1 %>%
  group_by(company_location,company_manufacturer) %>%
  summarise(n=n(),
            average = round(mean(rating),2),
            min=min(rating),
            median = round(median(rating),2),
            max=max(rating),
            range= max-min,
            histogram=list(rating),
            .groups="drop") %>%
  arrange(desc(average)) %>%
  mutate(boxplot ="",
         n2 = n, 
         #rank = dense_rank(desc(avg))
         ) %>%
  select(company_location, company_manufacturer, n, n2, average, histogram, min, median, max, range, boxplot) %>%
  gt() %>%
  gt_theme_538() %>%
 # histogram
  gt_sparkline(
    histogram,
    type = "histogram",
    line_color = "#7f5539",
    fill_color = "#7f5539",
    bw = .25,
    same_limit = TRUE
  ) %>%
 # boxplot
  gt_plot(
    column = boxplot,  # column to create plot in 
    plot_data = rate_list, # external data to reference
    plot_fun = spec_boxplot,  # which plot fun
    lim = rate_rng # range applied
    ) %>%
 # bar plot
 gt_plt_bar(column=n2, 
             color="#869d7a",
             width=30) %>%
 # format
  gt_merge_stack(company_manufacturer, company_location, colors=c("#7f5539","grey")) %>%
  cols_align(columns = c("histogram", "boxplot", "median"),
             align="center") %>%
  gt_color_rows(columns = c("average","range"),
                palette = "ggsci::brown_material") %>%
  cols_label(company_manufacturer = html("Manufacturer"),
             n=html(""),
             n2=html("N reviewed")) %>%
  tab_spanner(label="Rating", 
              columns=c(average:boxplot)) %>%
  tab_header(title=md("<span style='color:#7f5539'>Ratings of Plain Dark Chocolate Bar</span>"),
             subtitle=md("Summary table of ratings (between 1 to 5) by 22 manufacturers with more than 15 reviews, according to *Flavors of Cacao*.")) %>%
  tab_source_note(source_note = gt::html("<br>#TidyTuesday Week 3  |  Data source: Flavors of Cacao, by way of Georgios and Kelsey  |  Inline boxplot adapted from Tom Mock")) %>%
  tab_style(
    style = list(
      cell_text(
        weight="lighter"
      )
    ),
    locations = list(
      cells_title(groups = "subtitle")
    )
  )  

# Save table  
gtsave(table1,"2022_03.png",expand=50)








