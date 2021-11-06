# TidyTuesday 2021/06: HBCU Enrollment 

## references 
# cleaning: https://twitter.com/alexcookson/status/1356628106790985728/photo/1
	# code: https://gist.github.com/tacookson/b51b707ab2956f143562ae09670db45c)
# cleaning and analysis: https://twitter.com/JDavison_/status/1356603601020473346
	# code: https://jack-davison.github.io/posts/2021-02-02-tidytuesday-2021-week-6-hbcu-enrollment-an-ode-to-data-cleaning/
# dumbell plot with side panel: https://twitter.com/tessuheagle/status/1357004923045629964/photo/1
	# code: https://t.co/LphXBKTKOM?amp=1
# education level bar plot: https://twitter.com/berzinastella/status/1356687610962411521/photo/1
	# code: https://t.co/sab0aj8bc9?amp=1
# highlighted bar plot: https://twitter.com/JackiePsych/status/1356777664405778434/photo/1
	# code: https://t.co/jE5p18S2QR?amp=1


## other references 
# Shiny app:  https://twitter.com/avrodrigues_/status/1357038770676203523  
	# code: https://t.co/CbElJS03YG?amp=1
# Waffle plot with symbols: https://twitter.com/Amit_Levinson/status/1356863886910234624/photo/1
	# code: https://t.co/u3gD7VXXEJ?amp=1
# circular plot with nested plots:  #https://twitter.com/cnicault/status/1357033730905436162/photo/1
	# code: https://t.co/Pqie3OtgA3?amp=1
# arrow plot: https://twitter.com/jamiekerlin_/status/1356807932294258690
	# code: https://t.co/o4o6lP6pgD?amp=1

# metro plot: https://twitter.com/jennagoldd_/status/1357338353507553289/photo/1
	# code: https://t.co/4XOMRBycLk?amp=1
# minimal diverging bar plot: https://twitter.com/Juanma_MN/status/1357057260132917250/photo/1
	# https://t.co/OwpRw3YhQ2?amp=1
# matrix and bar: https://twitter.com/CharlieGallaghr/status/1358142866724446208/photo/1
	# https://t.co/nufN4MWHiL?amp=1
# dot plot by @Tobias: https://twitter.com/toeb18/status/1358062456048910338/photo/1
	# https://t.co/R0B74ojTmz?amp=1
# line moon chart: https://twitter.com/kustav_sen/status/1358335366936207361/photo/1
	# https://t.co/O7Fxg34KT2?amp=1


## last tuesday 
# infographic with map: https://twitter.com/avrodrigues_/status/1354516739866980357
	# code: https://t.co/KgUdGRno5C?amp=1
# map multivariate : https://twitter.com/Juanma_MN/status/1354514922705743877/photo/1
	# code: https://t.co/G3Vfbrjuv1?amp=1
# map multivariate: https://twitter.com/LukefromFuture/status/1354978398523678721
	# code: https://t.co/R97Q058S0B?amp=1
# map with categorical color: https://twitter.com/Luisfreii/status/1358581217004822530/photo/1
	# code: https://t.co/7Bz6vWhkWu?amp=1
# radar plot and dot plot: https://twitter.com/margaretsiple/status/1358643518634250240/photo/1
	# code: https://t.co/L4dTVn48fZ?amp=1
#
	
	
# theme(plot.margin=margin(25,25,10,25))


# libraries 
library(tidyverse)
library(janitor)
library(readxl)
library(glue)
library(wesanderson)
library(ggsci)
library(ggdark)
library(scales)
library(ggpubr)
library(viridis)



# import data (https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-02-02/readme.md)
# student data (https://data.world/nces/high-school-completion-and-bachelors-degree-attainment)
hs_students <- read_excel("104.10.xlsx", sheet = 1)
bach_students <- read_excel("104.10.xlsx", sheet = 2)
male_hs_students <- read_excel("104.10.xlsx", sheet = 3)
male_bach_students <- read_excel("104.10.xlsx", sheet = 4)
female_hs_students <- read_excel("104.10.xlsx", sheet = 5)
female_bach_students <- read_excel("104.10.xlsx", sheet = 6)
# HBCU data (https://data.world/nces/hbcu-fall-enrollment-1976-2015)
hbcu_all <- read_excel("tabn313.20.xls", sheet = 1)
hbcu_black <- read_excel("tabn313.20.xls", sheet = 2)


list(
  datasets = list(
    bach_students,
    female_bach_students,
    female_hs_students,
    hbcu_all,
    hbcu_black,
    hs_students,
    male_bach_students,
    male_hs_students
  ),
  names = ls()
) %>%
  pmap(
    .f = function(datasets, names) {
      write_csv(datasets, glue::glue("{names}.csv"))
    }
  )

# percentage of black-students enrolled in HBCU  
# pivot_longer then left_join 
long_al = hbcu_all %>%  select(Year, `4-year - Public`, `2-year - Public`, `4-year - Private`,`2-year - Private`) %>% 
  pivot_longer(cols = `4-year - Public`:`2-year - Private`) %>% rename(all=value)
long_bl =  hbcu_black %>% select(Year, `4-year - Public`, `2-year - Public`, `4-year - Private`,`2-year - Private`) %>% 
  pivot_longer(cols = `4-year - Public`:`2-year - Private`) %>% rename(black=value)
joined = left_join(long_al, long_bl, by=c('Year',"name"))
joined = joined %>% mutate(prop = black/all)

# plot
joined %>%  ggplot(aes(x = Year, y = prop, color = name)) +
  geom_line() + 
  geom_point(size=1, alpha=0.7) +
  scale_color_manual(values=c("#ECCBAE", "#59a5d8", "#D69C4E", "#ABDDDE")) +
  dark_theme_minimal() + 
  scale_x_continuous(limits=c(1980,2015)) +
  scale_y_continuous(labels=scales::percent) +
  labs(
  	color="", 
  	y="Percentage", 
  	title= "Percentage of black-student enrollments in HBCUs ",
  	subtitle = "Fall enrollment of degree-granting historically black colleges and universities from 1980 to 2015",
  	caption="Data source: Data.World"
  	) + 
  theme(plot.subtitle= element_text(size=8),
  		plot.caption=element_text(size=8, face="italic"),
  		axis.title= element_text(size=8),
  		plot.title= element_text(size=12),
  		plot.background=element_rect(fill="#143642"),
  		panel.grid.major= element_line(color="#263c41"),
  		panel.grid.minor= element_line(color="#263c41"))
  		
# high school students racial/ ethic group
hs_students %>% 
  mutate(Total = if_else(Total > 10000, str_sub(Total, 1, 4) %>% as.double(), Total)) %>% 
  rename(year = Total) %>% 
  select(!contains("Standard")) %>% 
  select(!contains("Total")) %>% 
  mutate(across(White1:last_col(), as.double)) %>% 
  pivot_longer(cols = 2:last_col(), names_to = "group", values_to = "percentage") %>% 
  filter(year >= 2000) %>% 
  ggplot(aes(x = year, y = percentage, color = group)) +
  scale_color_discrete(labels=function(x) str_wrap(x, width=13)) +
  geom_line() +
  theme(legend.position="bottom")

# high school student age 25 and above
hs_students %>%
	filter(between(Total, 1985, 2021)) %>% 
	rename(year=Total, age25= "Total, percent of all persons age 25 and over") %>%
	mutate(age25_prop = age25/100) %>%
	ggplot(aes(x=year, y=as.double(age25_prop))) + 
	geom_line() +
	geom_point() + 
	scale_y_continuous(labels=scales::percent, limits=c(0.7,0.95)) + 
	scale_x_continuous(breaks=seq(1985,2020,5)) +
	dark_theme_grey() 

# hbcu_all male vs female 1
hbcu_all %>%
	select(Year, Males, Females) %>%
	pivot_longer(cols=2:3, names_to="Gender", values_to ="Enrollments") %>%
	mutate(Gender = str_remove(Gender, "s$")) %>%
	filter(Year>1990) %>%
	ggplot(aes(x=Year, y=Enrollments)) +
	geom_point(aes(color=Gender, shape=Gender), size=2.5) +
	theme_minimal() + 
	scale_y_continuous(limits=c(90000,210000)) +
	scale_color_jama() + 
	theme(legend.position="top") +
	labs(color="",shape="")

# male vs female enrollments: cleveland plot
all2 = hbcu_all %>%
	select(Year, Males, Females) %>%
	pivot_longer(cols=2:3, names_to="Gender", values_to ="Enrollments") %>%
	mutate(Gender = str_remove(Gender, "s$")) %>%
	filter(Year>1990)
	
all2 %>%
	ggplot(aes(x=Enrollments, y=Year)) +
	geom_point(aes(color=Gender), size=2.5) +
	geom_line(aes(group=Year), color="#979dac") +
	geom_text(data = filter(hbcu_all, Year == 2015), aes(x = Males, y = Year, label="Males"), color="#0f4c5c", size=3.2, vjust=-2, fontface="bold") +
  geom_text(data = filter(hbcu_all, Year == 2015), aes(x = Females, y = Year, label="Females"), color="#fb8b24", size=3.2, vjust=-2, fontface="bold") +
	scale_x_continuous(limits=c(100000,210000), breaks=seq(100000,210000,20000)) +
	scale_y_continuous(limits=c(1990,2016), breaks=seq(1990,2016,5)) +
	theme_minimal() +
	theme(legend.position="none",
		panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
	scale_color_jco("#fb8b24","#0f4c5c")) + 
	labs(y="",
		title="Student Enrollments at Historically Black Colleges and Universities",
		plot.subtitle="") 

# male vs female enrollments: cleveland plot with side panel
# reference: 
library(ggalt) 
library(ggtext)

df <- hbcu_all %>%
  select(1:4)
df$percFem = (100*(df$Females / df$`Total enrollment`))
df <- df %>% filter(Year>=1990)
df$Males = df$Males/1000
df$Females = df$Females/1000

ggplot(df, aes(y = Year, x = Males, xend = Females))+
  geom_segment(data = df, aes(y = Year, yend = Year, x = min(Males), xend = max(Females) * 1.1), color = "#e0e1dd", alpha = .75)+
  geom_dumbbell(size= 0.5, size_x = 2, size_xend = 2, colour_x = "#0091ad", colour_xend = "#a01a58", colour = "#979dac")+
  geom_text(data = filter(df, Year == 2015), aes(x = Males, y = Year, label="Males"), color="#0091ad", size=3.2, vjust=-1, fontface="bold")+
  geom_text(data = filter(df, Year == 2015), aes(x = Females, y = Year, label="Females"), color="#a01a58", size=3.2, vjust=-1, fontface="bold")+
  labs(
    y = "",
    x = "Total Enrollment (in thousands)",
    title = "Enrollment at Historically Black Colleges and Universities")+
  scale_x_continuous(breaks = seq(100, 200, 20))+
  scale_y_continuous(labels = seq(1975, 2015, 5), breaks = seq(1975, 2015, 5))+
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 10, color = "black"),
    axis.title.x = element_text(size = 10, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    plot.title = element_text(size = 13, hjust = .5))+
  geom_rect(data = df, aes(xmin = max(Females) * 1.09, xmax = max(Females) * 1.13, ymin = -Inf, ymax = Inf), fill = "#f0efeb")+
  geom_text(data = df, aes(label = paste0(round(percFem), "%"), y = Year, x = max(Females) * 1.11, color=percFem), fontface = "bold", size = 2.5) + theme(legend.position="none") + scale_color_gradient(low="#5c4d7d", high="#b7094c") + 
  geom_text(data = filter(df, Year == 2015), aes(x = max(Females) * 1.244, y = Year, label = "% F"), color = "black", size = 2.5, vjust = -1.3, fontface = "bold") 



	
# female and male education level 
# inspired by: https://twitter.com/berzinastella/status/1356687610962411521/photo/1

# female data: select and merge 
fhs = female_hs_students[c(1:6,12, 22 ), 1:2]
fba = female_bach_students[c(1:6,12, 22 ), 1:2]
fdata = merge(fhs, fba, by="Total")
names(fdata)[1]<-"year"
names(fdata)[2] <- "highschool"
names(fdata)[3] <- "collage"
# plot
p1 = fdata %>% 
	mutate(high_school = highschool-collage, 
		no_high_school= 100-highschool) %>%
	rename(high_school_and_college = collage) %>%
	pivot_longer(cols=3:5, names_to="Education", values_to="Percentage") %>%
	mutate(Education= fct_relevel(Education,"no_high_school","high_school", "high_school_and_college"), 
		Percentage=Percentage/100) %>%
	ggplot(aes(x=year, y=Percentage, fill=Education)) +
	geom_col() + 
	scale_y_continuous(labels=scales::percent) + 
	scale_x_continuous(breaks=seq(1940,2010,10)) + 
	scale_fill_uchicago() + 
	theme_minimal()	+
	theme(legend.position="bottom", 
		plot.title=element_text(hjust=0.5),
		panel.grid.minor=element_blank()) +
	labs(title="Female")

# male data: select and merge 
mhs = male_hs_students[c(1:6,12, 22 ), 1:2]
mba = male_bach_students[c(1:6,12, 22 ), 1:2]
mdata = merge(mhs, mba, by="Total")
names(mdata)[1]<-"year"
names(mdata)[2] <- "highschool"
names(mdata)[3] <- "collage"
# plot
p2 = mdata %>% 
	mutate(high_school = highschool-collage, 
		no_high_school= 100-highschool) %>%
	rename(high_school_and_college = collage) %>%
	pivot_longer(cols=3:5, names_to="Education", values_to="Percentage") %>%
	mutate(Education= fct_relevel(Education,"no_high_school","high_school", "high_school_and_college"), 
		Percentage=Percentage/100) %>%
	ggplot(aes(x=year, y=Percentage, fill=Education)) +
	geom_col() + 
	scale_y_continuous(labels=scales::percent) + 
	scale_x_continuous(breaks=seq(1940,2010,10)) + 
	scale_fill_uchicago() + 
	theme_minimal()	+
	theme(legend.position="bottom", 
		plot.title=element_text(hjust=0.5),
		panel.grid.minor=element_blank()) +
	labs(title="Male")

# combined plot
ggarrange(p1,p2, ncol=2, common.legend=TRUE)



# highlighted bar chart 
# reference: https://t.co/jE5p18S2QR?amp=1
# prepare data
highlight = hbcu_all %>% clean_names() %>% filter(year>=2000) %>% select(year, total_enrollment) %>% mutate(total_enr=total_enrollment/1000)
# values to highlight
maxEnr = max(highlight$total_enr)
# new var to highlight max
highlight = highlight %>% mutate(tohighlight=if_else(total_enr== maxEnr,"yes","no"))
# plot
highlight %>% ggplot(aes(x=year, y= total_enr, fill= tohighlight)) + 
	geom_bar(stat="identity") +
	geom_text(aes(label= round(total_enr,0)), size=3, color="white",vjust=2, hjust=0.5, nudge_y=0.5) +
	scale_fill_manual(values=c("yes"="#1a936f", "no"="#114b5f", guide=FALSE)) +
	labs(title="HBCU Total Enrollment (in thousands)") +
	scale_x_continuous(breaks=seq(2000,2015,1)) + 
	scale_y_continuous(expand=c(0,0)) + 
	theme_minimal()+
	theme(legend.position="none",
		panel.grid=element_blank(),
		axis.text.y=element_blank(),
		plot.title=element_text(hjust=0.5)) + 
	labs(x="", y="")
	











# clean data 
# reference: https://gist.github.com/tacookson/b51b707ab2956f143562ae09670db45c)  	
hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv') %>% clean_names()
# gender breakdown
hbcu_by_gender <- hbcu_all %>%
  select(year, males, females) %>%
  pivot_longer(males:females,
               names_to = "gender",
               values_to = "students") %>%
  mutate(gender = str_remove(gender, "s$"))
# Program breakdown
hbcu_by_program <- hbcu_all %>%
  # We need fields with "public" or "private" in the name
  # (They also have 2- vs 4-year)
  # We DON'T need fields with "total" in the name, since this is redundant
  select(year,
         contains(c("public", "private")),
         -contains("total")) %>%
  # names_pattern argument does the heavy lifting
  # It separates names into groups, as specified by parentheses "(group)"
  # Field names are structured so that program length is followed by public/private
  # We also specify "x_" as an optional argument using regular expressions
  pivot_longer(cols = x4_year_public:x2_year_private,
               names_pattern = "[x_]?(.*)_(.*)",
               names_to = c("program_length", "public_private"),
               values_to = "students") %>%
  # parse_number() finds the number (i.e., 2 or 4) in program_length and discards the rest
  # Then we use paste() to add "years" on the end
  # It's a sorta-convoluted way to get rid of the underscores and extra bits
  mutate(program_length = paste(parse_number(program_length), "years"))




# clean data and analysis
# reference: Davison (2021, Feb. 2). Jack Davison R: An Ode to Data Tidying (TidyTuesday 2021 Week 6: HBCU Enrolment). Retrieved from https://jack-davison.github.io/posts/2021-02-02-tidytuesday-2021-week-6-hbcu-enrollment-an-ode-to-data-cleaning/

# clean data
tuesdata$bach_students %>%
  mutate(across(everything(), as.numeric)) %>% # change to numeric
  pivot_longer(-Total) %>% # restructure
  filter(!str_detect(name, "Total - Asian/Pacific Islander")) %>% #remove duplicated  
  mutate(name = str_remove(name, "Asian/Pacific Islander - |1")) %>% #remove - and 1
  separate(name, into = c("stat","race"), sep = " - ", fill = "left") %>% #separate
  pivot_wider(names_from = stat, values_from = value) %>% #total and se in sep col
  janitor::clean_names() %>% #clean names 
  mutate(
    race = str_remove_all(
      race, 
      ", percent of all persons age 25 and over|\r\n")
  )

# data analysis
# Bachelor's Degree Attainment 
ggthemr::ggthemr("flat", text_size = 14)

plot_data = df %>%
  drop_na() %>%
  mutate(across(total:standard_errors, ~.x/100))

plot_data %>%
  filter(race != "Total") %>%
  mutate(race = fct_reorder(race, total, max, na.rm = T),
         race = fct_rev(race)) %>%
  ggplot(aes(
    year,
    y = total,
    ymax = total + standard_errors,
    ymin = total - standard_errors,
    group = race
  )) +
  geom_ribbon(aes(fill = race), alpha = .25) +
  geom_line(aes(color = race)) +
  geom_line(data = plot_data %>% filter(race == "Total"), size = 2, color = ggthemr::swatch()[1]) +
  scale_color_manual(values = ggthemr::swatch()[-1]) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = NULL, color = "Race", fill = "Race",
       title = "Bachelor's Degree Attainment",
       subtitle = "The percentage of the population who have achieved a\nbachelor's degree in the US since 1980, split into racial groups.\nThe bold line represents the total population.") +
  theme(plot.title.position = "plot")

# Bachelor's Degree Attainment in 2016
plot_data_2 = plot_data %>%
  filter(year == max(year)) %>%
  mutate(race = fct_reorder(race, total))

tot = plot_data_2 %>%
  filter(race == "Total") %>% 
  pull(total)

plot_data_2 %>%
  mutate(flag = case_when(race == "Total" ~ "T",
                          total > tot ~ "Y",
                          total < tot ~ "X")) %>%
  ggplot(aes(y = race, x = total, xmax = total+standard_errors, xmin = total-standard_errors, fill = flag)) +
  geom_col(show.legend = F) +
  geom_vline(xintercept = tot, color = ggthemr::swatch()[1], size = 1) +
  geom_pointrange(show.legend = F) +
  scale_x_continuous(expand = expansion(mult = c(0,.1)), labels = scales::percent) +
  scale_fill_manual(values = ggthemr::swatch()[c(1,2,3)]) +
  labs(y = NULL, x = "Bachelor's Degree Attainment in 2016")

# table
plot_data_2 %>%
  janitor::remove_constant() %>%
  mutate(mutate(across(where(is.numeric), ~glue::glue("{abs(.x) * 100} %")))) %>%
  knitr::kable(col.names = c("Race", "Total", "Std Err.")) %>%
  kableExtra::kable_styling()




# Female enrollment in HBCUs (https://twitter.com/jamiekerlin_/status/1356807932294258690)
# code: https://raw.githubusercontent.com/jamiekerlin/tidytuesday/master/Scripts/2021_02_02.R
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(scico)

#Load data
data <- tidytuesdayR::tt_load('2021-02-02')
hbcu_all <- data$hbcu_all
hbcu_black <- data$hbcu_black

#Rename columns
hbcu_all <- hbcu_all %>%
  rename("total_enrollment" = 'Total enrollment', "males" = 'Males',
         "females" = 'Females', "four_year" = '4-year', "two_year" = '2-year',
         "total_public" = 'Total - Public', "four_year_public" = '4-year - Public',
         "two_year_public" = '2-year - Public', "total_private" = 'Total - Private',
         "four_year_private" = '4-year - Private', "two_year_private" = '2-year - Private',
         "year" = 'Year')

#Create columns with percent of group enrollment for total enrollment
hbcu_percentenrollment <- hbcu_all %>%
  mutate(percent_male = (males/total_enrollment) * 100, 
         percent_female = (females/total_enrollment) * 100, 
         percent_two_year_all = (two_year/total_enrollment) * 100, 
         percent_four_year_all = (four_year/total_enrollment) * 100,
         percent_public_all = (total_public/total_enrollment) * 100, 
         percent_private_all = (total_private/total_enrollment) * 100)

#Create columns with percent change of male/female enrollment of total 
#enrollment over years
hbcu_percentchange <- hbcu_percentenrollment %>%
  mutate(pct_change_female = (percent_female - lag(percent_female)),
         pct_change_male = (percent_male - lag(percent_male)))

#Plot
#Following blog post steps by Otho Mantegazza 
#https://otho.netlify.app/

#3 different plots... 
#Plot 1: shows female enrollment in tens of thousands and arrows show
#change in percent of female enrollment of TOTAL enrollment between years
#Plot 2: shows female enrollment in tens of thousands and arrows show
#change in percent of female enrollment between years
#Plot 3: shows percent of female enrollment of total enrollment and arrows
#show percent change of female enrollment of total enrollment between years
#I shared Plot 2 to twitter

################################################################
#Plot of female enrollment and percent change

hbcu_percentchange <- hbcu_percentchange %>%
  mutate(females_10000 = females/10000)

#Change color palette & center divergent palette
lim <- 
  hbcu_percentchange$pct_change_female %>% 
  range() %>% 
  abs() %>% 
  max()

#Setting theme
theme_set(
  theme_minimal() +
    theme(text = element_text(family = "Arial Narrow",
                              colour = "grey40",
                              size = 11),
          axis.title = element_text(size = 14),
          plot.title = element_text(colour = "grey20",
                                    face = "bold",
                                    size = 18),
          plot.subtitle = element_text(face = "bold",
                                       size = 12),
          aspect.ratio = .6,   
          plot.margin = margin(t = 10, r = 15, b = 0, l = 10,
                               unit = "mm"))
)


#Set basic aesthetic mapping

female_enrollment_plot <-hbcu_percentchange %>%
  mutate(yend = females_10000 + (pct_change_female)) %>%
  ggplot(aes(x = year, y = females_10000)) +
  geom_segment(aes(yend = yend, #Add first geometric objects
                   xend = ..x..,
                   colour = pct_change_female),
               size = 2, arrow = arrow(length = unit(3, "mm"),
                                       type = "closed")) +
  geom_point(colour = "grey40", size = 2) +
  #Add text to specify percent changes
  geom_text(nudge_y = 1, aes(y = case_when(pct_change_female > 0 ~ yend + .5,
                                                        TRUE ~ yend - 2.5),
                                          label = pct_change_female %>%
                                            round(digits = 1) %>% paste0("%"),
                                          colour = pct_change_female),
            angle = 90,
            size = 3.5) +
  scale_colour_scico(palette = "roma",
                     direction = 1,
                     limits = c(-lim, lim) * 
                       max(abs(hbcu_percentchange$pct_change_female)),
                     guide = FALSE) +
  #Add labels/title
  labs(title = "Female Enrollment in HBCUs",
       subtitle = str_wrap("Between 1976 and 2015, with total HBCU enrollment
       (in tens of thousands) and percent change compared to the previous year of data."),
       y = "Females Enrolled (Tens of thousands)",
       x = "Year")

female_enrollment_plot
############################################################

#Plot of female enrollment and percent change
#OF JUST WOMEN ENROLLMENT

hbcu_percentchange <- hbcu_percentchange %>%
  mutate(females_10000 = females/10000)

hbcu_percentchange <- hbcu_percentchange %>%
  mutate(pct_change = (females_10000/lag(females_10000) - 1) * 100)

#Change color palette & center divergent palette
lim <- 
  hbcu_percentchange$pct_change %>% 
  range() %>% 
  abs() %>% 
  max()

#Setting theme
theme_set(
  theme_minimal() +
    theme(text = element_text(family = "Arial Narrow",
                              colour = "grey40",
                              size = 11),
          axis.title = element_text(size = 14),
          plot.title = element_text(colour = "grey20",
                                    face = "bold",
                                    size = 18),
          plot.subtitle = element_text(face = "bold",
                                       size = 12),
          aspect.ratio = .6,   
          plot.margin = margin(t = 10, r = 15, b = 0, l = 10,
                               unit = "mm"))
)


#Set basic aesthetic mapping

female_enrollment_plot <-hbcu_percentchange %>%
  mutate(yend = females_10000 + (pct_change)) %>%
  ggplot(aes(x = year, y = females_10000)) +
  geom_segment(aes(yend = yend, #Add first geometric objects
                   xend = ..x..,
                   colour = pct_change),
               size = 2, arrow = arrow(length = unit(3, "mm"),
                                       type = "closed")) +
  geom_point(colour = "grey40", size = 2) +
  #Add text to specify percent changes
  geom_text(nudge_y = 1, aes(y = case_when(pct_change > 0 ~ yend + .5,
                                           TRUE ~ yend - 2.5),
                             label = pct_change %>%
                               round(digits = 1) %>% paste0("%"),
                             colour = pct_change),
            angle = 90,
            size = 3.5) +
  scale_colour_scico(palette = "roma",
                     direction = 1,
                     limits = c(-lim, lim) * 
                       max(abs(hbcu_percentchange$pct_change)),
                     guide = FALSE) +
  #Add labels/title
  labs(title = "Female Enrollment in HBCUs",
       subtitle = str_wrap("Between 1976 and 2015, with total HBCU enrollment
       (in tens of thousands) and percent change compared to the previous year of data."),
       y = "Females Enrolled (Tens of thousands)",
       x = "Year")

female_enrollment_plot

############################################################
#Plot of percent of total enrollment and percent change 

#Change color palette & center divergent palette
lim <- 
  hbcu_percentchange$pct_change_female %>% 
  range() %>% 
  abs() %>% 
  max()

#Setting theme
theme_set(
  theme_minimal() +
    theme(text = element_text(family = "Arial Narrow",
                              colour = "grey40",
                              size = 11),
          axis.title = element_text(size = 14),
          plot.title = element_text(colour = "grey20",
                                    face = "bold",
                                    size = 18),
          plot.subtitle = element_text(face = "bold",
                                       size = 12),
          aspect.ratio = .6,   
          plot.margin = margin(t = 10, r = 15, b = 0, l = 10,
                               unit = "mm"))
)


#Set basic aesthetic mapping

pct_change_plot <-hbcu_percentchange %>%
  mutate(yend = percent_female + (pct_change_female)) %>%
  ggplot(aes(x = year, y = percent_female)) +
  geom_segment(aes(yend = yend, #Add first geometric objects
                   xend = ..x..,
                   colour = pct_change_female),
               size = 2, arrow = arrow(length = unit(1.5, "mm"),
                                       type = "closed")) +
  geom_point(colour = "grey40", size = 2) +
#Add text to specify percent changes
  geom_text(nudge_x = .5, nudge_y = .5, aes(y = case_when(pct_change_female > 0 ~ yend + .5,
                              TRUE ~ yend - 1.2),
                label = pct_change_female %>%
                  round(digits = 1) %>% paste0("%"),
                colour = pct_change_female),
            angle = 90,
            size = 3.5) +
  scale_colour_scico(palette = "roma",
                     direction = 1,
                     limits = c(-lim, lim) * 
                       max(abs(hbcu_percentchange$pct_change_female)),
                     guide = FALSE) +
#Add labels/title
  labs(title = "Female Enrollment in HBCUs",
       subtitle = str_wrap("Between 1976 and 2015, with percent of total 
                           HBCU enrollment and percent change compared to the 
                           previous year of data."),
       y = "Female Percentage of Total Enrollment",
       x = "Year")

pct_change_plot







	

	
	
	

