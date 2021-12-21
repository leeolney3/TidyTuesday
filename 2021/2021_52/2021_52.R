# TidyTuesday Week 52 2021-12-21
# Starbucks drinks, data from PythonCoderUnicorn and @StarTrek_Lt

library(tidyverse)
library(ggtext)
library(ggchicklet)

library(showtext)
font_add_google("Lato","lato")
showtext_auto()
f2 = "lato"

# import data
starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')

# caffine and caffine/ml by serving size and product name
# wrangle
df = starbucks %>% 
  mutate(product_name= str_to_lower(product_name)) %>%
  filter(str_detect(size, "short|tall|grande|venti"),
         str_detect(product_name, "iced|cold|frappuccino"),
         milk==0|milk==5,
         whip==0) %>%
  filter(!(serv_size_m_l==591)) %>%
  filter(!(product_name=="iced caffè mocha" & (caffeine_mg==140|caffeine_mg==75|caffeine_mg==110))) %>%
  select(product_name, size, serv_size_m_l, caffeine_mg)

df_ad = tribble(
  ~product_name, ~ size, ~serv_size_m_l, ~caffeine_mg,
  "iced black tea lemonade", "tall", 354, 20, 
  "iced caramel macchiato", "venti", 709, 225,
)

df1 = bind_rows(df_ad, df) %>%
  mutate(mg_ml = caffeine_mg/serv_size_m_l) %>%
  filter(mg_ml>0) %>%
  mutate(product_name=str_to_title(product_name),
         product_name = fct_reorder(product_name, caffeine_mg, .fun=max),
         size=factor(size, levels=c("venti","grande","tall"))) %>%
  filter(!(str_detect(product_name, "tea"))) 

# plot
p1 = df1 %>%
  arrange(desc(caffeine_mg)) %>%
  ggplot(aes(product_name, caffeine_mg, fill=size)) +
  geom_chicklet(position="identity", color="white", size=.2, show.legend = F, alpha=.92) +
  scale_fill_manual(values=c("#0B5232","#E5BEAE","#8F6404")) + #starbucks fall color palette
  geom_text(data = df2 %>% filter(mg_ml>0.12) %>% 
            filter(!(product_name=="Espresso - Iced Caffè Americano" & size=="tall")),
            aes(label=round(mg_ml,2), color=I(ifelse(size=="grande","black","white"))), hjust=1.2,
            family=f2, size=2.8) +
  geom_text(data= df2 %>% filter(product_name=="Espresso - Iced Caffè Americano" & size=="tall"),
            aes(label=paste(round(mg_ml,2),"mg/ml")), hjust=1.1,
            family=f2, size=2.8, color="white") +
  coord_flip(expand=F, clip="off") +
  scale_y_continuous(sec.axis = dup_axis(name =''),
                     labels = scales::unit_format(unit = "mg")) +
  cowplot::theme_minimal_vgrid(11) +
  theme(text=element_text(family=f2),
        plot.margin=margin(.7,1.5,.5,.7, unit="cm"),
        axis.ticks.y = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.title.x.bottom = element_text(size=10, face="italic"),
        plot.title.position = "plot",
        plot.title = element_text(size=14, face="bold", hjust=.5),
        plot.subtitle = element_markdown(hjust=.5,margin=margin(b=2), size=11,lineheight = 1.6),
        plot.background = element_rect(fill="#fafafa", color=NA),
        plot.caption=element_text(color="grey30")
  ) +
  labs(y="Caffine (mg)", x=NULL,
       caption="\n#TidyTuesday Week 52 | Data from PythonCoderUnicorn and @StarTrek_Lt",
       title="Caffeine in Starbucks Cold Drinks",
       subtitle="<span style = 'font-size:10pt;color:grey20'>Caffeine (mg) and caffine per ml (mg/ml) by product and serving size.</span><br><span style = 'color:#8F6404;'>**Tall (354 ml)**</span> | <span style = 'color:#d29074;'>**Grande (473 ml)**</span> | <span style = 'color:#0B5232;'>**Venti (709 ml)**</span>")
       
p1
