library(tidyverse)
library(afrilearndata)
library(sf)
library(raster)
library(rayshader)

mgc = africountries %>% filter(name=="South Africa")

mgp = mask(afripop2020, mgc)

df_mgp = as.data.frame(mgp, xy=TRUE) %>% 
  drop_na() %>% 
  rename(pop = ppp_2020_1km_Aggregated) %>%
  mutate(year=2020)
range(df_mgp$pop)

p1 = ggplot() + 
  #geom_sf(data=mgc, color="white", fill="transparent") +
  geom_tile(data = df_mgp, aes(x=x, y=y, fill=pop)) +
  scale_fill_viridis_c(name="Log2 scale\n",labels=scales::comma, trans="log2") +
  coord_fixed() +
  #rcartocolor::scale_fill_carto_c(palette="SunsetDark", trans="log2") +
  theme_void(base_size = 10) +
  theme(plot.background = element_rect(fill="white", color=NA),
        plot.margin=margin(0,.5,.5,.5, unit="cm"),
        legend.title=element_text(lineheight = 1.1, size=8.5),
        plot.title.position="plot", 
        plot.caption.position="plot",         
        plot.title=element_text(hjust=.5, face="bold", size = 10),
        plot.caption=element_text(size=6.5, hjust=0.95),
        plot.subtitle=element_text(hjust=.5,size=8.5)) +
  guides(fill=guide_colorbar(barwidth = unit(.5, "lines"),
                             barheight = unit(8, "lines"))) +
  labs(title="South Africa", subtitle="Population density in 2020, aggregated to 20km squares\n", caption="#TidyTuesday Week 46 | Data source: afrimapr team and {afrilearndata} package")
p1

plot_gg(p1,theta=-10, phi=80, width=5, height=5, zoom=.6)

render_snapshot(clear=TRUE)