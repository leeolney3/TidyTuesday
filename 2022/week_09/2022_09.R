# 2022-02-22
# TidyTuesday Week 9 Alternative Fuel Stations
# Data from US DOT, by way of Data is Plural

library(sf)
library(tmap)

# shp file from https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-01/Alternative_Fueling_Stations.zip
shp = read_sf("data/Alternative_Fueling_Stations/Alternative_Fueling_Stations.shp")
shp2 = st_transform(shp, 4326)

box = c(xmin=-124.736342 , ymin=24.521208, xmax=-66.945392, ymax=49.382808) #bbox for lower 48 states
shp3 = st_crop(shp2, st_bbox(box))

p1 = tm_shape(shp3) +
  tm_dots(col="FUEL_TYPE_", legend.show=FALSE, 
          palette=c("#855C75","#AF6458","#736F4C","#526A83","#625377","#68855C","#A06177")) +
  tm_facets(by="FUEL_TYPE_", ncol=2, nrow=4) +
  tm_layout(frame = FALSE, frame.lwd = NA, panel.label.bg.color = NA,
            panel.label.fontface = "bold",
            main.title="Alternative Fuel Stations in Lower 48 States",
            main.title.size = 1.3,
            main.title.position = "center",
            attr.outside=T) +
  tm_credits("Note: Includes public access, private access and planned stations.\nTidyTuesday week 9 | Data from US DOT by way of Data is Plural", size=1)
  
tmap_save(p1, "2022_09.png",height=8, width=8, units="in")
  
