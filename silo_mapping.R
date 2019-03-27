library(maptools)
library(readr)
library(rgdal)

nrm <- readOGR(dsn = "Maps/NRM_Regions_2017", layer = "NRM_Regions_2017", stringsAsFactors = FALSE)

stations <- read_csv("Maps/AllStations.csv")

map <- ggplot() +
  geom_polygon(data = nrm,
               aes(x = long,
                   y = lat,
                   group = group),
               colour = "black",
               fill = NA)

stats <- ggplot() +
  geom_point(data = stations,
             aes(x = Lon,
                 y = Lat),
             colour = "skyblue",
             size = 0.2)
