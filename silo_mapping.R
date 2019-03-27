library(tidyverse)
library(maptools)
library(readr)
library(rgdal)

nrm <- readOGR(dsn = "Maps/NRM_Regions_2017", layer = "NRM_Regions_2017", stringsAsFactors = FALSE)

regions <- nrm@data[["NRM_REGION"]]
region <- subset(nrm, NRM_REGION == "Condamine")

stations <- read_csv("Maps/AllStations.csv")

map <- ggplot() +
  geom_polygon(data = region,
               aes(x = long,
                   y = lat,
                   group = group),
               colour = "black",
               fill = NA)

  geom_point(data = stations,
             aes(x = Lon,
                 y = Lat),
             colour = "skyblue",
             size = 0.2)
  #coord_cartesian(xlim = c(114, 152),
  #                ylim =c(-43, -12))
  #labs(title = "Australia by weather station location")
