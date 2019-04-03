library(tidyverse)
library(maptools)
library(readr)
library(rgdal)

# Read in NRM spatial data
nrm <- readOGR(dsn = "Maps/NRM_Regions_2017", layer = "NRM_Regions_2017", stringsAsFactors = FALSE)

# Read in station data
stations <- read_csv("Maps/AllStations.csv") %>%
  mutate(Region = "") %>%
  mutate(State = "") %>%
  mutate(Country = "Australia")

# Transform station data to spatial data
coordinates(stations) = ~Longitude + Latitude
proj4string(stations) <- proj4string(nrm)

# Find which station falls in which NRM_REGION
for(name in nrm$NRM_REGION){
  region <- subset(nrm, NRM_REGION == name)
  raw <- tibble(match = over(stations, as(region, "SpatialPolygons")))
  cond <- raw$match == 1
  stations$Region[cond] <- region$NRM_REGION
  stations$State[cond] <- region$STATE
}

# Setup Tableau Geocoding
{
  geo <- filter(data.frame(stations), Region != "")
  geo$optional = NULL
  #geo <- geo[c(3, 4, 5, 6, 7, 2, 1)]
  #geo <- geo[c(3, 5, 6, 7, 2, 1)]
  geo <- geo[c(3, 4, 5, 2, 1)]
  write_csv(geo, "Maps/Tableau/Station.csv")
}

# {
# regions <- c("Burnett Mary",
#              "Cape York",
#              "Condamine",
#              "Co-operative Management Area",
#              "Desert Channels",
#              "Fitzroy",
#              "Burdekin",
#              "Northern Gulf",
#              "Maranoa Balonne and Border Rivers",
#              "Mackay Whitsunday",
#              "South East Queensland",
#              "South West Queensland",
#              "Southern Gulf",
#              "Wet Tropics",
#              "Northern Territory",
#              "Rangelands Region"
#              )
#
# maps <- subset(nrm, NRM_REGION %in% regions)
# points <- data.frame(subset(stations, region %in% regions))
#
# map <- ggplot() +
#   geom_polygon(data = maps,
#                aes(x = long,
#                    y = lat,
#                    group = group),
#                colour = "black",
#                fill = "grey") +
#   geom_point(data = points,
#              aes(x = Lon,
#                  y = Lat,
#                  colour = region),
#              size = 0.02)
#   #coord_cartesian(xlim = c(114, 152),
#   #                ylim =c(-43, -12))
#   #labs(title = "Australia by weather station location")
# }

get_records = function(
  path = "StationRecords.csv"
)
{
  types = cols(
    X1 = col_double(),
    Lon = col_double(),
    Lat = col_double(),
    ID = col_double(),
    Station = col_character(),
    FirstRain = col_double(),
    LastRain = col_double(),
    FirstMaxT = col_double(),
    LastMaxT = col_double(),
    FirstMinT = col_double(),
    LastMinT = col_double(),
    LengthRainRecord = col_double(),
    LengthTmaxRecord = col_double(),
    LengthTminRecord = col_double()
  )

  records <- read_csv(path, col_types = types) %>%
    select(-one_of("X1")) %>%
    unique() %>%
    filter(Lat >= -20) %>%
    filter((FirstMaxT < 1958) | (LastMaxT > 2018)) %>%
    arrange(Station)
}

