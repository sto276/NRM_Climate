library(tidyverse)

# Clear the environment
rm(list=ls())

source('get_met.r')
source('transform_met.r')

# Station ID
id <- 1001

# Destination folder
folder <- "SILO_met"

periods <- transform_met(id, folder)

write_csv(periods, str_interp("SILO_csv/${id}.csv"))

graph <- ggplot(data = periods) +
          geom_point(mapping = aes(x = period, y = avg_maxt))

rains <- ggplot(data = periods) +
          geom_point(mapping = aes(x = period, y = num_25_mm_days), colour = "red") +
          geom_point(mapping = aes(x = period, y = num_50_mm_days), colour = "blue")

range <- ggplot(data = periods) +
          geom_point(mapping = aes(x = period, y = avg_maxt), colour = "red") +
          geom_point(mapping = aes(x = period, y = avg_mint), colour = "blue")

thids <- ggplot(data = periods) +
          geom_point(mapping = aes(x = period, y = num_80_thi_days), colour = "red") +
          geom_point(mapping = aes(x = period, y = num_85_thi_days), colour = "blue")
