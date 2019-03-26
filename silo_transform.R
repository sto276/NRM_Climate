# Contains functions for transforming SILO files into annual data

library(tidyverse)
source("silo_get.R")


# Transform .met file from daily data to annual data
transform_met = function(
  id,
  folder
)
{
  # Summarise the met data by year
  years <- get_met(id, folder) %>%
          mutate(thi = 0.8 * maxt + ((maxt / 100) * (maxt - 14.4)) + 46.4) %>%
          group_by(year) %>%
          summarise(
              mean_maxt = mean(maxt),
              abs_maxt = max(maxt),
              mean_mint = mean(mint),
              abs_mint = min(mint),
              mean_temp = mean((mint + maxt) / 2),
              mean_pan = mean(pan),
              sum_rain = sum(rain),
              sum_00_mm_days = sum(rain == 0),
              sum_25_mm_days = sum(rain > 25),
              sum_50_mm_days = sum(rain > 50),
              sum_80_thi_days = sum(thi > 80),
              sum_85_thi_days = sum(thi > 85)
            )
}

# Transform ppd (.txt) file from daily data to annual data
transform_ppd = function(
  id,
  path
)
{
  # Summarise the met data by year
  years <- get_ppd(id, path) %>%
    mutate(THI = 0.8 * T.Max + ((RHmaxT / 100) * (T.Max - 14.4)) + 46.4) %>%
    mutate(year = as.double(format(Date2, "%Y"))) %>%
    group_by(year) %>%
    summarise(
      mean_maxt = mean(T.Max),
      mean_mint = mean(T.Min),
      mean_temp = mean((T.Min + T.Max) / 2),
      mean_pan = mean(Evap),
      sum_rain = sum(Rain),
      sum_00_mm_days = sum(Rain == 0),
      sum_25_mm_days = sum(Rain > 25),
      sum_50_mm_days = sum(Rain > 50),
      sum_80_thi_days = sum(THI > 80),
      sum_85_thi_days = sum(THI > 85)
    )
}
