library(tidyverse)

# Compress a range of years into a single observation period
compress = function(
  years,
  start,
  finish
)
{
  output <- filter(years, between(year, start, finish)) %>%
    summarise(
      period = str_interp("${start}-${finish}"),
      avg_maxt = mean(mean_maxt),
      avg_mint = mean(mean_mint),
      avg_temp = mean(mean_temp),
      avg_pan = mean(mean_pan),
      tot_rain = sum(sum_rain),
      var_rain = sd(sum_rain, na.rm = TRUE),
      num_00_mm_days = sum(sum_00_mm_days),
      num_25_mm_days = sum(sum_25_mm_days),
      num_50_mm_days = sum(sum_50_mm_days),
      num_80_thi_days = sum(sum_80_thi_days),
      num_85_thi_days = sum(sum_85_thi_days)
    )
}

# Transform the .met data into summarised data over multiple years
transform_met = function(
  id,
  folder
)
{
  source('get_met.r')

  # Summarise the met data by year
  years <- get_met(id, folder) %>%
          mutate(thi = 0.8 * maxt + ((maxt / 100) * (maxt - 14.4)) + 46.4) %>%
          group_by(year) %>%
          summarise(
              mean_maxt = mean(maxt),
              mean_mint = mean(mint),
              mean_temp = mean((mint + maxt) / 2),
              mean_pan = mean(pan),
              sum_rain = sum(rain),
              sum_00_mm_days = sum(rain == 0),
              sum_25_mm_days = sum(rain > 25),
              sum_50_mm_days = sum(rain > 50),
              sum_80_thi_days = sum(thi > 80),
              sum_85_thi_days = sum(thi > 85)
            )

  # Split data into periods
  periods <- compress(years, 1961, 1990) %>%
          full_join(compress(years, 1991, 2000)) %>%
          full_join(compress(years, 2001, 2010)) %>%
          full_join(compress(years, 2011, 2018)) %>%
          full_join(compress(years, 1991, 2018))
}