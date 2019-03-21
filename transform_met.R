library(tidyverse)

# Compress a range of annual data into a single summary observation
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
      num_00_mm_days = sum(sum_00_mm_days) / (finish - start),
      num_25_mm_days = sum(sum_25_mm_days) / (finish - start),
      num_50_mm_days = sum(sum_50_mm_days) / (finish - start),
      num_80_thi_days = sum(sum_80_thi_days) / (finish - start),
      num_85_thi_days = sum(sum_85_thi_days) / (finish - start)
    )
}

# Transform .met file from daily data to annual data
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

# Join a set of year-range summaries into a single tibble based on year ranges (e.g. 1961 to 1990)
get_periods = function(
  years,
  ranges = matrix(c(1961, 1991, 2001, 2011, 1991, 1990, 2000, 2010, 2018, 2018), nrow = 5, ncol = 2)
)
{
  periods <- compress(years, ranges[1,1], ranges[1,2])
  vars <- c(
    "period",
    "avg_maxt",
    "avg_mint",
    "avg_temp",
    "avg_pan",
    "tot_rain",
    "var_rain",
    "num_00_mm_days",
    "num_25_mm_days",
    "num_50_mm_days",
    "num_80_thi_days",
    "num_85_thi_days"
  )

  # Select the time periods based on the given ranges
  for (r in 2:nrow(ranges))
  {
    periods <- full_join(periods, compress(years, ranges[r, 1], ranges[r, 2]), vars)
  }

  # Enforce ordering based on order added
  periods$period <- factor(periods$period, levels = periods$period)

  # Return the periods
  periods
}
