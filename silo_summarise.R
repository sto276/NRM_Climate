# Summarises transformed SILO data (by range of years)

# Compresses
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

# Join a set of year-range summaries into a single tibble based on year ranges (e.g. 1961 to 1990) that can be plotted
get_periods = function(
  years,
  ranges = matrix(c(1958, 1988, 1998, 2008, 1988, 1987, 1997, 2007, 2018, 2018), nrow = 5, ncol = 2)
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