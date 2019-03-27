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
      tot_rain = mean(sum_rain),
      var_rain = sd(sum_rain, na.rm = TRUE),
      num_00_mm_days = mean(sum_00_mm_days),
      num_25_mm_days = mean(sum_25_mm_days),
      num_50_mm_days = mean(sum_50_mm_days),
      num_80_thi_days = mean(sum_80_thi_days),
      num_85_thi_days = mean(sum_85_thi_days),
      num_35_tmax_days = mean(sum_35_tmax_days),
      num_40_tmax_days = mean(sum_40_tmax_days)
    )
}

# Join a set of year-range summaries into a single tibble based on year ranges (e.g. 1961 to 1990) that can be plotted
get_periods = function(
  years,
  ranges = matrix(c(1959, 1989, 1999, 2009, 1989, 1988, 1998, 2008, 2018, 2018), nrow = 5, ncol = 2)
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
    "num_85_thi_days",
    "num_35_tmax_days",
    "num_40_tmax_days"
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

# Merge a collection of periods into a single dataset
merge = function(
  ids
)
{
  merged <- NULL;

  # read_csv data types
  {
    types <- cols(
      period = col_character(),
      avg_maxt = col_double(),
      avg_mint = col_double(),
      avg_temp = col_double(),
      avg_pan = col_double(),
      tot_rain = col_double(),
      var_rain = col_double(),
      num_00_mm_days = col_double(),
      num_25_mm_days = col_double(),
      num_50_mm_days = col_double(),
      num_80_thi_days = col_double(),
      num_85_thi_days = col_double(),
      num_35_tmax_days = col_double(),
      num_40_tmax_days = col_double()
    )
  }

  # Join by names
  {
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
      "num_85_thi_days",
      "num_35_tmax_days",
      "num_40_tmax_days",
      "station"
    )
  }

  for(id in ids)  {
    file <- read_csv(str_c("SILO_summarised/", id, ".csv"), col_types = types) %>%
      mutate(station = id)

    if (is.null(merged)) {merged <- file}
    else {merged <- full_join(merged, file, vars)}
  }

  out <- group_by(merged, period) %>%
    summarise(
      avg_maxt = mean(avg_maxt),
      avg_mint = mean(avg_mint),
      avg_temp = mean(avg_temp),
      avg_pan = mean(avg_pan),
      tot_rain = mean(tot_rain),
      var_rain = mean(var_rain),
      num_00_mm_days = mean(num_00_mm_days),
      num_25_mm_days = mean(num_25_mm_days),
      num_50_mm_days = mean(num_50_mm_days),
      num_80_thi_days = mean(num_80_thi_days),
      num_85_thi_days = mean(num_85_thi_days),
      num_35_tmax_days = mean(num_35_tmax_days),
      num_40_tmax_days = mean(num_40_tmax_days),
      stations = sum(station > 0)
    )

  write_csv(out, "Average.csv")
}