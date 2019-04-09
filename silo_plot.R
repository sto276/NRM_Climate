# Contains default plot structures for transformed SILO.met files

library(tidyverse)

# Plot the average daily temperature extremes across each year
plot_year_temp_means = function(
  years,
  ymin = 0,
  ymax = 40
)
{
  ggplot(years, aes(year, mean_mint)) +
  geom_point(aes(colour = "1")) +
  geom_smooth(colour = "blue") +
  geom_point(aes(y = mean_maxt, colour = "2")) +
  geom_smooth(aes(y = mean_maxt), colour = "red") +
  scale_colour_manual(
    name = "",
    values = c("1"="skyblue", "2"="orange"),
    labels = c("1"="Mean minimum", "2"="Mean maximum")) +
  coord_cartesian(ylim = c(ymin, ymax)) +
  labs(
    title = "Average temperatures per year",
    subtitle = str_interp("Station: ${id}\n1961-2018")) +
  xlab("Year") +
  ylab("Temperature")
}

# Plot the temperature extremes for each year
plot_year_temp_extremes = function(
  years,
  ymin = -5,
  ymax = 55
)
{
  ggplot(years, aes(year, abs_mint)) +
    geom_point(aes(colour = "1")) +
    geom_point(aes(y = abs_maxt, colour = "2")) +
    geom_smooth(colour = "blue") +
    geom_smooth(aes(y = abs_maxt), colour = "red") +
    scale_colour_manual(
      name = "",
      values = c("1"="skyblue", "2"="orange"),
      labels = c("1"="Lowest minimum", "2"="Highest maximum")) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    labs(
      title = "Temperature extremes",
      subtitle = str_interp("Station: ${id}\n1961-2018")) +
    xlab("Year") +
    ylab("Temperature")
}

# Plot the sum of THI days in each year
plot_year_thi_days = function(
  years
)
{
  ggplot(years, aes(year, sum_80_thi_days)) +
    geom_point(aes(colour = "skyblue")) +
    geom_smooth(colour = "blue") +
    geom_point(aes(y = sum_85_thi_days, colour = "orange")) +
    geom_smooth(aes(y = sum_85_thi_days), colour = "red") +
    scale_colour_manual(
      name = "THI",
      values = c("orange"="orange", "skyblue"="skyblue"),
      labels = c("skyblue"=">80", "orange"=">85")) +
    coord_cartesian(ylim = c(0, 365)) +
    labs(
      title = "Sum of THI days in a year",
      subtitle = "1961-2018") +
    xlab("Year") +
    ylab("Days")
}

# Plot the average yearly THI days in each period
plot_period_thi_days = function(
  periods,
  id,
  slice = c(1:4)
)
{
  periods[slice,] %>%
    ggplot(aes(period, num_80_thi_days, fill = "1")) +
    geom_bar(stat = "identity", position = "dodge2") +
    geom_bar(stat = "identity", position = "dodge2", aes(y = num_85_thi_days, fill = "2")) +
    scale_fill_manual(
      name = "",
      values = c("1"="darkolivegreen3", "2"="darkolivegreen4"),
      labels = c("1"="THI > 80", "2"="THI > 85")) +
    coord_cartesian(ylim = c(0, 365)) +
    labs(
      title = "THI days per year",
      subtitle = str_interp("Station: ${id}\n1961-2018")) +
    xlab("Period") +
    ylab("Days")
}
