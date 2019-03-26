# Clear the environment
rm(list=ls())

source('silo_get.r')
source('silo_transform.r')
source('silo_summarise.r')
source('silo_plot.r')

run = function(
  id
)
{
  # Destination folder
  folder <- "SILO_raw"

  years <- transform_ppd(id, folder)
  periods <- get_periods(years)

  write_csv(periods, str_interp("SILO_summarised/${id}.csv"))

  plt <- plot_period_thi_days(periods)

  ggsave(filename = str_interp("${id}.png"),
         plot = plt,
         device = "png",
         path = "Plots")
}

# Station ID's
ids <- c("002012",
         "003003",
         "005026",
         "014904",
         "015590",
         "030018",
         "030161",
         "034084",
         "036031",
         "037003")

for(id in ids){
  run(id)
}