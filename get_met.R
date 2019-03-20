# Designed to extract relevant data from SILO.met files
library(tidyverse)
library(stringr)
library(RCurl)

# Download a .met apsim file by station ID and save it to the destination
# Returns the file loaded into R as a spec_tbl_df object
get_met = function(
  ID,
  destination,
  name = "",
  start_day = 1,
  start_month = 1,
  start_year = 1961,
  end_day = 31,
  end_month = 12,
  end_year = 2018
)
{
  # Website (defaulting to apsim format)
  site <- "http://apsrunet.apsim.info/cgi-bin/getData.met?format=apsim"

  # Combine the search arguments
  args <- str_interp("&station=${ID}&ddStart=${start_day}&mmStart=${start_month}&yyyyStart=${start_year}&ddFinish=${end_day}&mmFinish=${end_month}&yyyyFinish=${end_year}")

  # Attach search arguments to site
  URL <- str_c(site, args)

  # Download the file if it doesn't exist already
  path <- str_interp("${destination}/${ID}${name}.met")
  if(file.exists(path) == FALSE){
    download.file(
      URL,
      destfile = path,
      method = "wget",
      extra = "-O -q",
      cacheOK = FALSE,
      quiet = TRUE
    )
  }

  # Define the column names
  {
  names <- c(
    "year",
    "day",
    "radn",
    "maxt",
    "mint",
    "rain",
    "pan",
    "vp",
    "code"
  )
  }

  # Define the column types
  {
  types <- cols(
    year = col_integer(),
    day = col_integer(),
    radn = col_double(),
    maxt = col_double(),
    mint = col_double(),
    rain = col_double(),
    pan = col_double(),
    vp = col_double(),
    code = col_integer()
  )
  }

  # Read and return the file
  {
  met <- read_delim(
    path,
    " ",
    comment = "!",
    skip = 20,
    trim_ws = TRUE,
    skip_empty_rows = TRUE,
    col_names = names,
    col_types = types
  )
  }
}


