# Contains functions for accessing SILO data

library(tidyverse)
library(stringr)
library(RCurl)

# Download a .met file from apsim by station ID and save it to the destination
# Returns the file loaded into R as a spec_tbl_df object
get_met = function(
  ID,
  destination,
  name = "",
  start_day = 1,
  start_month = 1,
  start_year = 1959,
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

# Download a SILO PPD file by station ID and save it to the destination
# Returns the file loaded into R as a spec_tbl_df object
get_ppd = function(
  ID,
  destination,
  username,
  password,
  name = "",
  start_day = "01",
  start_month = "01",
  start_year = "1959",
  end_day = "31",
  end_month = "12",
  end_year = "2018"
)
{
  # Website (defaulting to apsim format)
  site <- "https://legacy.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?"

  # The file format
  form <- "format=Standard"

  # The station ID
  station <- str_interp("&station=${ID}")

  # The start date
  start <- str_interp("&start=${start_year}${start_month}${start_day}")

  # The finish date
  finish <- str_interp("&finish=${end_year}${end_month}${end_day}")

  # The user credentials
  creds <- str_interp("&username=${username}&password=${password}")

  # Combine the parameters into a valid request URL
  URL <- str_c(site, form, station, start, finish, creds)

  # wget options
  options <- "--ca-certificate=cacert.pem -O"

  # Download the file if it doesn't exist already
  path <- str_interp("${destination}/${ID}${name}.txt")
  if(file.exists(path) == FALSE){
    download.file(
      URL,
      destfile = path,
      method = "wget",
      extra = options,
      cacheOK = FALSE,
      quiet = TRUE
    )
  }

  # Scan the column names
  names <- scan(path, what = character(), nlines = 1, skip = 35)

  # Define the column types
  {
    types <- cols(
      Date = col_double(),
      Day = col_double(),
      T.Max = col_double(),
      Smx = col_double(),
      T.Min = col_double(),
      Smn = col_double(),
      Rain = col_double(),
      Srn = col_double(),
      Evap = col_double(),
      Sev = col_double(),
      Radn = col_double(),
      Ssl = col_double(),
      VP = col_double(),
      Svp = col_double(),
      RHmaxT = col_double(),
      RHminT = col_double(),
      Date2 = col_date("%d/%m/%Y")
    )
  }

  # Read and return the file
  {
    ppd <- read_delim(
      path,
      " ",
      comment = '"',
      skip = 37,
      trim_ws = TRUE,
      skip_empty_rows = TRUE,
      col_types = types,
      col_names = names
    ) %>%
      mutate(THI = 0.8 * T.Max + ((RHmaxT / 100) * (T.Max - 14.4)) + 46.4)
  }
}

