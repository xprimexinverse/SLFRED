# Header ------------------------------------------------------------------
#
# Title: R code for downloading time-series from the FRED database
#
# Author: Graeme Walsh <graeme.walsh@hotmail.co.uk>
# Date: 19/08/2018


# Package Info ------------------------------------------------------------

#' A package for downloading time-series from the FRED database.
#'
#' The workhorse function of the package is \code{getFRED()}. This function returns a
#' list containing the requested time-series and the corresponding metadata.
#' The code behind this package also serves as a backend to an EViews add-in created by the author.
#'
#'
#' @section SLFRED functions:
#' \code{getFRED()}
#'
#' @docType package
#' @name SLFRED
NULL


# Important variables -----------------------------------------------------

# New environment
SLFRED.env <- new.env()

# FRED API URLs
SLFRED.env$obs_base_url    <- "https://api.stlouisfed.org/fred/series/observations?series_id="
SLFRED.env$series_base_url <- "https://api.stlouisfed.org/fred/series?series_id="

# API key (which can be used by multiple users of the software)
SLFRED.env$api_key <- "&api_key=498dd227ac96d062170aef43ccef93f0"

# FRED Data URL
SLFRED.env$base_url <- "https://fred.stlouisfed.org/data/"

# Read data functions -----------------------------------------------------

#' @title Get a time-series from the FRED
#' @description Downloads a time-series from FRED into R
#' @param series_id The series id for the time-series to be downloaded
#' @export
#' @author Graeme Walsh
#' @details The data is downloaded from the FRED API
getSLFRED <- function(series_id){

  # Get the data
  obs <- getObs(series_id)

  # Get the metadata
  metadata <- getSeriesMeta(series_id)

  # Package together the information into a list
  fred_data <- list(data=obs, metadata=metadata)

  return(fred_data)
}

#' @title Get time-series values (observations) from the FRED
#' @description Downloads time-series values (observations) from FRED into R
#' @param series_id The series id for the time-series to be downloaded
#' @importFrom XML xmlParse xpathApply xmlGetAttr
#' @export
#' @author Graeme Walsh
#' @details The data is downloaded from the FRED API
getObs <- function(series_id){

  # Create a .xml temp file
  temp <- tempfile(fileext = ".xml")

  # Output file options for curl command
  opts <- paste0(" -o ", temp)

  # Create the curl command
  command <- paste0("curl ", paste0(SLFRED.env$obs_base_url, series_id, SLFRED.env$api_key), opts)

  # Download the file
  system(command, intern = TRUE)

  # Parse the file
  xml_file <- xmlParse(temp)

  # Extract the values
  values <- unlist(xpathApply(xml_file, "//observation[@value]", xmlGetAttr, "value"))
  values[which(values==".")] <- NA
  values <- as.numeric(values)

  # Extract the dates
  dates <- unlist(xpathApply(xml_file, "//observation[@date]", xmlGetAttr, "date"))

  # Create dataframe containing the observations (and dates)
  obs <- data.frame(dates=dates, values=values, stringsAsFactors = FALSE)

  return(obs)
}

#' @title Get time-series metadata from the FRED
#' @description Downloads time-series metadata from FRED into R
#' @param series_id The series id for the time-series of interest
#' @importFrom XML xmlParse xpathApply xmlGetAttr
#' @export
#' @author Graeme Walsh
#' @details The data is downloaded from the FRED API
getSeriesMeta <- function(series_id){

  # Create a .xml temp file
  temp <- tempfile(fileext = ".xml")

  # Output file options for curl command
  opts <- paste0(" -o ", temp)

  # Create the curl command
  command <- paste0("curl ", paste0(SLFRED.env$series_base_url, series_id, SLFRED.env$api_key), opts)

  # Download the file
  system(command, intern = TRUE)

  # Parse the file
  xml_file <- xmlParse(temp)

  # Extract the series title
  series_title <- unlist(xpathApply(xml_file, "//series[@title]", xmlGetAttr, "title"))

  # Extract the series start and end
  series_start <- unlist(xpathApply(xml_file, "//series[@observation_start]", xmlGetAttr, "observation_start"))
  series_end <- unlist(xpathApply(xml_file, "//series[@observation_end]", xmlGetAttr, "observation_end"))

  # Extract the series frequency
  series_freq <- unlist(xpathApply(xml_file, "//series[@frequency]", xmlGetAttr, "frequency"))
  series_freq_short <- unlist(xpathApply(xml_file, "//series[@frequency_short]", xmlGetAttr, "frequency_short"))

  # Extract the series units
  series_units <- unlist(xpathApply(xml_file, "//series[@units]", xmlGetAttr, "units"))
  series_units_short <- unlist(xpathApply(xml_file, "//series[@units_short]", xmlGetAttr, "units_short"))

  # Extract the series seasonal adjustment
  series_sa <- unlist(xpathApply(xml_file, "//series[@seasonal_adjustment]", xmlGetAttr, "seasonal_adjustment"))
  series_sa_short <- unlist(xpathApply(xml_file, "//series[@seasonal_adjustment_short]", xmlGetAttr, "seasonal_adjustment_short"))

  # Extract the series last update
  series_last <- unlist(xpathApply(xml_file, "//series[@last_updated]", xmlGetAttr, "last_updated"))

  # Extract the series notes
  series_notes <- unlist(xpathApply(xml_file, "//series[@notes]", xmlGetAttr, "notes"))

  # Package the metadata together into a list
  metadata <- list(title=series_title, start=series_start, end=series_end, frequency=series_freq, freq=series_freq_short, seas_adj=series_sa, sa=series_sa_short, units=series_units, u=series_units_short, last_update=series_last, notes=series_notes)

  return(metadata)
}

#' @title Get a time-series from the FRED
#' @description Downloads a time-series from FRED into R
#' @param series_id The series id for the time-series to be downloaded
#' @importFrom utils download.file read.table
#' @export
#' @author Graeme Walsh
#' @details The data is downloaded from the FRED data folder (not using the API).
getSLFRED2 <- function(series_id){

  # Create a .txt temp file
  temp <- tempfile(fileext = ".txt")

  # Download the file
  download.file(paste0(SLFRED.env$base_url, series_id, ".txt"), temp)

  # Read the file and get the number of header lines
  data <- readLines(temp)
  numHeaderLines <- grep("DATE", data) - 1

  # Read the metadata
  metadata <- readLines(temp, n = numHeaderLines)

  # Read the numerical data
  numdata <- read.table(temp, skip = numHeaderLines, header = TRUE, stringsAsFactors = FALSE, na.strings = ".")
  numdata[,"DATE"] <- as.Date(numdata[,"DATE"])
  numdata[,"VALUE"] <- as.numeric(numdata[,"VALUE"])

  # Package the data into a list
  fred_data <- list(data = numdata, metadata = metadata)

  return(fred_data)
}
