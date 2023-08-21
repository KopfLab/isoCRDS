#' @title read Picarro CRDS output .dat file
#' @description reads Picarro cavity ring down spectroscopy 'CRDS' .dat file
#' @param filename
#' @return a tibble of cleaned Picarro data
#' 

require(tidyverse)
require(lubridate)

# Use this for reading a single CRDS file:
iso_read_crds <- function(path) {
  crds_df <- read.delim(path , sep = "", header = T) |> 
    mutate(
      DATE = ymd(DATE), # convert column to date type
      TIME = hms(TIME), # convert column to time type
      datetime = DATE + TIME # create a single datetime column
    ) |> 
    select(datetime, everything()) # put it at the front for readability
  return(crds_df)
}

# For reading multiple files from a single directory:
iso_read_many_crds <- function(directory) {
  file_names <- list.files(path = directory, full.names = TRUE)
  
  combined_tibble <- map(file_names, iso_read_crds) |> 
    list_rbind()
  
  return(combined_tibble)
}

