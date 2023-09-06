#' @title read Picarro CRDS output .dat file
#' @description reads Picarro cavity ring down spectroscopy 'CRDS' .dat file
#' @param path file path to a single CRDS .dat file
#' @return a tibble of cleaned Picarro data
#' @export
#'


# Use this for reading a single CRDS file:
iso_read_crds <- function(path) {
  read.delim(path , sep = "", header = T) |>
    dplyr::mutate(
      DATE = lubridate::ymd(DATE), # convert column to date type
      TIME = lubridate::hms(TIME), # convert column to time type
      datetime = DATE + TIME # create a single datetime column
    ) |>
    dplyr::select(datetime, everything()) # put it at the front for readability

}


