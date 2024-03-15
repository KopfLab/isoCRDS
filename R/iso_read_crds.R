#' @title read Picarro CRDS output .dat file
#' @description reads Picarro cavity ring down spectroscopy 'CRDS' .dat file
#' @param path file path to a single CRDS .dat file
#' @return a tibble of cleaned Picarro data
#' @examples
#' # read a data file
#' iso_read_crds("a_crds_data_file.dat")
#' # pass the data to a variable in your R workspace
#' df <- iso_read_crds("a_crds_data_file.dat")
#' df
#' @export
#'


# Use this for reading a single CRDS file:
iso_read_crds <- function(path) {
  read.delim(path , sep = "", header = TRUE) |>
    dplyr::mutate(
      DATE = lubridate::ymd(DATE), # convert column to date type
      TIME = lubridate::hms(TIME), # convert column to time type
      datetime = DATE + TIME, # create a single datetime column
      filename = path # note the file it was imported from
    ) |>
    dplyr::select(datetime, everything()) # put it at the front for readability

}


