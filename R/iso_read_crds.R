#' @title Read Picarro CRDS output .dat file
#' @description Reads Picarro cavity ring down spectroscopy 'CRDS' .dat file
#' @param path A character string indicating the file path to a single CRDS .dat file
#' @param timezone A character string indicating the timezone to read CRDS datetime format. Defaults to UTC if not specified.
#' @return A tibble of cleaned Picarro data
#' @examples
#' # read a data file
#' iso_read_crds("a_crds_data_file.dat")
#' # pass the data to a variable in your R workspace
#' df <- iso_read_crds("a_crds_data_file.dat")
#' df
#' @export
#'


# Use this for reading a single CRDS file:
iso_read_crds <- function(path, timezone = "UTC") {
  read.delim(path , sep = "", header = TRUE) |>
    dplyr::mutate(
      DATE = lubridate::ymd(DATE), # convert column to date type
      TIME = lubridate::hms(TIME), # convert column to time type
      # convert to datetime format, account for timezone difference
      datetime = dplyr::case_when(
        timezone == "UTC" ~ ymd_hms(DATE + TIME),
        timezone != "UTC" ~ ymd_hms(DATE + TIME, tz = timezone)
      ),
      filename = path # note the file it was imported from
    ) |>
    dplyr::select(datetime, everything()) # put it at the front for readability
}


