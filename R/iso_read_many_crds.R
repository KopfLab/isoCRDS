#' @title Read multiple Picarro CRDS output .dat file
#' @description Reads all Picarro cavity ring down spectroscopy 'CRDS' .dat files from a single directory.
#' @param directory A folder directory containing multiple .dat files
#' @param timezone A character string indicating the timezone to read CRDS datetime format. Defaults to UTC if not specified.
#' @return A tibble of cleaned Picarro data
#' @examples
#' # read a folder's worth of CRDS data
#' iso_read_many_crds("folder_with_crds_data/")
#'
#' # write it to a variable in your R workspace
#' df <- iso_read_many_crds("folder_with_crds_data/")
#'
#' # then cache it as an RDS file for downstream mapping
#' df |> saveRDS(object = df, file = "my_crds_cache.RDS")
#'
#' @export
#'


# For reading multiple files from a single directory:
iso_read_many_crds <- function(directory, timezone = "UTC") {
  file_names <- list.files(path = directory, full.names = TRUE)

  combined_tibble <- map(file_names, iso_read_crds(timezone = timezone)) |>
    list_rbind()

  return(combined_tibble)
}
