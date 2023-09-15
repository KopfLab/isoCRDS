#' @title read multiple Picarro CRDS output .dat file
#' @description reads all Picarro cavity ring down spectroscopy 'CRDS' .dat files from a single directory.
#' @param directory a folder directory containing multiple .dat files
#' @return a tibble of cleaned Picarro data
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
iso_read_many_crds <- function(directory) {
  file_names <- list.files(path = directory, full.names = TRUE)

  combined_tibble <- map(file_names, iso_read_crds) |>
    list_rbind()

  return(combined_tibble)
}
