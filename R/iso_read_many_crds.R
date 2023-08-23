#' @title read multiple Picarro CRDS output .dat file
#' @description reads all Picarro cavity ring down spectroscopy 'CRDS' .dat files from a single directory.
#' @param directory
#' @return a tibble of cleaned Picarro data
#'


# For reading multiple files from a single directory:
iso_read_many_crds <- function(directory) {
  file_names <- list.files(path = directory, full.names = TRUE)

  combined_tibble <- map(file_names, iso_read_crds) |>
    list_rbind()

  return(combined_tibble)
}
