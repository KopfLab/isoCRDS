#' @title Map a list of sample_ids to CRDS data
#' @description Map a list of sample IDs to CRDS data. Import a sample map with a list of sample_ids and two columns that correspond to the start and end datetimes of the sample
#' @param crds_data cavity ring-down spectroscopy with a datetime column. Ideally this has been read using 'iso_read_crds'
#' @param sample_map a three-column tibble containing sample_id, datetime_start, and datetime_end, corresponding to sample intervals
#' @returns a tibble of CRDS data with a sample_id column mapped to specific time intervals
#'

require(dplyr)
require(fuzzyjoin)

iso_map_crds <- function(crds_data, sample_map) {
  mapped_data <- crds_data |>
    fuzzyjoin::fuzzy_left_join(
      sample_map, # join with the sample map
      by = c("datetime" = "datetime_start", "datetime" = "datetime_end"),
      # match function is datetime >= datetime_start *and* <= datetime_end
      match_fun = list(`>=`, `<=`)
    ) |>
    # add the is_in_interval boolean for easy filtering
    mutate(
      is_in_interval = !is.na(sample_id)
    )
  return(mapped_data)
}


