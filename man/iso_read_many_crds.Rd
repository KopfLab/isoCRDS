% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iso_read_many_crds.R
\name{iso_read_many_crds}
\alias{iso_read_many_crds}
\title{read multiple Picarro CRDS output .dat file}
\usage{
iso_read_many_crds(directory)
}
\arguments{
\item{directory}{a folder directory containing multiple .dat files}
}
\value{
a tibble of cleaned Picarro data
}
\description{
reads all Picarro cavity ring down spectroscopy 'CRDS' .dat files from a single directory.
}
\examples{
# read a folder's worth of CRDS data
iso_read_many_crds("folder_with_crds_data/")

# write it to a variable in your R workspace
df <- iso_read_many_crds("folder_with_crds_data/")

# then cache it as an RDS file for downstream mapping
df |> saveRDS(object = df, file = "my_crds_cache.RDS")

}
