#' @param crds_data a crds tibble
#' @param panels a character vector of the columns to be plotted

require(ggstackplot)

iso_plot_crds <- function(crds_data) {
  crds_data |> 
    ggstackplot(
      x = datetime,
      y = c("X12CO2", "X13CO2", "Delta_Raw_iCO2", "HP_12CH4", "HR_13CH4", "Delta_iCH4_Raw"),
      alternate_axes = FALSE,
      color = c("#201e75", "#4c4ac2", "#2e4759", "#a83e00", "#911600", "#592e48"),
      template = 
        ggplot() +
        scale_x_datetime() +
        geom_line() +
        theme_stackplot(),
      add = list(
        `X12CO2` = scale_y_continuous("12CO2 (ppm)"),
        `X13CO2` = scale_y_continuous("13CO2 (ppm)"),
        `Delta_Raw_iCO2` = scale_y_continuous("d13C_CO2"),
        `HP_12CH4` = scale_y_continuous("12CH4"),
        `HR_13CH4` = scale_y_continuous("13CH4"),
        `Delta_iCH4_Raw` = scale_y_continuous("d13C_CH4")
        )
      )
}

