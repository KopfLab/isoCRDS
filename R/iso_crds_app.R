#' @title Map sample information onto CRDS data
#' @description Initializes a Shiny app that allows the user to interactively map sample information onto CRDS data.
#' @examples
#' # this assumes you have already cached your CRDS data with iso_read_crds or iso_read_many_crds
#' df <- iso_read_many_crds("test_data/")
#' saveRDS(df, "unmapped_crds_data.RDS")
#'
#' # now that you've saved your CRDS data, you can open it in the app
#' # run the app:
#' iso_crds_app()
#'
#' # after mapping, you have saved a new RDS file of mapped data
#' mapped_df <- readRDS("mapped_crds_data.RDS")
#' # voila! ^_^


require(shiny)
require(plotly)
require(dplyr)
require(bslib)

# this function(...) definition helps the shiny app run smoothly as a packaged function
iso_crds_app <- function(...) {

  # Define the App UI
  crds_ui <- fluidPage(
    theme = bs_theme(version = 4, bootswatch = "yeti"),        # theme options
    titlePanel("isoCRDS Interactive Data Mapper"),             # title
    # Define our layout style: sidebar layout
    sidebarLayout(
      # The sidebar:
      sidebarPanel(
        fileInput("uploadFile", "Upload RDS file:"),               # a button to upload CRDS data in RDS format
        selectInput("xColumn", "Select X Axis:", ""),              # allow user to choose X axis
        selectInput("yColumn", "Select Y Axis:", ""),              # allow user to choose Y axis
        textInput("sampleName", "Enter Sample Name:", ""),         # here the user defines their sample ID
        actionButton("addSampleId", "Add Sample ID"),              # and must press this to map it to their data
        textInput("outputFileName", "Enter Output File Name (without extension):", "modified_data"), # user defines output file name
        actionButton("saveButton", "Save Mapped Data as RDS")      # a button to save their mapped data as RDS
      ),
      # The Main Panel:
      mainPanel(
        plotlyOutput("scatterPlot"),                               # render a plotly of the CRDS data

      )
    )
  )

  crds_server <- function(input, output, session) {
    options(shiny.maxRequestSize=100*1024^2) # set the maximum upload size to 100 MB

    data <- reactiveVal(NULL)       # create an empty reactive

    # Wait for an event: a user uploads a .RDS file of CRDS data.
    observeEvent(input$uploadFile, {
      uploaded_data <- readRDS(input$uploadFile$datapath)  # Read the user-uploaded .RDS file
      if (is.data.frame(uploaded_data)) {
        data(uploaded_data)
        updateSelectInput(session, "xColumn", choices = colnames(uploaded_data))
        updateSelectInput(session, "yColumn", choices = colnames(uploaded_data))
      } else {
        showModal(modalDialog("Uploaded data is not in a valid CRDS data format."))
      }
    })

    # Wait for an event: the user adds a sample ID
    observeEvent(input$addSampleId, {
      if (!is.null(data()) && nchar(input$sampleName) > 0) {
        selected_points <- event_data("plotly_selected")

        if (!is.null(selected_points)) {
          sample_name <- input$sampleName
          selected_indices <- selected_points$pointNumber
          data_modified <- data()
          data_modified[selected_indices, "sample_id"] <- sample_name
          data(data_modified)
        } else {
          showModal(modalDialog("To map a sample, please select points on the scatter plot."))
        }
      }
    })

    # Render a plotly scatterplot of the CRDS data that lets the user
    # click and drag to define their sample regions
    output$scatterPlot <- renderPlotly({
      if (!is.null(data())) {
        x_column <- input$xColumn # these are the user-defined x and y columns
        y_column <- input$yColumn # these are the user-defined x and y columns
        # define a plotly:
        p <- plot_ly(
          data(),
          x = ~.data[[x_column]],
          y = ~.data[[y_column]],
          type = "scatter",
          mode = "lines"
          ) |>
          layout(
            xaxis = list(title = x_column),
            yaxis = list(title = y_column),
            dragmode = "select" # this is key: user can click and drag to select data points!
          )
        p
      }
    })

    # Interactive save
    observeEvent(input$saveButton, {
      if (!is.null(data())) {
        datetime_col <- "datetime" # define the datetime column name
        sample_id_col <- "sample_id" # define the sample_id column name
        # define the items not shared between the two sets
        other_cols <- setdiff(names(data()), c(datetime_col, sample_id_col))
        # set the column order
        col_order <- c(datetime_col, sample_id_col, other_cols)

        file_name <- paste0(input$outputFileName, ".rds") # define the filename based on user-definition
        data_modified <- data() |>
          select(col_order)

        saveRDS(data_modified, file_name) # save the RDS file
        showModal(modalDialog(paste("Modified data saved as '", file_name, "'.", sep = "")))
      }
    })
  }

  # Initialize the Shiny App
  shinyApp(ui = crds_ui, server = crds_server)

}
