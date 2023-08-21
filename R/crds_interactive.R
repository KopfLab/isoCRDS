library(shiny)
library(plotly)
library(dplyr)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "materia"),
  fileInput("uploadFile", "Upload RDS file:"),
  selectInput("xColumn", "Select X Axis:", ""),
  selectInput("yColumn", "Select Y Axis:", ""),
  plotlyOutput("scatterPlot"),
  textInput("sampleName", "Enter Sample Name:", ""),
  actionButton("addSampleId", "Add Sample ID"),
  textInput("outputFileName", "Enter Output File Name (without extension):", "modified_data"),
  actionButton("saveButton", "Save Mapped Data as RDS")
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$uploadFile, {
    uploaded_data <- readRDS(input$uploadFile$datapath)  # Read .RDS file
    if (is.data.frame(uploaded_data)) {
      data(uploaded_data)
      updateSelectInput(session, "xColumn", choices = colnames(uploaded_data))
      updateSelectInput(session, "yColumn", choices = colnames(uploaded_data))
    } else {
      showModal(modalDialog("Uploaded data is not in a valid format."))
    }
  })
  
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
        showModal(modalDialog("Please select points on the scatter plot."))
      }
    }
  })
  
  output$uploadedDataTable <- renderDT({
    if (!is.null(data())) {
      datatable(head(data()))
    }
  })
  
  output$scatterPlot <- renderPlotly({
    if (!is.null(data())) {
      x_column <- input$xColumn
      y_column <- input$yColumn
      p <- plot_ly(
        data(), 
        x = ~.data[[x_column]],
        y = ~.data[[y_column]], 
        type = "scatter", 
        mode = "markers"
        ) |>
        layout(
          xaxis = list(title = x_column),
          yaxis = list(title = y_column),
          dragmode = "select"
        )
      p
    }
  })
  
  observeEvent(input$saveButton, {
    if (!is.null(data())) {
      datetime_col <- "datetime"
      sample_id_col <- "sample_id"
      other_cols <- setdiff(names(data()), c(datetime_col, sample_id_col))
      col_order <- c(datetime_col, sample_id_col, other_cols)
      
      file_name <- paste0(input$outputFileName, ".rds")
      data_modified <- data() |>
        select(col_order)
      
      saveRDS(data_modified, file_name)
      showModal(modalDialog(paste("Modified data saved as '", file_name, "'.", sep = "")))
    }
  })
}

shinyApp(ui, server)
