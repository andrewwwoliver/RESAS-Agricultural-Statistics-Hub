# File: module_manure_usage.R

manureUsageUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        uiOutput(ns("variable_select")),
        actionButton(ns("select_all_button"), "Select All"),
        actionButton(ns("deselect_all_button"), "Deselect All"),
        radioButtons(ns("data_type"), "Data Type", choices = c("Area" = "area", "Holdings" = "holdings"), selected = "area")
      ),
      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Bar Chart", barChartUI(ns("bar_chart")), value = ns("bar")),
          tabPanel("Data Table",
                   DTOutput(ns("data_table")),
                   downloadButton(ns("downloadData"), "Download Data"),
                   value = ns("data"))
        )
      )
    )
  )
}

manureUsageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Ensure chart_data is filtered based on the selected variables and data type
    chart_data <- reactive({
      data <- manure_fertiliser
      if (!is.null(input$variables)) {
        data <- data %>%
          filter(`Fertiliser by type` %in% input$variables)
      }
      data
    })
    
    # Render variable selection UI
    output$variable_select <- renderUI({
      choices <- unique(manure_fertiliser$`Fertiliser by type`)
      selected <- choices
      checkboxGroupInput(ns("variables"), "Choose variables to add to chart", choices = choices, selected = selected)
    })
    
    # Select all variables
    observeEvent(input$select_all_button, {
      updateCheckboxGroupInput(session, ns("variables"), selected = unique(manure_fertiliser$`Fertiliser by type`))
    })
    
    # Deselect all variables
    observeEvent(input$deselect_all_button, {
      updateCheckboxGroupInput(session, ns("variables"), selected = character(0))
    })
    
    # Define the reactive y_col based on the selected data type
    y_col <- reactive({
      if (input$data_type == "area") {
        "2023 area"
      } else {
        "2023 holdings"
      }
    })
    
    # Pass the filtered chart_data and y_col to the barChartServer
    barChartServer(
      id = "bar_chart",
      chart_data = chart_data,
      title = "Manure Usage by Type in Scotland",
      yAxisTitle = "Amount",
      xAxisTitle = "Fertiliser Type",
      footer = '<div style="font-size: 16px; font-weight: bold;">Source: Scottish manure fertiliser usage 2023.</div>',
      x_col = "Fertiliser by type",
      y_col = y_col
    )
    
    # Render the data table with the filtered chart_data
    render_data_table(
      table_id = "data_table",
      chart_data = chart_data,
      output = output
    )
    
    # Handle data download
    handle_data_download(
      download_id = "downloadData",
      chart_type = "Manure Usage",
      chart_data = chart_data,
      input = input,
      output = output
    )
  })
}
