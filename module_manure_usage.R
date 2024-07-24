# File: module_manure_usage.R

manureUsageUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons(ns("data_type"), "Data Type", choices = c("Holdings" = "holdings", "Area" = "area"), selected = "holdings")  # Changed order here
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
    
    chart_data <- reactive({
      data <- manure_fertiliser
      if (!is.null(input$variables)) {
        data <- data %>%
          filter(`Fertiliser by type` %in% input$variables)
      }
      data
    })
    
    output$variable_select <- renderUI({
      choices <- unique(manure_fertiliser$`Fertiliser by type`)
      selected <- choices
      checkboxGroupInput(ns("variables"), "Choose variables to add to chart", choices = choices, selected = selected)
    })
    
    observeEvent(input$select_all_button, {
      updateCheckboxGroupInput(session, ns("variables"), selected = unique(manure_fertiliser$`Fertiliser by type`))
    })
    
    observeEvent(input$deselect_all_button, {
      updateCheckboxGroupInput(session, ns("variables"), selected = character(0))
    })
    
    y_col <- reactive({
      if (input$data_type == "holdings") {
        "2023 holdings"
      } else {
        "2023 area"
      }
    })
    
    yAxisTitle <- reactive({
      if (input$data_type == "holdings") {
        "Holdings"
      } else {
        "Area (1,000 hectares)"
      }
    })
    
    tooltip_format <- reactive({
      if (input$data_type == "holdings") {
        "Holdings: {point.y:.0f}"
      } else {
        "Area (hectares): {point.y:.2f}"
      }
    })
    
    barChartServer(
      id = "bar_chart",
      chart_data = chart_data,
      title = "Manure Usage by Type in Scotland",
      yAxisTitle = yAxisTitle,
      xAxisTitle = "Fertiliser Type",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "Fertiliser by type",
      y_col = y_col,
      tooltip_format = tooltip_format
    )
    
    render_data_table(
      table_id = "data_table",
      chart_data = chart_data,
      output = output
    )
    
    handle_data_download(
      download_id = ns("downloadData"),
      chart_name = "Manure Usage",
      chart_data = chart_data,
      input = input,
      output = output,
      year_input = NULL
    )
  })
}
