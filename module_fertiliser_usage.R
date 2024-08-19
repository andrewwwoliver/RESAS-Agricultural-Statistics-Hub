# File: module_fertiliser_usage.R

fertiliserUsageUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons(ns("data_type"), "Data Type", choices = c("Holdings" = "holdings", "Area" = "area"), selected = "holdings")
      ),
      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Bar Chart", barChartUI(ns("bar_chart")), value = ns("bar")),
          tabPanel("Data Table",
                   DTOutput(ns("data_table")),
                   tags$div(
                     style = "margin-top: 20px;",
                     downloadButton(ns("downloadData"), "Download Data")
                   ),
                   value = ns("data"))
        )
      )
    )
  )
}

fertiliserUsageServer <- function(id) {
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
        "Holdings: {point.y:.0f} holdings"
      } else {
        "Area: {point.y:.2f} hectares"
      }
    })
    
    barChartServer(
      id = "bar_chart",
      chart_data = chart_data,
      title = "Fertiliser Usage by Type in Scotland",
      yAxisTitle = yAxisTitle,
      xAxisTitle = "Fertiliser Type",
      unit = "",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "Fertiliser by type",
      y_col = y_col,
      tooltip_format = tooltip_format
    )
    
    output$data_table <- renderDT({
      datatable(chart_data(), options = list(
        scrollX = TRUE
      ))
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Fertiliser_Usage_Data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(chart_data(), file, row.names = FALSE)
      }
    )
  })
}

