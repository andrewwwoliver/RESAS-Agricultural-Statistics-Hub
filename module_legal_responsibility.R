# File: module_legal_responsibility.R

legalResponsibilityUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons(ns("data_type"), "Data Type", choices = c("Holdings" = "holdings", "Area" = "area"), selected = "holdings"),
        checkboxGroupInput(ns("selected_variables"), "Select Variables", choices = NULL, selected = NULL)
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

legalResponsibilityServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    chart_data <- reactive({
      data <- legal_responsibility %>%
        filter(`Legal responsibility` != "Total")
      data
    })
    
    initial_order <- reactive({
      data <- chart_data() %>%
        arrange(desc(Holdings))
      data$`Legal responsibility`
    })
    
    observe({
      choices <- unique(chart_data()$`Legal responsibility`)
      selected <- choices
      updateCheckboxGroupInput(session, "selected_variables", choices = choices, selected = selected)
    })
    
    y_col <- reactive({
      if (input$data_type == "holdings") {
        "Holdings"
      } else {
        "Area of holdings"
      }
    })
    
    yAxisTitle <- reactive({
      if (input$data_type == "holdings") {
        "Number of Holdings (1,000)"
      } else {
        "Area of Holdings (1,000 hectares)"
      }
    })
    
    unit <- reactive({
      if (input$data_type == "holdings") {
        "holdings"
      } else {
        "hectares"
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
      chart_data = reactive({
        data <- chart_data() %>%
          filter(`Legal responsibility` %in% input$selected_variables) %>%
          mutate(`Legal responsibility` = factor(`Legal responsibility`, levels = initial_order())) %>%
          arrange(`Legal responsibility`)
        data
      }),
      title = "Legal Responsibility of Holdings in Scotland",
      yAxisTitle = yAxisTitle,
      xAxisTitle = "Legal Responsibility",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "Legal responsibility",
      y_col = y_col,
      tooltip_format = tooltip_format,
      maintain_order = TRUE
    )
    
    output$data_table <- renderDT({
      datatable(chart_data(), options = list(
        scrollX = TRUE
      ))
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Legal_Responsibility_Data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(chart_data(), file, row.names = FALSE)
      }
    )
  })
}
