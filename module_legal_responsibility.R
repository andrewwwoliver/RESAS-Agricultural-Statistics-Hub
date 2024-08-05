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

# File: module_legal_responsibility.R

legalResponsibilityServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create reactive data excluding 'Total'
    chart_data <- reactive({
      data <- legal_responsibility %>%
        filter(`Legal responsibility` != "Total")
      data
    })
    
    # Get initial order based on 'Holdings'
    initial_order <- reactive({
      data <- chart_data() %>%
        arrange(desc(Holdings))
      data$`Legal responsibility`
    })
    
    # Update checkboxGroupInput choices
    observe({
      choices <- unique(chart_data()$`Legal responsibility`)
      selected <- choices  # Select all choices, including 'Unknown'
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
    
    tooltip_format <- reactive({
      if (input$data_type == "holdings") {
        "Holdings: {point.y:.0f}"
      } else {
        "Area (hectares): {point.y:.2f}"
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
      unit = "hectares",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "Legal responsibility",
      y_col = y_col,
      tooltip_format = tooltip_format,
      maintain_order = TRUE
    )
    
    render_data_table(
      table_id = "data_table",
      chart_data = chart_data,
      output = output
    )
    
    handle_data_download(
      download_id = ns("downloadData"),
      chart_type = "Legal Responsibility",
      chart_data = chart_data,
      input = input,
      output = output,
      year_input = NULL
    )
  })
}
