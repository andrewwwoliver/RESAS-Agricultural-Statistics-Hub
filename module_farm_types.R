# File: module_farm_types.R

farmTypesUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons(ns("data_type"), "Data Type", choices = c("Holdings" = "holdings", "Area" = "area", "Total from Standard Outputs" = "total", "Average standard outputs per holding" = "average"), selected = "holdings"),
        uiOutput(ns("variable_select")),  # Added variable select UI
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

farmTypesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create reactive data excluding 'All'
    chart_data <- reactive({
      data <- farm_type %>%
        filter(`Main farm type` != "All")
      if (!is.null(input$variables)) {
        data <- data %>%
          filter(`Main farm type` %in% input$variables)
      }
      data
    })
    
    # Get initial order based on 'Holdings'
    initial_order <- reactive({
      data <- chart_data() %>%
        arrange(desc(Holdings))
      data$`Main farm type`
    })
    
    output$variable_select <- renderUI({
      choices <- unique(farm_type$`Main farm type`)
      selected <- setdiff(choices, "All")
      selectizeInput(
        ns("variables"), 
        "Click within the box to select variables", 
        choices = choices, 
        selected = selected,
        multiple = TRUE,
        options = list(
          plugins = list('remove_button')
        )
      )
    })
    
    y_col <- reactive({
      switch(input$data_type,
             "holdings" = "Holdings",
             "area" = "Hectares",
             "total" = "Total from Standard Outputs",
             "average" = "Average standard outputs per holding")
    })
    
    yAxisTitle <- reactive({
      switch(input$data_type,
             "holdings" = "Number of Holdings",
             "area" = "Area (hectares)",
             "total" = "Total from Standard Outputs",
             "average" = "Average Standard Outputs per Holding")
    })
    
    tooltip_format <- reactive({
      switch(input$data_type,
             "holdings" = "Holdings: {point.y:.0f}",
             "area" = "Area (hectares): {point.y:.2f}",
             "total" = "Total from Standard Outputs: {point.y:.0f}",
             "average" = "Average Standard Outputs per Holding: {point.y:.2f}")
    })
    
    barChartServer(
      id = "bar_chart",
      chart_data = reactive({
        data <- chart_data()
        data <- data %>%
          mutate(`Main farm type` = factor(`Main farm type`, levels = initial_order())) %>%
          arrange(`Main farm type`)
        data
      }),
      title = "Farm Types in Scotland",
      yAxisTitle = yAxisTitle,
      xAxisTitle = "Main Farm Type",
      unit = "holdings",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "Main farm type",
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
      chart_type = "Farm Types",
      chart_data = chart_data,
      input = input,
      output = output,
      year_input = NULL
    )
  })
}
