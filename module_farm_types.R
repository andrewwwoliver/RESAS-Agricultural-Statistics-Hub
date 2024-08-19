# File: module_farm_types.R

farmTypesUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons(ns("data_type"), "Data Type", choices = c("Holdings" = "holdings", "Area" = "area", "Total from Standard Outputs" = "total", "Average standard outputs per holding" = "average"), selected = "holdings"),
        conditionalPanel(
          condition = paste0("input['", ns("tabs"), "'] == '", ns("bar"), "'"),  # Show variable select only on Bar Chart tab
          uiOutput(ns("variable_select"))  # Variable select UI
        )
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
    
    # Create reactive data excluding 'All' and depending only on data_type
    filtered_data <- reactive({
      farm_type %>%
        filter(`Main farm type` != "All")
    })
    
    # Get filtered data based on selected variables (only for bar chart)
    chart_data <- reactive({
      data <- filtered_data()
      if (input$tabs == ns("bar") && !is.null(input$variables)) {
        data <- data %>%
          filter(`Main farm type` %in% input$variables)
      }
      data
    })
    
    # Select the appropriate column based on data_type
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
    
    # Render the data table based only on data_type selection
    output$data_table <- renderDT({
      datatable(
        filtered_data() %>%
          select(`Main farm type`, y_col()),
        colnames = c("Main Farm Type", yAxisTitle())
      )
    })
    
    # Create a download handler for the data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Farm_Types_", input$data_type, ".xlsx", sep = "")
      },
      content = function(file) {
        write.xlsx(filtered_data() %>% 
                     select(`Main farm type`, y_col()), file, rowNames = FALSE)
      }
    )
    
    # Render the variable selection UI dynamically
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
    
    # Render the bar chart using the filtered data
    barChartServer(
      id = "bar_chart",
      chart_data = chart_data,
      title = "Farm Types in Scotland",
      yAxisTitle = yAxisTitle,
      xAxisTitle = "Main Farm Type",
      unit = input$data_type,
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "Main farm type",
      y_col = y_col,
      tooltip_format = tooltip_format,
      maintain_order = FALSE
    )
  })
}
