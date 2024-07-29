# File: module_stockfeeding.R

stockfeedingUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        condition = "input.tabsetPanel === 'Map'",
        ns = ns,
        radioButtons(
          ns("variable"), 
          "Select Variable", 
          choices = unique(stockfeeding_subregion$`Land use by category`),
          
        )
      ),     
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = unique(stockfeeding_data$`Crop/Land use`),
          selected = c(
            "Turnips/swedes",
            "Kale/cabbage",
            "Maize",
            "Rape",
            "Fodder beet",
            "Lupins"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Data Table'",
        ns = ns,
        radioButtons(
          ns("table_data"),
          "Select Data to Display",
          choices = c("Map Data" = "map", "Time Series Data" = "timeseries"),
          selected = "map"
        )
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabsetPanel"),
        tabPanel("Map", mapUI(ns("map"))),
        tabPanel("Time Series", lineChartUI(ns("line"))),
        tabPanel("Area Chart", areaChartUI(ns("area"))),
        tabPanel("Data Table", DTOutput(ns("table")))
      )
    )
  )
}

stockfeedingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    stockfeeding_map <- stockfeeding_subregion %>%
      select(-Scotland) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Land use by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = safe_as_numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        stockfeeding_map %>% filter(`Land use by category` == input$variable)
      }),
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      variable = reactive(input$variable),
      title = "Stockfeeding Area Distribution by Region (hectares)"
    )
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- stockfeeding_data %>%
        filter(`Crop/Land use` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Crop/Land use`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Stockfeeding Area Chart Data",
      yAxisTitle = "Area of Stockfeeding Crops (1,000 hectares)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Stockfeeding Area Chart Data",
      yAxisTitle = "Area of Stockfeeding Crops (1,000 hectares)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        req(input$variable)
        stockfeeding_subregion %>%
          datatable()
      } else {
        stockfeeding_data  %>%
          datatable()
      }
    })
  })
}


stockfeeding_demo <- function() {
  ui <- fluidPage(stockfeedingUI("stockfeeding_test"))
  server <- function(input, output, session) {
    stockfeedingServer("stockfeeding_test")
  }
  shinyApp(ui, server)
}

stockfeeding_demo()
