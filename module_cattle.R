# File: module_cattle.R



cattleUI <- function(id) {
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
          choices = c(
            "Total Cattle" = "Total Cattle",
            "Total Female Dairy Cattle" = "Total Female Dairy Cattle",
            "Total Female Beef Cattle" = "Total Female Beef Cattle",
            "Total Male Cattle" = "Total Male Cattle",
            "Total Calves" = "Total Calves"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        selectizeInput(
          ns("timeseries_variables"),
          "Click within the box to select variables",
          choices = unique(number_of_cattle$`Cattle by category`),
          selected = c(
            "Total Female Dairy Cattle",
            "Total Female Beef Cattle",
            "Total Male Cattle",
            "Total Calves"
          ),
          multiple = TRUE,
          options = list(
            plugins = list('remove_button')
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
        tabPanel("Time Series", lineChartUI(ns("line"), note_type = 2)),
        tabPanel("Area Chart", areaChartUI(ns("area"), note_type = 2)),
        tabPanel("Data Table", DTOutput(ns("table")))
      )
    )
  )
}

cattleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cattle_data <- livestock_subregion %>%
      filter(`Livestock by category` %in% c(
        "Total Female Dairy Cattle",
        "Total Female Beef Cattle",
        "Total Male Cattle",
        "Total Calves",
        "Total Cattle"
      )) %>%
      select(-Scotland) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = safe_as_numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        cattle_data %>% filter(`Livestock by category` == input$variable)
      }),
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      variable = reactive(input$variable),
      title = "Cattle Distribution by Region"
    )
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- number_of_cattle %>%
        filter(`Cattle by category` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Cattle by category`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Cattle Area Chart Data",
      yAxisTitle = "Number of Cattle (1,000)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Cattle Area Chart Data",
      yAxisTitle = "Number of Cattle (1,000)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        req(input$variable)
        cattle_data %>%
          filter(`Livestock by category` == input$variable) %>%
          datatable()
      } else {
        number_of_cattle %>%
          pivot_longer(cols = -`Cattle by category`, names_to = "year", values_to = "value") %>%
          datatable()
      }
    })
  })
}

cattle_demo <- function() {
  ui <- fluidPage(cattleUI("cattle_test"))
  server <- function(input, output, session) {
    cattleServer("cattle_test")
  }
  shinyApp(ui, server)
}

cattle_demo()
