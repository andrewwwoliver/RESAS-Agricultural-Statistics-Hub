# File: module_poultry.R

poultryUI <- function(id) {
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
            "Total poultry" = "Total poultry",
            "Fowls for producing eggs" = "Fowls for producing eggs",
            "Fowls for breeding" = "Fowls for breeding",
            "Broilers for other table fowls and other poultry" = "Broilers for other table fowls and other poultry"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        selectizeInput(
          ns("timeseries_variables"),
          "Click within the box to select variables",
          choices = unique(number_of_poultry$`Poultry by category`),
          selected = c(
            "Total fowls for producing eggs",
            "Total fowls for breeding",
            "Broilers and other table birds",
            "Total Poultry"
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

poultryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    poultry_data <- livestock_subregion %>%
      filter(`Livestock by category` %in% c(
        "Fowls for producing eggs",
        "Fowls for breeding",
        "Broilers for other table fowls and other poultry",
        "Total poultry"
      )) %>%
      select(-Scotland) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = safe_as_numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        poultry_data %>% filter(`Livestock by category` == input$variable)
      }),
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      variable = reactive(input$variable),
      title = "Poultry Distribution by Region"
    )
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- number_of_poultry %>%
        filter(`Poultry by category` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Poultry by category`, names_to = "year", values_to = "value") %>%
        mutate(value = safe_as_numeric(value))
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Poultry Area Chart Data",
      yAxisTitle = "Number of Poultry (1,000)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;">* Estimates for 2023 are not comparable to previous years due to methodological improvements.<br/><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Poultry Area Chart Data",
      yAxisTitle = "Number of Poultry (1,000)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;">* Estimates for 2023 are not comparable to previous years due to methodological improvements.<br/><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        req(input$variable)
        poultry_data %>%
          filter(`Livestock by category` == input$variable) %>%
          datatable()
      } else {
        number_of_poultry %>%
          pivot_longer(cols = -`Poultry by category`, names_to = "year", values_to = "value") %>%
          datatable()
      }
    })
  })
}

# Testing module
poultry_demo <- function() {
  ui <- fluidPage(poultryUI("poultry_test"))
  server <- function(input, output, session) {
    poultryServer("poultry_test")
  }
  shinyApp(ui, server)
}

poultry_demo()