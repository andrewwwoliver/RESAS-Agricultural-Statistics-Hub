# File: module_other_animals.R

otherAnimalsUI <- function(id) {
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
            "Goats and kids" = "Goats and kids",
            "Deer" = "Deer",
            "Horses" = "Horses",
            "Donkeys" = "Donkeys",
            "Camelids" = "Camelids",
            "Beehives" = "Beehives"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = c(
            "Goats",
            "Deer",
            "Horses",
            "Donkeys",
            "Camelids",
            "Beehives"
          ),
          selected = c(
            "Goats",
            "Deer",
            "Horses",
            "Donkeys",
            "Camelids",
            "Beehives"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Data Table'",
        ns = ns,
        radioButtons(
          ns("table_data"),
          "Select Data to Display",
          choices = c("Map Data" = "map", "Chart Data" = "timeseries"),
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
        tabPanel("Data Table", DTOutput(ns("table")))
      )
    )
  )
}

otherAnimalsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    other_animals_data <- livestock_subregion %>%
      filter(`Livestock by category` %in% c(
        "Goats and kids",
        "Deer",
        "Horses",
        "Donkeys",
        "Camelids",
        "Beehives"
      )) %>%
      select(-Scotland) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = safe_as_numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        other_animals_data %>% filter(`Livestock by category` == input$variable)
      }),
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      variable = reactive(input$variable),
      title = "Other Animals Distribution by Region"
    )
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- number_of_other_livestock %>%
        mutate(across(-`Livestock by category`, as.numeric)) %>%
        filter(`Livestock by category` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Livestock by category`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
  
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Other Animals Area Chart Data",
      yAxisTitle = "Number of Animals (1,000)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        req(input$variable)
        other_animals_data %>%
          filter(`Livestock by category` == input$variable) %>%
          datatable()
      } else {
        number_of_other_livestock %>%
          pivot_longer(cols = -`Cattle by category`, names_to = "year", values_to = "value") %>%
          datatable()
      }
    })
  })
}

# Testing module
other_animals_demo <- function() {
  ui <- fluidPage(otherAnimalsUI("other_animals_test"))
  server <- function(input, output, session) {
    otherAnimalsServer("other_animals_test")
  }
  shinyApp(ui, server)
}

other_animals_demo()
