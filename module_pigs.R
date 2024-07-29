# File: module_pigs.R

pigsUI <- function(id) {
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
            "Total pigs" = "Total pigs",
            "Female pigs breeding herd" = "Female pigs breeding herd",
            "All other non-breeding pigs" = "All other non-breeding pigs"
          ))
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        selectizeInput(
          ns("timeseries_variables"),
          "Click within the box to select variables",
          choices = unique(number_of_pigs$`Pigs by category`),
          selected = c(
            "Total breeding herd",
            "80 kg liveweight and over",
            "50 kg and under 80 kg liveweight",
            "Under 50 kg liveweight"
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
        tabPanel("Time Series", lineChartUI(ns("line"))),
        tabPanel("Area Chart", areaChartUI(ns("area"))),
        tabPanel("Data Table", DTOutput(ns("table")))
      )
    )
  )
}

pigsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pigs_data <- livestock_subregion %>%
      filter(`Livestock by category` %in% c(
        "Female pigs breeding herd",
        "All other non-breeding pigs",
        "Total pigs"
      )) %>%
      select(-Scotland) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))  # Ensure value is numeric
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        pigs_data %>% filter(`Livestock by category` == input$variable)
      }),
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      variable = reactive(input$variable),
      title = "Pigs Distribution by Region"
    )
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- number_of_pigs %>%
        mutate(across(-`Pigs by category`, as.numeric)) %>%
        filter(`Pigs by category` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Pigs by category`, names_to = "year", values_to = "value") %>%
        mutate(value = as.numeric(value))  # Ensure value is numeric
      filtered_data
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Pigs Area Chart Data",
      yAxisTitle = "Number of Pigs (1,000)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Pigs Area Chart Data",
      yAxisTitle = "Number of Pigs (1,000)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        req(input$variable)
        pigs_data %>%
          filter(`Livestock by category` == input$variable) %>%
          datatable()
      } else {
        number_of_pigs %>%
          pivot_longer(cols = -`Pigs by category`, names_to = "year", values_to = "value") %>%
          datatable()
      }
    })
  })
}

# Testing module
pigs_demo <- function() {
  ui <- fluidPage(pigsUI("pigs_test"))
  server <- function(input, output, session) {
    pigsServer("pigs_test")
  }
  shinyApp(ui, server)
}

pigs_demo()