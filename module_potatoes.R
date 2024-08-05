# File: module_potatoes.R

potatoesUI <- function(id) {
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
          choices = unique(potatoes_subregion$`Land use by category`)
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = unique(potatoes_data$`Crop/Land use`),
          selected = c(
            "Ware potatoes",
            "Seed potatoes"
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
        ),
        tags$div(
          style = "width: 100%;",
          downloadButton(ns("download_data"), "Download Data")
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
# File: module_potatoes.R

source("utils.R")

potatoesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    potatoes_map <- potatoes_subregion %>%
      select(-Scotland) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Land use by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = safe_as_numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        potatoes_map %>% filter(`Land use by category` == input$variable)
      }),
      unit = "hectares",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      variable = reactive(input$variable),
      title = "Potatoes Distribution by Region (hectares)"
    )
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- potatoes_data %>%
        filter(`Crop/Land use` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Crop/Land use`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Potatoes Area Planted",
      yAxisTitle = "Area of Potatoes (1,000 hectares)",
      xAxisTitle = "Year",
      unit = "hectares",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Potatoes Area Planted",
      yAxisTitle = "Area of Potatoes (1,000 hectares)",
      xAxisTitle = "Year",
      unit = "hectares",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        req(input$variable)
        datatable(potatoes_subregion)
      } else {
        datatable(potatoes_data)
      }
    })
    
    output$download_data <- createDownloadHandler(
      input = input,
      file_map_name = "Potato_Subregion_Data.xlsx",
      file_timeseries_name = "Potato_Timeseries_Data.xlsx",
      map_data = potatoes_subregion,
      timeseries_data = potatoes_data
    )
  })
}

potatoes_demo <- function() {
  ui <- fluidPage(potatoesUI("potatoes_test"))
  server <- function(input, output, session) {
    potatoesServer("potatoes_test")
  }
  shinyApp(ui, server)
}

potatoes_demo()

