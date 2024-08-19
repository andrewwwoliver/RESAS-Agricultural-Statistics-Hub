landUseSummaryUI <- function(id) {
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
          choices = unique(land_use_subregion$`Land use by category`)
        )
      ), 
      conditionalPanel(
        condition = "input.tabsetPanel === 'Summary'",
        ns = ns,
        div(
          style = "font-size: 24px; font-weight: bold;",
          " "
        )
      ), 
      conditionalPanel(
        condition = "input.tabsetPanel === 'Bar Chart'",
        ns = ns,
        checkboxGroupInput(
          ns("variables"), 
          "Choose variables to add to chart", 
          choices = c("Total crops, fallow, and set-aside", "Total grass", "Rough grazing", 
                      "Total sole right agricultural area", "Common grazings"),
          selected = c("Total crops, fallow, and set-aside", "Total grass", "Rough grazing", 
                       "Total sole right agricultural area", "Common grazings")
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = unique(land_use_data$`Crop/Land use`),
          selected = c(
            "Common grazings",
            "Rough grazing",
            "Total crops, fallow, and set-aside",
            "Total grass",
            "Cauliflower",
            "Total sole right agricultural area"
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
      id = ns("mainpanel"),
      width = 9,
      tabsetPanel(
        id = ns("tabsetPanel"),
        tabPanel("Map", mapUI(ns("map"))),
        tabPanel("Bar Chart", barChartUI(ns("bar_chart"))),
        tabPanel("Time Series", lineChartUI(ns("line"))),
        tabPanel("Data Table", 
                 DTOutput(ns("table")),
                 tags$div(
                   style = "margin-top: 20px;",
                   downloadButton(ns("download_data"), "Download Data")
                 )
        )
      )
    )
  )
}



landUseSummaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    land_use_map <- land_use_subregion %>%
      select(-Scotland) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Land use by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        land_use_map %>% filter(`Land use by category` == input$variable)
      }),
      unit = "hectares",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      variable = reactive(input$variable),
      title = "Land Use by Region (hectares)"
    )
    
    chart_data <- reactive({
      agricultural_area_hectares %>%
        filter(`Crop/Land use` %in% input$variables) %>%
        select(`Crop/Land use`, `2023 Area`) %>%
        rename(Variable = `Crop/Land use`, Value = `2023 Area`)
    })
    
    timeseries_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- land_use_data %>%
        filter(`Crop/Land use` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Crop/Land use`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    barChartServer(
      id = "bar_chart",
      chart_data = chart_data,
      title = "Agricultural Area in 2023 by Land Use Type",
      yAxisTitle = "Area (1,000 hectares)",
      xAxisTitle = "Land Use Type",
      unit = "hectares",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "Variable",
      y_col = "Value",
      tooltip_format = "Area (hectares): {point.y:.2f}"
    )
    
    lineChartServer(
      id = "line",
      chart_data = timeseries_data,
      title = "Land Use Chart Data",
      yAxisTitle = "Area of Land Use (1,000 hectares)",
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
        datatable(land_use_subregion, options = list(
          scrollX = TRUE))
      } else {
        datatable(land_use_data, options = list(
          scrollX = TRUE))
      }
    })
    
    output$download_data <- createDownloadHandler(
      input = input,
      file_map_name = "Land Use Subregion Data 2023.xlsx",
      file_timeseries_name = "Land Use Timeseries Data 2012 to 2023.xlsx",
      map_data = land_use_subregion,
      timeseries_data = land_use_data
    )
  })
}

land_use_demo <- function() {
  ui <- fluidPage(landUseSummaryUI("land_use_test"))
  server <- function(input, output, session) {
    landUseSummaryServer("land_use_test")
  }
  shinyApp(ui, server)
}

land_use_demo()
