full_data_total <- reactive({ national_total })


# UI for Total Emissions Module
totalEmissionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        conditionalPanel(
          condition = "input.tabs != 'Summary_Page'",
          ns = ns,
          uiOutput(ns("variable_select")),
          actionButton(ns("select_all_button"), "Select All"),
          actionButton(ns("deselect_all_button"), "Deselect All"),
          sliderInput(ns("year"), "Select year range", value = c(1990, 2022), min = 1990, max = 2022, step = 1, sep = "", ticks = TRUE)
        ),
        conditionalPanel(
          condition = "input.tabs == 'Summary_Page'",
          ns = ns,
          sliderInput(ns("summary_current_year_total"), "Current Year", min = 1990, max = 2022, value = 2022, step = 1, sep = ""),
          sliderInput(ns("summary_comparison_year_total"), "Comparison Year", min = 1990, max = 2022, value = 2021, step = 1, sep = "")
        )
      ),
      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Summary Page",
                   value = "Summary_Page",
                   fluidRow(
                     column(width = 12, div(class = "header-text", "Top 3 Emitting Industries:"))
                   ),
                   fluidRow(
                     column(width = 4, valueBoxUI(ns("totalIndustry1_total")), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 4, valueBoxUI(ns("totalIndustry2_total")), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 4, valueBoxUI(ns("totalIndustry3_total")), style = "padding-right: 0; padding-left: 0;")
                   ),
                   fluidRow(
                     column(width = 12, div(class = "header-text", "Summary Analysis:"))
                   ),
                   fluidRow(
                     column(width = 4, valueBoxUI(ns("totalValue_total")), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 4, chartUI(ns("industryLineChart_total"), "Industry Emissions Over Time"), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 4, chartUI(ns("industryBarChart_total"), "Emissions by Category"), style = "padding-right: 0; padding-left: 0;")
                   )
          ),
          tabPanel("Timelapse", timelapseBarChartUI(ns("bar")), value = "Timelapse"),
          tabPanel("Time Series", lineChartUI(ns("line")), value = "Line_Chart"),
          tabPanel("Area Chart", areaChartUI(ns("area")), value = "Area_Chart"),
          tabPanel("Data Table",
                   DTOutput(ns("data_table")),
                   downloadButton(ns("downloadData"), "Download Data"),
                   value = "Data_Table")
        )
      )
    )
  )
}

# Server for Total Emissions Module
totalEmissionsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive data for line chart
    line_chart_data <- reactive({
      data <- national_total
      data <- data %>% filter(Year >= min(input$year) & Year <= max(input$year))
      data <- data %>% filter(Industry %in% c("Agriculture", "Total"))
      data
    })
    
    # Reactive data for other charts
    chart_data <- reactive({
      data <- national_total
      data <- data %>% filter(Year >= min(input$year) & Year <= max(input$year))
      if (!is.null(input$variables)) {
        data <- data %>%
          filter(Industry %in% input$variables)
      }
      data
    })
    
    output$variable_select <- renderUI({
      choices <- unique(national_total$Industry)
      selected <- setdiff(choices, "Total")
      checkboxGroupInput(ns("variables"), "Choose variables to add to chart", choices = choices, selected = selected)
    })
    
    observeEvent(input$select_all_button, {
      updateCheckboxGroupInput(session, ns("variables"), selected = setdiff(unique(national_total$Industry), "Total"))
    })
    
    observeEvent(input$deselect_all_button, {
      updateCheckboxGroupInput(session, ns("variables"), selected = character(0))
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "National Greenhouse Gas Emissions by Source in Scotland",
      yAxisTitle = "Emissions (MtCO₂e)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px;"><a href="https://www.gov.scot/publications/scottish-greenhouse-gas-statistics-2022/documents/">Source: Scottish Greenhouse Gas Statistics 2022</a></div>',
      x_col = "Year",
      y_col = "Value"
    )
    
    timelapseBarChartServer(
      id = "bar",
      chart_data = chart_data,
      title = "National Greenhouse Gas Emissions Timelapse",
      yAxisTitle = "Emissions (MtCO₂e)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px;"><a href="https://www.gov.scot/publications/scottish-greenhouse-gas-statistics-2022/documents/">Source: Scottish Greenhouse Gas Statistics 2022</a></div>',
      x_col = "Year",
      y_col = "Value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "National Greenhouse Gas Emissions by Source in Scotland",
      yAxisTitle = "Emissions (MtCO₂e)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px;"><a href="https://www.gov.scot/publications/scottish-greenhouse-gas-statistics-2022/documents/">Source: Scottish Greenhouse Gas Statistics 2022</a></div>',
      x_col = "Year",
      y_col = "Value"
    )
    
    render_data_table(
      table_id = "data_table",
      chart_data = chart_data,
      output = output
    )
    
    handle_data_download(
      download_id = "downloadData",
      chart_type = "National",
      chart_data = chart_data,
      input = input,
      output = output,
      year_input = "year"
    )
    
    # Summary Module
    current_year <- reactive({ input$summary_current_year_total })
    comparison_year <- reactive({ input$summary_comparison_year_total })
    
    first_col_name <- "Industry"
    
    valueBoxServer("totalIndustry1_total", full_data_total, first_col_name, get_industry(1, full_data_total, current_year, first_col_name), current_year, comparison_year, "MtCO₂e")
    valueBoxServer("totalIndustry2_total", full_data_total, first_col_name, get_industry(2, full_data_total, current_year, first_col_name), current_year, comparison_year, "MtCO₂e")
    valueBoxServer("totalIndustry3_total", full_data_total, first_col_name, get_industry(3, full_data_total, current_year, first_col_name), current_year, comparison_year, "MtCO₂e")
    valueBoxServer("totalValue_total", full_data_total, first_col_name, reactive("Total"), current_year, comparison_year, "MtCO₂e")
    summaryLineChartServer("industryLineChart_total", line_chart_data,  "MtCO₂e")
    summaryBarChartServer("industryBarChart_total", full_data_total, current_year, comparison_year, first_col_name, "MtCO₂e")
  })
}

# Demo function to test the module
total_demo <- function() {
  ui <- fluidPage(totalEmissionsUI("total_test"))
  server <- function(input, output, session) {
    totalEmissionsServer("total_test")
  }
  shinyApp(ui, server)
}

total_demo()
