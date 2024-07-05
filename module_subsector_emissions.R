subsectorEmissionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        uiOutput(ns("variable_select")),
        actionButton(ns("select_all_button"), "Select All"),
        actionButton(ns("deselect_all_button"), "Deselect All"),
        sliderInput(ns("year"), "Select year range", value = c(1990, 2022), min = 1990, max = 2022, step = 1, sep = "", ticks = TRUE)
      ),
      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Summary Page",
                   value = ns("summary"),
                   fluidRow(
                     column(width = 4, sliderInput(ns("summary_current_year"), "Current Year", min = 1990, max = 2022, value = 2022, step = 1)),
                     column(width = 4, sliderInput(ns("summary_comparison_year"), "Comparison Year", min = 1990, max = 2022, value = 2021, step = 1)),
                     column(width = 4,
                            div(
                              style = "margin-top: 25px; text-align: center; border: 4px solid #e3e3e3",
                              h5("Adjust the sliders to compare data from different years.", style = "padding: 0px; font-weight: bold;")
                            )
                     )
                   ),
                   fluidRow(
                     column(width = 12, div(class = "header-text", "Top 3 Categories:"))
                   ),
                   generate_top_industries("subsector"),
                   fluidRow(
                     column(width = 12, div(class = "header-text", "Summary Analysis:"))
                   ),
                   generate_summary_bottom_row("subsector", "Subsector Emissions")
          ),
          tabPanel("Timelapse", timelapseBarChartUI(ns("bar")), value = ns("bar")),
          tabPanel("Line Chart", lineChartUI(ns("line")), value = ns("line")),
          tabPanel("Area Chart", areaChartUI(ns("area")), value = ns("area")),
          tabPanel("Data Table",
                   DTOutput(ns("data_table")),
                   downloadButton(ns("downloadData"), "Download Data"),
                   value = ns("data"))
        )
      )
    )
  )
}

subsectorEmissionsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    chart_data <- reactive({
      data <- subsector_total
      data <- data %>% filter(Year >= min(input$year) & Year <= max(input$year))
      if (!is.null(input$variables)) {
        data <- data %>%
          filter(Subsector %in% input$variables)
      }
      data
    })
    
    output$variable_select <- renderUI({
      choices <- unique(subsector_total$Subsector)
      selected <- setdiff(choices, "Total")
      checkboxGroupInput(ns("variables"), "Choose variables to add to chart", choices = choices, selected = selected)
    })
    
    observeEvent(input$select_all_button, {
      updateCheckboxGroupInput(session, ns("variables"), selected = setdiff(unique(subsector_total$Subsector), "Total"))
    })
    
    observeEvent(input$deselect_all_button, {
      updateCheckboxGroupInput(session, ns("variables"), selected = character(0))
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Agricultural Emissions by Subsector in Scotland",
      yAxisTitle = "Emissions (MtCO₂e)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;">Source: Scottish agriculture greenhouse gas emissions and nitrogen use 2022-23.</div>',
      x_col = "Year",
      y_col = "Value"
    )
    
    timelapseBarChartServer(
      id = "bar",
      chart_data = chart_data,
      title = "Agricultural Greenhouse Gas Emissions Timelapse",
      yAxisTitle = "Emissions (MtCO₂e)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;">Source: Scottish agriculture greenhouse gas emissions and nitrogen use 2022-23.</div>',
      x_col = "Year",
      y_col = "Value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Agricultural Greenhouse Gas Emissions by Subsector in Scotland",
      yAxisTitle = "Emissions (MtCO₂e)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;">Source: Scottish agriculture greenhouse gas emissions and nitrogen use 2022-23.</div>',
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
      chart_type = "Subsector",
      chart_data = chart_data,
      input = input,
      output = output,
      year_input = "year"
    )
    
    # Summary Module
    current_year <- reactive({ input$summary_current_year })
    comparison_year <- reactive({ input$summary_comparison_year })
    
    valueBoxServer(ns("totalIndustry1"), chart_data, "Subsector", get_industry(1, chart_data, current_year, "Subsector"), current_year, comparison_year)
    valueBoxServer(ns("totalIndustry2"), chart_data, "Subsector", get_industry(2, chart_data, current_year, "Subsector"), current_year, comparison_year)
    valueBoxServer(ns("totalIndustry3"), chart_data, "Subsector", get_industry(3, chart_data, current_year, "Subsector"), current_year, comparison_year)
    valueBoxServer(ns("totalValue"), chart_data, "Subsector", reactive("Total"), current_year, comparison_year)
    
    summaryBarChartServer(ns("industryBarChart"), chart_data, current_year, comparison_year, "Subsector")
  })
}
