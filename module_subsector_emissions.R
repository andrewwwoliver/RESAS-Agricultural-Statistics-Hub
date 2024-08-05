source("module_summary.R")

# Data setup for Subsector Emissions
full_data_subsector <- reactive({ subsector_total })
units_subsector <- "MtCO₂e"

# UI for Subsector Emissions Module
subsectorEmissionsUI <- function(id) {
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
          sliderInput(ns("summary_current_year_subsector"), "Current Year", min = 1990, max = 2022, value = 2022, step = 1, sep = ""),
          sliderInput(ns("summary_comparison_year_subsector"), "Comparison Year", min = 1990, max = 2022, value = 2021, step = 1, sep = "")
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
                     column(width = 12, div(class = "header-text", "Top 3 Emitting Subsectors:"))
                   ),
                   fluidRow(
                     column(width = 4, valueBoxUI(ns("totalIndustry1_subsector")), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 4, valueBoxUI(ns("totalIndustry2_subsector")), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 4, valueBoxUI(ns("totalIndustry3_subsector")), style = "padding-right: 0; padding-left: 0;")
                   ),
                   fluidRow(
                     column(width = 12, div(class = "header-text", "Summary Analysis:"))
                   ),
                   fluidRow(
                     column(width = 4, valueBoxUI(ns("totalValue_subsector")), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 4, chartUI(ns("industryPieChart_subsector"), "Subsector Breakdown"), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 4, chartUI(ns("industryBarChart_subsector"), "Emissions by Category"), style = "padding-right: 0; padding-left: 0;")
                   )
          ),
          tabPanel("Timelapse", timelapseBarChartUI(ns("timelapse_bar")), value = "Timelapse"),
          tabPanel("Breakdown", highchartOutput(ns("breakdown")), value = "Breakdown"),
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
# File: module_subsector_emissions.R

# Server for Subsector Emissions Module
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
      unit = "MtCO₂e",
      footer = '<div style="font-size: 16px; font-weight: bold;">Source: Scottish agriculture greenhouse gas emissions and nitrogen use 2022-23.</div>',
      x_col = "Year",
      y_col = "Value"
    )
    
    timelapseBarChartServer(
      id = "timelapse_bar",
      chart_data = chart_data,
      title = "Agricultural Greenhouse Gas Emissions Timelapse",
      yAxisTitle = "Emissions (MtCO₂e)",
      xAxisTitle = "Year",
      unit = "MtCO₂e",
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
      unit = "MtCO₂e",
      footer = '<div style="font-size: 16px; font-weight: bold;">Source: Scottish agriculture greenhouse gas emissions and nitrogen use 2022-23.</div>',
      x_col = "Year",
      y_col = "Value"
    )
    
    # Breakdown chart server logic
    output$breakdown <- renderHighchart({
      data_long <- subsector_source %>%
        pivot_longer(cols = -Source, names_to = "Subsector", values_to = "Value") %>%
        arrange(match(Source, rev(unique(Source))))
      
      unique_sources <- unique(data_long$Source)
      color_map <- setNames(rev(preset_colors)[1:length(unique_sources)], unique_sources)
      
      series_data <- lapply(unique_sources, function(source) {
        list(
          name = source,
          data = data_long %>%
            filter(Source == source) %>%
            arrange(match(Subsector, unique(data_long$Subsector))) %>%
            pull(Value),
          color = color_map[[source]]
        )
      })
      
      highchart() %>%
        hc_chart(type = "bar", zoomType = "xy") %>%
        hc_xAxis(categories = unique(data_long$Subsector), 
                 labels = list(style = list(color = "#000000", fontSize = '16px', fontFamily = 'Arial'))) %>%
        hc_yAxis(title = list(text = "Emissions (MtCO₂e)", style = list(color = "#000000", fontSize = '16px', fontFamily = 'Arial')),
                 labels = list(style = list(color = "#000000", fontSize = '16px', fontFamily = 'Arial')),
                 tickInterval = 0.5) %>%
        hc_plotOptions(bar = list(
          stacking = "normal",
          groupPadding = 0,
          pointPadding = 0.1,
          borderWidth = 0
        )) %>%
        hc_tooltip(
          useHTML = TRUE,
          headerFormat = "<span style='font-size: 16px; font-family: Arial'><b>{point.key}</b></span><br/>",
          pointFormatter = JS("function() {
            var value = this.y;
            var formattedValue;
            if (value >= 1000) {
              formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 0, maximumFractionDigits: 0});
            } else {
              formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 1, maximumFractionDigits: 2});
            }
            return this.series.name + ': ' + formattedValue + ' MtCO₂e';
          }"),
          style = list(fontSize = "16px", fontFamily = "Arial")
        ) %>%
        hc_legend(
          align = "right", 
          verticalAlign = "middle", 
          layout = "vertical",
          itemStyle = list(fontSize = '16px', fontFamily = 'Arial', fontWeight = 'normal')
        ) %>%
        hc_add_series_list(series_data)
    })
    
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
    
    current_year <- reactive({ input$summary_current_year_subsector })
    comparison_year <- reactive({ input$summary_comparison_year_subsector })
    
    first_col_name <- "Subsector"
    
    valueBoxServer(
      id = "totalIndustry1_subsector",
      data = full_data_subsector,
      category = first_col_name,
      industry = get_industry(1, full_data_subsector, current_year, first_col_name),
      current_year = current_year,
      comparison_year = comparison_year,
      unit = "MtCO₂e"
    )
    valueBoxServer(
      id = "totalIndustry2_subsector",
      data = full_data_subsector,
      category = first_col_name,
      industry = get_industry(2, full_data_subsector, current_year, first_col_name),
      current_year = current_year,
      comparison_year = comparison_year,
      unit = "MtCO₂e"
    )
    valueBoxServer(
      id = "totalIndustry3_subsector",
      data = full_data_subsector,
      category = first_col_name,
      industry = get_industry(3, full_data_subsector, current_year, first_col_name),
      current_year = current_year,
      comparison_year = comparison_year,
      unit = "MtCO₂e"
    )
    valueBoxServer(
      id = "totalValue_subsector",
      data = full_data_subsector,
      category = first_col_name,
      industry = reactive("Total"),
      current_year = current_year,
      comparison_year = comparison_year,
      unit = "MtCO₂e"
    )
    summaryPieChartServer(
      id = "industryPieChart_subsector",
      data = full_data_subsector,
      current_year = current_year,
      category = first_col_name,
      unit = "MtCO₂e"
    )
    summaryBarChartServer(
      id = "industryBarChart_subsector",
      data = full_data_subsector,
      current_year = current_year,
      comparison_year = comparison_year,
      category = first_col_name,
      unit = "MtCO₂e"
    )
    
  })
}

# Demo function to test the module
subsector_demo <- function() {
  ui <- fluidPage(subsectorEmissionsUI("subsector_test"))
  server <- function(input, output, session) {
    subsectorEmissionsServer("subsector_test")
  }
  shinyApp(ui, server)
}

subsector_demo()
