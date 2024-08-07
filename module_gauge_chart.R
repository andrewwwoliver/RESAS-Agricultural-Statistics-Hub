#module_gauge_chart.R
library(shiny)
library(highcharter)

# UI for Gauge Chart Module
gaugeChartUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("title")),
    highchartOutput(ns("gauge_chart"), width = "100%", height = "300px"),
    htmlOutput(ns("footer"))
  )
}

# Server for Gauge Chart Module
gaugeChartServer <- function(id, chart_data, title, color = "#002d54", footer = "") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$title <- renderUI({
      HTML(paste0("<div style='font-size: 20px; font-weight: bold;'>", title, "</div>"))
    })
    
    output$footer <- renderUI({
      HTML(footer)
    })
    
    output$gauge_chart <- renderHighchart({
      highchart() %>%
        hc_chart(type = "solidgauge") %>%
        hc_pane(
          center = list('50%', '85%'),
          size = '140%',
          startAngle = -90,
          endAngle = 90,
          background = list(
            innerRadius = '60%',
            outerRadius = '100%',
            shape = 'arc'
          )
        ) %>%
        hc_yAxis(
          min = 0,
          max = 100,
          lineWidth = 0,
          minorTickInterval = NULL,
          tickPositions = NULL,  # Remove tick positions
          tickWidth = 0,  # Ensure tick width is set to 0
          labels = list(
            y = 20,  # Move labels down
            formatter = JS("function() { if (this.value === 0 || this.value === 100) return this.value; return ''; }")
          ) # Add 0 and 100 labels
        ) %>%
        hc_add_series(
          name = "Percentage",
          data = list(
            list(
              y = chart_data(),
              color = color  # Set the color for the gauge
            )
          ),
          tooltip = list(valueSuffix = " %"),
          dataLabels = list(
            format = '<div style="text-align:center"><span style="font-size:25px">{y:.1f}</span><br/>
                      <span style="font-size:12px;opacity:0.4">%</span></div>'
          ),
          innerRadius = '60%',
          radius = '100%'
        ) %>%
        hc_plotOptions(
          solidgauge = list(
            dataLabels = list(
              y = 5,
              borderWidth = 0,
              useHTML = TRUE
            )
          )
        )
    })
  })
}