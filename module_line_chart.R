# module_line_chart.R

lineChartUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("title")),
    highchartOutput(ns("line_chart")),
    htmlOutput(ns("footer")),
    div(
      class = "note",
      style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
      HTML(
        "<strong>Note:</strong><ul>
          <li>To remove a series from the chart, deselect the variable from the sidebar menu.</li>
          <li>Select a year range by adjusting the slider on the sidebar or by zooming into the graph by clicking and dragging over an area you wish to see.</li>
          <li>You can see data values for a specific year by hovering your mouse over the line.</li>
        </ul>"
      )
    )
  )
}

lineChartServer <- function(id, chart_data, group_column, title, yAxisTitle, xAxisTitle, footer, x_col, y_col) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    reactive_colors <- reactive({ assign_colors(chart_data(), preset_colors) })
    
    output$title <- renderUI({
      year_min <- min(chart_data()[[x_col]], na.rm = TRUE)
      year_max <- max(chart_data()[[x_col]], na.rm = TRUE)
      HTML(paste0("<div style='font-size: 20px; font-weight: bold;'>", title, ", ", year_min, " to ", year_max, "</div>"))
    })
    
    output$footer <- renderUI({
      HTML(footer)
    })
    
    output$line_chart <- renderHighchart({
      data <- chart_data()
      colors <- reactive_colors()
      hc <- highchart() %>%
        hc_chart(type = "line", zoomType = "xy") %>%
        hc_yAxis(title = list(text = yAxisTitle)) %>%
        hc_xAxis(title = list(text = xAxisTitle), type = "category", tickInterval = 5) %>%
        hc_legend(align = "left", alignColumns = FALSE, layout = "horizontal") %>%
        hc_plotOptions(line = list(colorByPoint = FALSE)) %>%
        hc_add_theme(thm)
      
      unique_groups <- unique(data[[group_column]])
      lapply(unique_groups, function(g) {
        hc <<- hc %>%
          hc_add_series(name = g, data = data[data[[group_column]] == g, ] %>% select(x = !!sym(x_col), y = !!sym(y_col)), color = colors[[g]])
      })
      
      hc
    })
  })
}
