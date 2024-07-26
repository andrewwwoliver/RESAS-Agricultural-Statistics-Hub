# Line Chart UI Module
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
          <li>To add or remove a series from the chart, select/deselect the variable from the sidebar menu.</li>
          <li>Select a year range by adjusting the slider on the sidebar or by zooming into the graph by clicking and dragging over an area you wish to see.</li>
          <li>You can see data values for a specific year by hovering your mouse over the line.</li>
        </ul>"
      )
    )
  )
}



lineChartServer <- function(id, chart_data, title, yAxisTitle, xAxisTitle, footer, x_col, y_col) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    reactive_colors <- reactive({ assign_colors(chart_data(), preset_colors) })
    
    output$title <- renderUI({
      year_min <- min(as.numeric(chart_data()[[x_col]]), na.rm = TRUE)
      year_max <- max(as.numeric(chart_data()[[x_col]]), na.rm = TRUE)
      HTML(paste0("<div style='font-size: 20px; font-weight: bold;'>", title, ", ", year_min, " to ", year_max, "</div>"))
    })
    
    output$footer <- renderUI({
      HTML(footer)
    })
    
    output$line_chart <- renderHighchart({
      data <- chart_data()
      colors <- reactive_colors()
      group_column <- setdiff(names(data), c(x_col, y_col))[1] # Assuming only one group column
      

      
      hc <- highchart() %>%
        hc_chart(type = "line", zoomType = "xy") %>%
        hc_yAxis(title = list(text = yAxisTitle)) %>%
        hc_xAxis(title = list(text = xAxisTitle), type = "category", tickInterval = 5) %>%
        hc_plotOptions(line = list(colorByPoint = FALSE)) %>%
        hc_legend(align = "left", alignColumns = FALSE, layout = "horizontal") %>%
        hc_add_theme(thm)
      
      unique_groups <- unique(data[[group_column]])
      lapply(unique_groups, function(g) {
        series_data <- data[data[[group_column]] == g, ]
        hc <<- hc %>%
          hc_add_series(name = g, data = list_parse2(series_data %>% transmute(x = as.numeric(!!sym(x_col)), y = !!sym(y_col))), color = colors[[g]])

      })
      
      hc
    })
  })
}
