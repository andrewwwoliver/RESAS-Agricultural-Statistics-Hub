# File: module_bar_chart.R

barChartUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("title")),
    highchartOutput(ns("bar_chart")),
    htmlOutput(ns("footer")),
    div(
      class = "note",
      style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
      HTML(
        "<strong>Note:</strong><ul>
          <li>You can see data values for a specific variable by hovering your mouse over the bars.</li>
        </ul>"
      )
    )
  )
}

barChartServer <- function(id, chart_data, title, yAxisTitle, xAxisTitle, footer, x_col, y_col, tooltip_format) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    reactive_colors <- reactive({ assign_colors(chart_data(), preset_colors) })
    
    output$title <- renderUI({
      HTML(paste0("<div style='font-size: 20px; font-weight: bold;'>", title, "</div>"))
    })
    
    output$footer <- renderUI({
      HTML(footer)
    })
    
    output$bar_chart <- renderHighchart({
      data <- chart_data()
      if (nrow(data) == 0) return(NULL)
      colors <- reactive_colors()
      group_column <- setdiff(names(data), c(x_col, y_col()))[1]
      
      data <- data %>%
        arrange(desc(!!sym(y_col())))  # Sort data by value in descending order
      
      highchart() %>%
        hc_chart(type = "bar", inverted = TRUE, zoomType = "xy") %>%
        hc_xAxis(categories = data[[x_col]], title = list(text = xAxisTitle)) %>%
        hc_yAxis(title = list(text = yAxisTitle()), allowDecimals = FALSE) %>%
        hc_plotOptions(bar = list(
          dataLabels = list(enabled = FALSE),
          colorByPoint = TRUE,
          groupPadding = 0,
          pointPadding = 0.1,
          borderWidth = 0
        )) %>%
        hc_add_series(
          name = "",
          data = data %>% mutate(y = !!sym(y_col()), color = colors[!!sym(x_col)]) %>% 
            select(name = !!sym(x_col), y, color),
          colorByPoint = TRUE,
          showInLegend = FALSE
        ) %>%
        hc_tooltip(
          style = list(
            fontFamily = "Arial, sans-serif",
            fontSize = "16px",
            color = "black"
          ),
          headerFormat = "<b>{point.key}</b><br/>",  
          pointFormat = paste0(tooltip_format())
        ) %>%
        hc_add_theme(thm)
    })
  })
}
