#module_summary.R

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(highcharter)

# Function to create a small line plot for the value boxes
small_line_plot <- function(data, color) {
  ggplot(data, aes(x = Year, y = Value)) +
    geom_line(color = "#002d54") +
    theme_void() +
    theme(plot.background = element_rect(fill = "transparent", color = NA))
}

# Function to create an arrow for Year on Year change
create_yoy_arrow <- function(change) {
  if (is.na(change) || is.nan(change)) {
    icon("minus", style = "color: grey;")
  } else if (change > 0) {
    icon("arrow-up", style = "color: #2b9c93;")
  } else {
    icon("arrow-down", style = "color: #002d54;")
  }
}



# UI Module for Value Box
valueBoxUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("valueBox"))
}

# Server Module for Value Box
valueBoxServer <- function(id, data, category, industry, current_year, comparison_year, unit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Ensure the data is filtered and processed correctly
    reactive_data <- reactive({ 
      data() %>% filter(!!sym(category) == !!industry(), Year %in% c(current_year(), comparison_year())) 
    })
    
    output$valueBox <- renderUI({
      data_filtered <- reactive_data()
      current_value <- data_filtered %>% filter(Year == current_year()) %>% summarise(Value = sum(Value, na.rm = TRUE)) %>% pull(Value)
      comparison_value <- data_filtered %>% filter(Year == comparison_year()) %>% summarise(Value = sum(Value, na.rm = TRUE)) %>% pull(Value)
      yoy_change <- if (comparison_value == 0 || is.na(comparison_value)) NA else ((current_value - comparison_value) / comparison_value) * 100
      
      formatted_value <- format_number(current_value)  # Use format_number to format the value
      
      # Use HTML to inject the formatted value directly
      box(
        class = "value-box",
        title = NULL,
        width = 12,
        solidHeader = TRUE,
        div(
          style = "display: flex; flex-direction: column; justify-content: space-between; height: 100%; padding: 5px;",
          div(
            style = "flex: 1; margin-bottom: 5px;",
            h5(class = "value-box-title", industry()),
            div(
              style = "display: flex; align-items: baseline; margin-bottom: 5px;",
              h3(HTML(formatted_value), style = "margin: 0;"),  # Injecting HTML directly to force correct rendering
              span(class = "value-box-units", unit)
            ),
            div(
              style = "display: flex; align-items: center; margin-bottom: 5px;",
              create_yoy_arrow(yoy_change),
              span(class = "value-box-yoy", 
                   ifelse(is.na(yoy_change), "NA", sprintf("%+.2f%% %d vs. %d", yoy_change, current_year(), comparison_year())), 
                   style = ifelse(yoy_change > 0, "color: #2b9c93; margin-left: 5px;", "color: #002d54; margin-left: 5px;"))
            )
          ),
          div(
            style = "margin-top: 10px;",  
            plotOutput(ns("sparkline"), height = "30px", width = "100%")
          )
        )
      )
    })
    
    output$sparkline <- renderPlot({
      small_line_plot(data() %>% filter(!!sym(category) == !!industry()), "#28a745")
    })
  })
}









# UI Module for Chart
chartUI <- function(id, title) {
  ns <- NS(id)
  box(
    title = span(class = "box-title", title),
    width = 12,
    solidHeader = TRUE,
    div(class = "box-content", highchartOutput(ns("chartOutput"), height = "300px"))
  )
}


# Server Module for Line Chart on Summary Page
summaryLineChartServer <- function(id, data, unit = "") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$chartOutput <- renderHighchart({
      summary_line_data <- data()
      variable_col <- names(summary_line_data)[1]  # Get the first column name, which is the variable name
      
      series_list <- lapply(unique(summary_line_data[[variable_col]]), function(variable) {
        df <- summary_line_data %>% filter(!!sym(variable_col) == variable)
        list(
          name = variable,
          data = df %>% select(Year, Value) %>% list_parse2()
        )
      })
      
      highchart() %>%
        hc_chart(type = "line", zoomType = "xy") %>%
        hc_xAxis(categories = unique(summary_line_data$Year)) %>%
        hc_yAxis(title = list(text = unit)) %>%
        hc_legend(align = "left", alignColumns = FALSE, layout = "horizontal") %>%
        hc_plotOptions(line = list(marker = list(enabled = FALSE))) %>%  # Disable markers
        hc_tooltip(
          useHTML = TRUE,
          headerFormat = "<b>{point.key}</b><br/>",
          pointFormatter = JS(sprintf("function() {
            var value = this.y;
            var formattedValue;
            if (value >= 1000) {
              formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 0, maximumFractionDigits: 0});
            } else {
              formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 1, maximumFractionDigits: 2});
            }
            return this.series.name + ': ' + formattedValue + ' %s';
          }", unit))
        ) %>%
        hc_add_theme(thm) %>%
        hc_add_series_list(series_list)
    })
  })
}

# Server Module for Pie Chart
summaryPieChartServer <- function(id, data, current_year, category, unit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$chartOutput <- renderHighchart({
      pie_data <- data() %>% filter(Year == current_year() & !!sym(category) != "Total") %>% 
        group_by(!!sym(category)) %>% summarise(Value = sum(Value, na.rm = TRUE))
      
      highchart() %>%
        hc_chart(type = "pie") %>%
        hc_series(list(data = list_parse(pie_data %>% transmute(name = !!sym(category), y = Value)))) %>%
        hc_plotOptions(pie = list(
          dataLabels = list(enabled = FALSE),
          showInLegend = TRUE
        )) %>%
        hc_tooltip(
          useHTML = TRUE,
          pointFormat = sprintf(' {point.y:.2f} %s ({point.percentage:.2f}%%)', unit)
        )
    })
  })
}



# Server Module for Bar Chart
summaryBarChartServer <- function(id, data, current_year, comparison_year, category, unit = "") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$chartOutput <- renderHighchart({
      bar_data <- data() %>% filter(Year == current_year() & !!sym(category) != "Total") %>% group_by(!!sym(category)) %>% summarise(Value = sum(Value, na.rm = TRUE))
      line_data <- data() %>% filter(Year == comparison_year() & !!sym(category) != "Total") %>% group_by(!!sym(category)) %>% summarise(Value = sum(Value, na.rm = TRUE))
      
      colors <- c("#002d54", "#2b9c93", "#6a2063", "#e5682a", "#0b4c0b", "#5d9f3c", "#592c20", "#ca72a2")
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_xAxis(categories = bar_data[[category]]) %>%
        hc_yAxis(title = list(text = unit)) %>%
        hc_add_series(
          name = as.character(current_year()), 
          data = bar_data$Value, 
          type = "bar", 
          colorByPoint = TRUE, 
          colors = colors
        ) %>%
        hc_add_series(
          name = as.character(comparison_year()), 
          data = line_data$Value, 
          type = "scatter", 
          color = "#ff0000", 
          marker = list(enabled = TRUE, symbol = "circle", lineWidth = 2, radius = 3)
        ) %>%
        hc_plotOptions(series = list(groupPadding = 0, pointPadding = 0.1, borderWidth = 0)) %>%
        hc_tooltip(
          useHTML = TRUE,
          shared = TRUE,
          pointFormatter = JS(sprintf("function() {
            var value = this.y;
            var formattedValue;
            if (value >= 1000) {
              formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 0, maximumFractionDigits: 0});
            } else {
              formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 1, maximumFractionDigits: 2});
            }
            return this.series.name + ': ' + formattedValue + ' %s';
          }", unit))
        ) %>%
        hc_add_theme(thm)
    })
  })
}


# Function to get top industries
get_industry <- function(index, data, current_year, first_col_name) {
  reactive({
    industries <- data() %>%
      filter(Year == current_year() & !!sym(first_col_name) != "Total") %>%
      group_by(!!sym(first_col_name)) %>%
      summarise(Value = sum(Value, na.rm = TRUE)) %>%
      arrange(desc(Value)) %>%
      slice_head(n = 3) %>%
      pull(!!sym(first_col_name))
    if (length(industries) >= index) {
      industries[index]
    } else {
      NA
    }
  })
}
