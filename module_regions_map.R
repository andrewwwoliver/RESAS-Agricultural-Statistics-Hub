# File: module_regions_map.R

library(shiny)
library(highcharter)
library(geojsonio)
library(dplyr)

# Load the GeoJSON file
region_geojson_data <- geojson_read("regions_simplified.geojson", what = "sp")

# Convert GeoJSON to a Highcharts-compatible format
region_geojson_list <- geojson_list(region_geojson_data)

mapRegionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    mainPanel(
      htmlOutput(ns("title")),
      highchartOutput(ns("map"), height = "75vh"),  # Set the height to be responsive
      htmlOutput(ns("footer")),
      div(
        class = "note",
        style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
        HTML(
          "<strong>Note:</strong><ul>
            <li>To change the data shown, select a variable from the radio buttons within the sidebar.</li>
            <li>You can see data values for each variable by hovering your mouse over the region.</li>
            <li>To change the zoom level, use the + and - to the left of the graph, or scroll using your mouse wheel.</li>
          </ul>"
        )
      )
    )
  )
}

mapRegionsServer <- function(id, data, variable, unit = "", title, footer) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$title <- renderUI({
      HTML(paste0("<div style='font-size: 20px; font-weight: bold;'>", title, "</div>"))
    })
    
    output$footer <- renderUI({
      HTML(footer)
    })
    
    filtered_data <- reactive({
      req(variable)
      req(variable())
      req(data())
      
      filtered <- data() %>%
        select(region, value) %>%
        mutate(region = as.character(region))
      
      filtered
    })
    
    output$map <- renderHighchart({
      data <- filtered_data()
      
      hc_data <- data %>% 
        list_parse()
      
      variable_name <- variable()
      
      highchart(type = "map") %>%
        hc_add_series(
          mapData = region_geojson_list, 
          joinBy = c("region", "region"),
          data = hc_data,
          borderColor = "#FFFFFF",
          borderWidth = 0.5,
          states = list(
            hover = list(
              color = "#BADA55"
            )
          ),
          dataLabels = list(
            enabled = FALSE
          ),
          tooltip = list(
            useHTML = TRUE,
            headerFormat = "<b>{point.key}</b><br/>",
            pointFormatter = JS(sprintf("function() {
              var value = this.value;
              var formattedValue;
              if (value >= 1000) {
                formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 0, maximumFractionDigits: 0});
              } else {
                formattedValue = value.toLocaleString(undefined, {minimumFractionDigits: 1, maximumFractionDigits: 2});
              }
              return '<b>' + this.region + '</b><br/>' +
                     '%s: ' + formattedValue + ' %s';
            }", variable_name, unit))
          ),
          nullColor = '#E0E0E0'
        ) %>%
        hc_mapNavigation(enabled = TRUE) %>%
        hc_colorAxis(
          min = 0,
          stops = color_stops(5),
          labels = list(
            format = "{value:,.0f}"
          )
        ) %>%
        hc_chart(reflow = TRUE) %>%
        hc_legend(
          layout = "horizontal",
          align = "center",
          verticalAlign = "bottom",
          title = list(text = "Legend", style = list(fontSize = '15px')),
          itemStyle = list(width = '100px')
        )
    })
  })
}
