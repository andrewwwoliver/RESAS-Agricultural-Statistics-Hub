library(shiny)
library(highcharter)
library(geojsonio)
library(dplyr)

# Load the GeoJSON file
geojson_data <- geojson_read("subregions_simplified.geojson", what = "sp")

# Convert GeoJSON to a Highcharts-compatible format
geojson_list <- geojson_list(geojson_data)

mapUI <- function(id) {
  ns <- NS(id)
  tagList(
    highchartOutput(ns("map"), height = "75vh")
  )
}

mapServer <- function(id, data, variable, title) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    filtered_data <- reactive({
      req(variable)  # Ensure that variable is not null or missing
      req(variable())  # Ensure that variable() is not null or missing
      req(data())      # Ensure that data() is not null or missing
      
      first_col_name <- names(data())[1]  # Get the name of the first column dynamically
      data() %>%
        filter(!!sym(first_col_name) == variable())  # Use the first column name for filtering
    })
    
    output$map <- renderHighchart({
      data <- filtered_data()
      hc_data <- data %>% 
        mutate(sub_region = as.character(sub_region)) %>%
        select(sub_region, value) %>%
        list_parse()
      
      variable_name <- variable()  # Get the selected variable name
      
      highchart(type = "map") %>%
        hc_add_series(
          mapData = geojson_list, 
          joinBy = c("sub_region", "sub_region"),
          data = hc_data,
          borderColor = "#FFFFFF",
          borderWidth = 0.5,
          states = list(
            hover = list(
              color = "#BADA55"
            )
          ),
          dataLabels = list(
            enabled = FALSE  # Disable the overlays
          ),
          tooltip = list(
            useHTML = TRUE,
            headerFormat = "<b>{point.key}</b><br/>",
            pointFormatter = JS(sprintf("function() {
              return '<b>' + this.sub_region + '</b><br/>' +
                     '%s: ' + this.value;
            }", variable_name))
          ),
          nullColor = '#E0E0E0'  # Color for regions with no data
        ) %>%
        hc_mapNavigation(enabled = TRUE) %>%
        hc_colorAxis(
          min = 0,
          stops = color_stops(5),
          labels = list(
            format = "{value:,.0f}"  # Ensure the labels show the correct values
          )
        ) %>%
        hc_title(text = title) %>%
        hc_chart(reflow = TRUE) %>% # Make chart responsive
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
