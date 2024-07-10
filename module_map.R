library(highcharter)
library(geojsonio)
library(dplyr)
library(shiny)
library(tidyr)

# Load the GeoJSON file
geojson_data <- geojson_read("subregions_simplified.geojson", what = "sp")

# Convert GeoJSON to a Highcharts-compatible format
geojson_list <- geojson_list(geojson_data)

# Coerce all relevant columns to character before pivoting
occupiers_employees_subregion <- occupiers_employees_subregion %>%
  mutate(across(-`Occupiers and employees by category`, as.character))

# Transform the data
regions_data <- occupiers_employees_subregion %>% 
  select(-Scotland) %>% 
  pivot_longer(cols = -`Occupiers and employees by category`, names_to = "sub_region", values_to = "value") %>%
  mutate(value = ifelse(value == "c", NA, as.numeric(value))) # Convert 'c' to NA and the rest to numeric

# Filter for the specific categories
categories <- c("Regular full-time staff total", 
                "Regular part-time staff total", 
                "Total Casual and seasonal staff", 
                "Total agricultural workforce")

filtered_regions_data <- regions_data %>%
  filter(`Occupiers and employees by category` %in% categories)

mapUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      radioButtons(ns("variable"), "Select Variable", choices = categories)
    ),
    mainPanel(
      highchartOutput(ns("map"), height = "75vh"),  # Set the height to be responsive
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

mapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    filtered_data <- reactive({
      filtered_regions_data %>%
        filter(`Occupiers and employees by category` == input$variable)
    })
    
    output$map <- renderHighchart({
      data <- filtered_data()
      hc_data <- data %>% 
        mutate(sub_region = as.character(sub_region)) %>%
        select(sub_region, value) %>%
        list_parse()
      
      variable_name <- input$variable  # Get the selected variable name
      
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
        hc_title(text = "Occupiers and Employees by Region") %>%
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

# Testing module
content_demo <- function() {
  ui <- fluidPage(mapUI("map_test"))
  server <- function(input, output, session) {
    mapServer("map_test")
  }
  shinyApp(ui, server)
}

content_demo()
