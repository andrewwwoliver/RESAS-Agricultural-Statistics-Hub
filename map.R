library(sf)
library(rmapshaper)
library(highcharter)
library(dplyr)
library(geojsonio)



geojson_data <- geojson_read("subregions_simplified.geojson", what = "sp")


# Convert GeoJSON to a Highcharts-compatible format
geojson_list <- geojson_list(geojson_data)


# Sample data frame for demonstration purposes
regions_data <- data.frame(
  sub_region = c("Shetland", "Orkney", "Na h-Eileanan Siar", "Highland", 
                 "Grampian", "Tayside", "Fife", "Lothian", "Scottish Borders", 
                 "East Central", "Argyll & Bute", "Clyde Valley", "Ayrshire", 
                 "Dumfries & Galloway"),
  value = c(24, 37, 7, 90, 153, 55, 47, 37, 46, 67, 82, 221, 320, 503)
)

# Create the map with correct joinBy properties
highchart(type = "map") %>%
  hc_add_series(
    mapData = geojson_list, 
    name = "Regions Data",
    joinBy = c("sub_region", "sub_region"),  # Make sure these properties match
    data = regions_data,
    borderColor = "#FFFFFF",
    borderWidth = 0.5,
    states = list(
      hover = list(
        color = "#BADA55"
      )
    ),
    dataLabels = list(
      enabled = TRUE,
      format = '{point.name}'
    ),
    tooltip = list(
      pointFormat = '{point.name}: {point.value}'
    )
  ) %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_colorAxis(
    min = 0,
    max = 600,
    stops = color_stops(5)
  ) %>%
  hc_title(text = "Custom Regions and Sub-regions Map")
