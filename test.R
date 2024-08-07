# Load necessary libraries
library(highcharter)
library(dplyr)

# Load the RData file
load("module_2023.RData")

# Extract the necessary data from the combined data frame
combined_nutrient_mgmt <- combined_nutrient_mgmt
soil_testing_percentage <- combined_nutrient_mgmt %>%
  filter(`Soil nutrient management` == "Soil testing on grassland in last five years") %>%
  pull(`Percentage of holdings`)

# Define the gauge chart
highchart() %>%
  hc_chart(type = "solidgauge") %>%
  hc_title(text = "Soil Testing on Grassland in Last Five Years") %>%
  hc_pane(
    center = list('50%', '85%'),
    size = '140%',
    startAngle = -90,
    endAngle = 90,
    background = list(
      backgroundColor = '#EEE',
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
    ),  # Add 0 and 100 labels
    title = list(y = -70)
  ) %>%
  hc_add_series(
    name = "Percentage",
    data = list(soil_testing_percentage),
    tooltip = list(valueSuffix = " %"),
    dataLabels = list(
      format = '<div style="text-align:center"><span style="font-size:25px">{y:.1f}</span><br/>
                <span style="font-size:12px;opacity:0.4">%</span></div>'
    ),
    innerRadius = '60%',
    radius = '100%',
    color = "#FF5733",  # Single color for the gauge
    backgroundColor = "#EEE",  # Color for the rest of the gauge
    borderWidth = 0,
    borderColor = NULL
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
