
library(shiny)
library(highcharter)
library(dplyr)
source("module_gauge_chart.R")

load("module_2023.RData")

# Custom CSS to remove gaps
custom_css <- "
.gauge-title {
  margin-bottom: 0px;
  padding-bottom: 0px;
}
.gauge-footer {
  margin-top: 0px;
  padding-top: 0px;
}
"

# UI for Soil Testing Module
soilTestingUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(custom_css)),  # Include custom CSS
    fluidRow(
      column(6, gaugeChartUI(ns("soilTestingGaugeEither"))),
      column(6, gaugeChartUI(ns("soilTestingGaugeChange")))
    ),
    fluidRow(
      column(6, gaugeChartUI(ns("soilTestingGaugeGrassland"))),
      column(6, gaugeChartUI(ns("soilTestingGaugeCropland"))),
    ),
    fluidRow(
      column(6, gaugeChartUI(ns("soilTestingGaugePhGrassland"))),
      column(6, gaugeChartUI(ns("soilTestingGaugePhCropland"))),
    )
  )
}

# Server for Soil Testing Module
soilTestingServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data <- reactive({ combined_nutrient_mgmt })
    
    chart_data_grassland <- reactive({
      req(data())
      data() %>%
        filter(`Soil nutrient management` == "Soil testing on grassland in last five years") %>%
        pull(`Percentage of holdings`)
    })
    
    chart_data_cropland <- reactive({
      req(data())
      data() %>%
        filter(`Soil nutrient management` == "Soil testing on cropland in last five years") %>%
        pull(`Percentage of holdings`)
    })
    
    chart_data_either <- reactive({
      req(data())
      data() %>%
        filter(`Soil nutrient management` == "Soil testing on either grass or crops in last five years") %>%
        pull(`Percentage of holdings`)
    })
    
    chart_data_change <- reactive({
      req(data())
      data() %>%
        filter(`Soil nutrient management` == "Soil testing resulted in change of crop nutrient application") %>%
        pull(`Percentage of holdings`)
    })
    
    chart_data_ph_grassland <- reactive({
      req(data())
      data() %>%
        filter(`Soil nutrient management` == "Regular pH testing on grassland") %>%
        pull(`Percentage of holdings`)
    })
    
    chart_data_ph_cropland <- reactive({
      req(data())
      data() %>%
        filter(`Soil nutrient management` == "Regular pH testing on cropland") %>%
        pull(`Percentage of holdings`)
    })
    
    gaugeChartServer(
      id = "soilTestingGaugeGrassland",
      chart_data = chart_data_grassland,
      title = "Soil Testing on Grassland in Last Five Years",
      color = "#6a2063",
      footer = "Source: Combined Nutrient Management Data"
    )
    
    gaugeChartServer(
      id = "soilTestingGaugeCropland",
      chart_data = chart_data_cropland,
      title = "Soil Testing on Cropland in Last Five Years",
      color = "#e5682a",
      footer = "Source: Combined Nutrient Management Data"
    )
    
    gaugeChartServer(
      id = "soilTestingGaugeEither",
      chart_data = chart_data_either,
      title = "Soil Testing on Either Grass or Crops in Last Five Years",
      color = "#002d54",
      footer = "Source: Combined Nutrient Management Data"
    )
    
    gaugeChartServer(
      id = "soilTestingGaugeChange",
      chart_data = chart_data_change,
      title = "Soil Testing Resulted in Change of Crop Nutrient Application",
      color = "#2b9c93",
      footer = "Source: Combined Nutrient Management Data"
    )
    
    
    
    gaugeChartServer(
      id = "soilTestingGaugePhGrassland",
      chart_data = chart_data_ph_grassland,
      title = "Regular pH Testing on Grassland",
      color = "#0b4c0b",
      footer = "Source: Combined Nutrient Management Data"
    )
    
    gaugeChartServer(
      id = "soilTestingGaugePhCropland",
      chart_data = chart_data_ph_cropland,
      title = "Regular pH Testing on Cropland",
      color = "#5d9f3c",
      footer = "Source: Combined Nutrient Management Data"
    )
  })
}

# Testing function for Soil Testing Module
soilTestingDemo <- function() {
  ui <- fluidPage(soilTestingUI("soil_testing_demo"))
  server <- function(input, output, session) {
    soilTestingServer("soil_testing_demo")
  }
  shinyApp(ui, server)
}

soilTestingDemo()