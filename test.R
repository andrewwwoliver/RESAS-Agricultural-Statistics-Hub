# File: module_manure.R

library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(DT)
library(shinydashboard)

# Load the data
load("module_2023.RData")

# Convert relevant columns to character before pivoting
manure_qty <- manure_qty %>%
  mutate(across(everything(), as.character))

# Transform the data, filtering out "All respondents"
manure_data_long <- manure_qty %>%
  pivot_longer(cols = -Region, names_to = "variable", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  rename(region = Region)

# Extract the data for "All respondents"
national_data <- manure_qty %>%
  filter(Region == "All respondents") %>%
  select(-Region) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# List of variables for radio buttons
variables <- colnames(manure_qty)[-1]

source("module_regions_map.R")

# UI for the value boxes
valueBoxUI <- function(id, title, value, unit) {
  box(
    title = title,
    width = 12,
    status = "primary",
    solidHeader = TRUE,
    valueBox(
      value = value,
      subtitle = unit,
      icon = icon("info-circle"),
      width = 12
    )
  )
}

manureUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width = 3,
           uiOutput(ns("sidebar_ui"))
    ),
    column(width = 3,
           fluidRow(
             valueBoxUI(ns("total_manure"), "Total Manure Area", national_data$value[national_data$variable == "Manure"], "tons"),
             valueBoxUI(ns("total_holdings"), "Total Holdings", national_data$value[national_data$variable == "Holdings"], "units"),
             valueBoxUI(ns("total_application_rate"), "Total Application Rate", national_data$value[national_data$variable == "Application rate"], "units"),
             valueBoxUI(ns("average_mixed_sward"), "Average Mixed Sward per Holding", national_data$value[national_data$variable == "Average mixed sward area per holding"], "ha"),
             valueBoxUI(ns("average_grassland"), "Average Grassland Area per Holding", national_data$value[national_data$variable == "Average grassland area per holding"], "ha")
           )
    ),
    column(width = 6,
           tabsetPanel(
             id = ns("tabs"),
             tabPanel("Map", mapRegionsUI(ns("map")), value = "map"),
             tabPanel("Data Table", DTOutput(ns("data_table")), downloadButton(ns("downloadData"), "Download Data"), value = "data_table")
           )
    )
  )
}

manureServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$sidebar_ui <- renderUI({
      req(input$tabs)
      if (input$tabs == "map") {
        radioButtons(ns("variable"), "Select Variable", choices = variables)
      } else if (input$tabs == "data_table") {
        radioButtons(ns("data_source"), "Choose data to show:", choices = c("Map Data"))
      }
    })
    
    mapRegionsServer(
      id = "map",
      data = reactive({
        req(input$variable)
        manure_data_long %>%
          filter(variable == input$variable)
      }),
      unit = "quantity",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="#">Source: Agricultural Data</a></div>',
      variable = reactive(input$variable),
      title = "Manure Quantity by Region"
    )
    
    output$data_table <- renderDT({
      req(input$data_source)
      if (input$data_source == "Map Data") {
        datatable(manure_qty)
      }
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("map_data", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(manure_qty, file, row.names = FALSE)
      }
    )
  })
}

# Test the module
content_demo <- function() {
  ui <- fluidPage(manureUI("manure_map_test"))
  server <- function(input, output, session) {
    manureServer("manure_map_test")
  }
  shinyApp(ui, server)
}

# Uncomment the line below to run the test
content_demo()
