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
  filter(Region != "All respondents") %>%
  pivot_longer(cols = -Region, names_to = "variable", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  rename(region = Region)

# Extract the data for "All respondents"
national_data <- manure_qty %>%
  filter(Region == "All respondents") %>%
  select(-Region) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(value = as.numeric(value)) # Ensure numeric values

# List of variables for radio buttons
variables_manure <- colnames(manure_qty)[-1]

source("module_regions_map.R")

# Helper function to format numbers with commas and appropriate decimal places
format_number <- function(number) {
  if (is.na(number)) {
    return("N/A")
  } else if (number %% 1 == 0) {
    format(number, big.mark = ",", scientific = FALSE)
  } else {
    format(round(number, 2), big.mark = ",", scientific = FALSE, nsmall = 2)
  }
}

valueBoxManureUI <- function(id, title, value, unit) {
  numeric_value <- as.numeric(value) # Ensure the value is numeric
  formatted_value <- format_number(numeric_value)
  
  
  box(
    class = "value-box",
    title = NULL,
    width = 12,
    solidHeader = TRUE,
    div(
      style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100%; padding: 5px;",  # Center content vertically and horizontally
      div(
        style = "font-size: 14px; font-weight: bold; margin-bottom: 15px; text-align: center;",  # Adjusted title style and added margin for vertical gap
        h5(class = "value-box-title", title)
      ),
      div(
        style = "display: flex; align-items: center; justify-content: center;",  # Center the value and unit
        h3(HTML(formatted_value), style = "font-size: 24px; font-weight: bold; margin: 0;"),  # Main value
        span(class = "value-box-units", unit, style = "font-size: 24px; font-weight: normal; margin-left: 5px;")
      )
    ),
    style = "border: 1px solid white; background-color: transparent; box-shadow: none; display: flex; align-items: center; justify-content: center;"  # Center the entire content of the box
  )
  
  
}


manureUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      uiOutput(ns("sidebar_ui"))
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabs"),
        tabPanel("Summary",
                 fluidRow(
                   column(width = 4,
                          valueBoxManureUI(ns("total_manure"), "Total Manure Applied", 
                                           national_data$value[national_data$variable == "Manure"], "tons"),
                          p(style = "color: white;", "/"),
                          valueBoxManureUI(ns("total_holdings"), "Total Holdings", 
                                           national_data$value[national_data$variable == "Holdings"], "units"),
                          p(style = "color: white;", "/"),
                          valueBoxManureUI(ns("total_application_rate"), "Total Application Rate", 
                                           national_data$value[national_data$variable == "Application rate"], "units"),
                          p(style = "color: white;", "/"),
                          valueBoxManureUI(ns("average_mixed_sward"), "Average Mixed Sward per Holding", 
                                           national_data$value[national_data$variable == "Average mixed sward area per holding"], "ha"),
                          p(style = "color: white;", "/"),
                          valueBoxManureUI(ns("average_grassland"), "Average Grassland Area per Holding", 
                                           national_data$value[national_data$variable == "Average grassland area per holding"], "ha")
                   ),
                   column(width = 8,
                          mapRegionsUI(ns("map"))
                   )
                 ),
                 value = "map"
        ),
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
        radioButtons(ns("variable"), "Select Variable", choices = variables_manure)
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
# content_demo()
