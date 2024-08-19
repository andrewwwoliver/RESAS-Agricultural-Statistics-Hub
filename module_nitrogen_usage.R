# File: module_nitrogen_usage.R

library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(DT)
library(shinydashboard)

# Load the data
load("module_2023.RData")  # Assuming nitrogen_400 and nitrogen_250 are loaded from this file

# Transform the data, filtering out "All respondents"
nitrogen_400_long <-  nitrogen_400 %>%
  filter(Region != "All respondents") %>%
  pivot_longer(cols = -Region, names_to = "variable", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  rename(region = Region)


nitrogen_250_long <-  nitrogen_250 %>%
  filter(Region != "All respondents") %>%
  pivot_longer(cols = -Region, names_to = "variable", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  rename(region = Region)


# Ensure that national_data_400 and national_data_250 are data frames
national_data_400 <- as.data.frame(
  nitrogen_400 %>%
    filter(Region == "All respondents") %>%
    select(-Region) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
)

national_data_250 <- as.data.frame(
  nitrogen_250 %>%
    filter(Region == "All respondents") %>%
    select(-Region) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
)

# Now use these data frames directly in your UI function


# List of variables for radio buttons
variables_nitrogen <- colnames(nitrogen_400)[-1]

source("module_regions_map.R")




valueBoxNitrogenUI <- function(id, title, value, unit) {
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
        style = "text-align: center;",  # Center the value and unit together
        h3(HTML(formatted_value), style = "font-size: 24px; font-weight: bold; margin: 0;"),  # Main value centered
        div(style = "font-size: 16px; font-weight: normal; margin-top: 5px;", unit)  # Unit placed below the value, also centered
      )
    ),
    style = "border: 1px solid white; background-color: transparent; box-shadow: none; display: flex; align-items: center; justify-content: center;"  # Center the entire content of the box
  )
}

nitrogenUI <- function(id) {
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
        tabPanel("Nitrogen Application (400 kg/Ha cap)",
                 fluidRow(
                   column(width = 4,
                          valueBoxNitrogenUI(ns("total_nitrogen_400"), "Total Nitrogen Applied", 
                                             national_data_400[national_data_400$variable == "Total nitrogen", "value"], "kg"),
                          p(style = "color: white;", "/"),
                          valueBoxNitrogenUI(ns("total_holdings_400"), "Total Holdings", 
                                             national_data_400[national_data_400$variable == "Holdings", "value"], "holdings"),
                          p(style = "color: white;", "/"),
                          valueBoxNitrogenUI(ns("total_application_rate_400"), "Total Application Rate", 
                                             national_data_400[national_data_400$variable == "Application rate", "value"], "kg / hectares"),
                          p(style = "color: white;", "/"),
                          valueBoxNitrogenUI(ns("average_mixed_sward_400"), "Average Mixed Sward per Holding", 
                                             national_data_400[national_data_400$variable == "Average mixed sward area per holding", "value"], "ha"),
                          p(style = "color: white;", "/"),
                          valueBoxNitrogenUI(ns("average_grassland_400"), "Average Grassland Area per Holding", 
                                             national_data_400[national_data_400$variable == "Average grassland area per holding", "value"], "ha")
                   ),
                   column(width = 8,
                          mapRegionsUI(ns("map_400"))
                   )
                 ),
                 value = "map_400"
        ),
        tabPanel("Nitrogen Application (250 kg/Ha cap)",
                 fluidRow(
                   column(width = 4,
                          valueBoxNitrogenUI(ns("total_nitrogen_250"), "Total Nitrogen Applied", 
                                             national_data_250[national_data_250$variable == "Total nitrogen", "value"], "kg"),
                          p(style = "color: white;", "/"),
                          valueBoxNitrogenUI(ns("total_holdings_250"), "Total Holdings", 
                                             national_data_250[national_data_250$variable == "Holdings", "value"], "holdings"),
                          p(style = "color: white;", "/"),
                          valueBoxNitrogenUI(ns("total_application_rate_250"), "Total Application Rate", 
                                             national_data_250[national_data_250$variable == "Application rate", "value"], "kg / hectare"),
                          p(style = "color: white;", "/"),
                          valueBoxNitrogenUI(ns("average_mixed_sward_250"), "Average Mixed Sward per Holding", 
                                             national_data_250[national_data_250$variable == "Average mixed sward area per holding", "value"], "hectare"),
                          p(style = "color: white;", "/"),
                          valueBoxNitrogenUI(ns("average_grassland_250"), "Average Grassland Area per Holding", 
                                             national_data_250[national_data_250$variable == "Average grassland area per holding", "value"], "hectare")
                   ),
                   column(width = 8,
                          mapRegionsUI(ns("map_250"))
                   )
                 ),
                 value = "map_250"
        ),
        tabPanel("Data Table", 
                 DTOutput(ns("data_table")),
                 downloadButton(ns("downloadData"), "Download Data"), 
                 value = "data_table"
        )
      )
    )
  )
}



nitrogenServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$sidebar_ui <- renderUI({
      req(input$tabs)
      if (input$tabs == "map_400") {
        radioButtons(ns("variable_400"), "Select Variable", choices = variables_nitrogen)
      } else if (input$tabs == "map_250") {
        radioButtons(ns("variable_250"), "Select Variable", choices = variables_nitrogen)
      } else if (input$tabs == "data_table") {
        radioButtons(ns("data_source"), "Choose data to show:", choices = c("400 kg/Ha cap", "250 kg/Ha cap"))
      }
    })
    
    mapRegionsServer(
      id = "map_400",
      data = reactive({
        req(input$variable_400)
        nitrogen_400_long %>%
          filter(variable == input$variable_400)
      }),
      unit = "kg / hectare",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-module-june-2023/" target="_blank">Source: Scottish Agricultural Census: Module June 2023</a></div>',
      variable = reactive(input$variable_400),
      title = "Nitrogen Usage (400 kg/Ha cap) by Region"
    )
    
    mapRegionsServer(
      id = "map_250",
      data = reactive({
        req(input$variable_250)
        nitrogen_250_long %>%
          filter(variable == input$variable_250)
      }),
      unit = "kg / hectare",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-module-june-2023/" target="_blank">Source: Scottish Agricultural Census: Module June 2023</a></div>',
      variable = reactive(input$variable_250),
      title = "Nitrogen Usage (250 kg/Ha cap) by Region"
    )
    
    output$data_table <- renderDT({
      req(input$data_source)
      if (input$data_source == "400 kg/Ha cap") {
        datatable(nitrogen_400)
      } else if (input$data_source == "250 kg/Ha cap") {
        datatable(nitrogen_250)
      }
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        if (input$data_source == "400 kg/Ha cap") {
          paste("nitrogen_data_400", Sys.Date(), ".csv", sep = "")
        } else {
          paste("nitrogen_data_250", Sys.Date(), ".csv", sep = "")
        }
      },
      content = function(file) {
        if (input$data_source == "400 kg/Ha cap") {
          write.csv(nitrogen_400, file, row.names = FALSE)
        } else {
          write.csv(nitrogen_250, file, row.names = FALSE)
        }
      }
    )
  })
}

# Test the module
content_demo <- function() {
  ui <- fluidPage(nitrogenUI("nitrogen_map_test"))
  server <- function(input, output, session) {
    nitrogenServer("nitrogen_map_test")
  }
  shinyApp(ui, server)
}

# Uncomment the line below to run the test
 content_demo()
