# File: module_employees.R

library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)

# Load the module_map.R
source("module_map.R")
# Load the existing module_data_table.R
source("module_data_table.R")

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

employeesMapUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons(ns("variable"), "Select Variable", choices = categories)
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabs"),
        tabPanel("Map", mapUI(ns("map"))),
        tabPanel("Data Table", DTOutput(ns("data_table")), downloadButton(ns("downloadData"), "Download Data"))
      )
    )
  )
}

employeesMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    filtered_data <- reactive({
      filtered_regions_data %>%
        filter(`Occupiers and employees by category` == input$variable)
    })
    
    mapServer(
      id = "map",
      data = filtered_data,
      variable = reactive(input$variable),
      title = "Agricultural Employees by Region"
    )
    
    output$data_table <- renderDT({
      datatable(filtered_regions_data)
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("employees_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filtered_regions_data, file, row.names = FALSE)
      }
    )
  })
}

# Testing module
content_demo <- function() {
  ui <- fluidPage(employeesMapUI("employees_map_test"))
  server <- function(input, output, session) {
    employeesMapServer("employees_map_test")
  }
  shinyApp(ui, server)
}

# content_demo()
