# File: module_poultry.R

library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(geojsonio)
library(DT)

# Load the required module
source("module_map.R")

# Define UI for the poultry module
poultryUI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    id = ns("tabs"),
    tabPanel(
      "Map",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          radioButtons(
            ns("variable"), 
            "Select Variable", 
            choices = c(
              "Fowls for producing eggs" = "Fowls for producing eggs",
              "Fowls for breeding" = "Fowls for breeding",
              "Broilers for other table fowls and other poultry" = "Broilers for other table fowls and other poultry",
              "Total poultry" = "Total poultry"
            )
          )
        ),
        mainPanel(
          width = 9,
          mapUI(ns("map"))
        )
      )
    ),
    tabPanel(
      "Timeseries",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectizeInput(
            ns("timeseries_variable"),
            "Select Variable",
            choices = unique(number_of_poultry$`Poultry by category`),
            selected = c(
              "Total fowls for producing eggs",
              "Total fowls for breeding",
              "Broilers and other table birds",
              "Total Poultry"
            ),
            multiple = TRUE
          )
        ),
        mainPanel(
          width = 9,
          highchartOutput(ns("timeseries"), height = "75vh")
        )
      )
    ),
    tabPanel(
      "Data Table",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          radioButtons(
            ns("data_type"), 
            "Select Data Type", 
            choices = c("Map Data", "Timeseries Data")
          )
        ),
        mainPanel(
          width = 9,
          DTOutput(ns("datatable"))
        )
      )
    )
  )
}

# Define server logic for the poultry module
poultryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter livestock_subregion dataset for the selected categories
    poultry_data <- livestock_subregion %>%
      filter(`Livestock by category` %in% c(
        "Fowls for producing eggs",
        "Fowls for breeding",
        "Broilers for other table fowls and other poultry",
        "Total poultry"
      )) %>%
      select(-Scotland) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = safe_as_numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        poultry_data %>% filter(`Livestock by category` == input$variable)
      }),
      variable = reactive(input$variable),
      title = "Poultry Distribution by Region"
    )
    
    # Timeseries data processing
    timeseries_data <- number_of_poultry %>%
      pivot_longer(cols = -`Poultry by category`, names_to = "year", values_to = "value") %>%
      mutate(year = as.numeric(year))
    
    output$timeseries <- renderHighchart({
      req(input$timeseries_variable)
      
      filtered_timeseries <- timeseries_data %>%
        filter(`Poultry by category` %in% input$timeseries_variable)
      
      highchart() %>%
        hc_chart(zoomType = "xy") %>%
        hc_title(text = "Poultry Time Series") %>%
        hc_xAxis(categories = unique(filtered_timeseries$year)) %>%
        hc_add_series_list(
          lapply(input$timeseries_variable, function(variable) {
            data_series <- filtered_timeseries %>% filter(`Poultry by category` == variable)
            list(name = variable, data = data_series$value)
          })
        )
    })
    
    output$datatable <- renderDT({
      req(input$data_type)
      
      if (input$data_type == "Map Data") {
        datatable(poultry_data)
      } else if (input$data_type == "Timeseries Data") {
        datatable(timeseries_data)
      }
    })
  })
}

# Testing module
poultry_demo <- function() {
  ui <- fluidPage(poultryUI("poultry_test"))
  server <- function(input, output, session) {
    poultryServer("poultry_test")
  }
  shinyApp(ui, server)
}

poultry_demo()
