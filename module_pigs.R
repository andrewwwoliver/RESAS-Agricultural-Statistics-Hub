# File: module_pigs.R

library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(geojsonio)
library(DT)

# Load the required module
source("module_map.R")

# Define UI for the pigs module
pigsUI <- function(id) {
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
              "Female pigs breeding herd" = "Female pigs breeding herd",
              "All other non-breeding pigs" = "All other non-breeding pigs",
              "Total pigs" = "Total pigs"
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
            choices = unique(number_of_pigs$`Pigs by category`),
            selected = c(
              "Total breeding herd",
              "80 kg liveweight and over",
              "50 kg and under 80 kg liveweight",
              "Under 50 kg liveweight"
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

# Define server logic for the pigs module
pigsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter livestock_subregion dataset for the selected categories
    pigs_data <- livestock_subregion %>%
      filter(`Livestock by category` %in% c(
        "Female pigs breeding herd",
        "All other non-breeding pigs",
        "Total pigs"
      )) %>%
      select(-Scotland) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        pigs_data %>% filter(`Livestock by category` == input$variable)
      }),
      variable = reactive(input$variable),
      title = "Pigs Distribution by Region"
    )
    
    # Timeseries data processing
    timeseries_data <- number_of_pigs %>%
      pivot_longer(cols = -`Pigs by category`, names_to = "year", values_to = "value") %>%
      mutate(year = as.numeric(year))
    
    output$timeseries <- renderHighchart({
      req(input$timeseries_variable)
      
      filtered_timeseries <- timeseries_data %>%
        filter(`Pigs by category` %in% input$timeseries_variable)
      
      highchart() %>%
        hc_chart(zoomType = "xy") %>%
        hc_title(text = "Pigs Time Series") %>%
        hc_xAxis(categories = unique(filtered_timeseries$year)) %>%
        hc_add_series_list(
          lapply(input$timeseries_variable, function(variable) {
            data_series <- filtered_timeseries %>% filter(`Pigs by category` == variable)
            list(name = variable, data = data_series$value)
          })
        )
    })
    
    output$datatable <- renderDT({
      req(input$data_type)
      
      if (input$data_type == "Map Data") {
        datatable(pigs_data)
      } else if (input$data_type == "Timeseries Data") {
        datatable(timeseries_data)
      }
    })
  })
}

# Testing module
pigs_demo <- function() {
  ui <- fluidPage(pigsUI("pigs_test"))
  server <- function(input, output, session) {
    pigsServer("pigs_test")
  }
  shinyApp(ui, server)
}

pigs_demo()
