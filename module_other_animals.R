# File: module_other_animals.R

library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(geojsonio)
library(DT)

# Load the required module
source("module_map.R")

# Define UI for the other animals module
otherAnimalsUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        condition = "input.tabsetPanel === 'Map'",
        ns = ns,
        radioButtons(
          ns("variable"), 
          "Select Variable", 
          choices = c(
            "Goats and kids" = "Goats and kids",
            "Deer" = "Deer",
            "Horses" = "Horses",
            "Donkeys" = "Donkeys",
            "Camelids" = "Camelids",
            "Beehives" = "Beehives"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = c(
            "Goats",
            "Deer",
            "Horses",
            "Donkeys",
            "Camelids",
            "Beehives"
          ),
          selected = c(
            "Goats",
            "Deer",
            "Horses",
            "Donkeys",
            "Camelids",
            "Beehives"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Data Table'",
        ns = ns,
        radioButtons(
          ns("table_data"),
          "Select Data to Display",
          choices = c("Map Data" = "map", "Time Series Data" = "timeseries"),
          selected = "map"
        )
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabsetPanel"),
        tabPanel("Map", mapUI(ns("map"))),
        tabPanel("Time Series", highchartOutput(ns("timeseries"))),
        tabPanel("Data Table", DTOutput(ns("table")))
      )
    )
  )
}

# Define server logic for the other animals module
otherAnimalsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter livestock_subregion dataset for the selected categories
    other_animals_data <- livestock_subregion %>%
      filter(`Livestock by category` %in% c(
        "Goats and kids",
        "Deer",
        "Horses",
        "Donkeys",
        "Camelids",
        "Beehives"
      )) %>%
      select(-Scotland) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        other_animals_data %>% filter(`Livestock by category` == input$variable)
      }),
      variable = reactive(input$variable),
      title = "Other Animals Distribution by Region"
    )
    
    output$timeseries <- renderHighchart({
      req(input$timeseries_variables)
      filtered_data <- number_of_other_livestock %>%
        mutate(across(-`Livestock by category`, as.numeric)) %>%
        pivot_longer(cols = -`Livestock by category`, names_to = "year", values_to = "value")
      
      highchart() %>%
        hc_add_series(data = filtered_data, hcaes(x = year, y = value, group = `Livestock by category`), type = "line") %>%
        hc_title(text = "Other Animals Time Series Data") %>%
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "Number of Animals")) %>%
        hc_chart(zoomType = "xy")
    })
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        datatable(other_animals_data)
      } else {
        req(input$timeseries_variables)
        number_of_other_livestock %>%
          filter(`Livestock by category` %in% input$timeseries_variables) %>%
          mutate(across(-`Livestock by category`, as.numeric)) %>%
          pivot_longer(cols = -`Livestock by category`, names_to = "year", values_to = "value") %>%
          datatable()
      }
    })
  })
}

# Testing module
other_animals_demo <- function() {
  ui <- fluidPage(otherAnimalsUI("other_animals_test"))
  server <- function(input, output, session) {
    otherAnimalsServer("other_animals_test")
  }
  shinyApp(ui, server)
}

other_animals_demo()
