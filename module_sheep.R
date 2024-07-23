# File: module_sheep.R

library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(geojsonio)
library(DT)

# Load the required module
source("module_map.R")

# Define UI for the sheep module
sheepUI <- function(id) {
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
            "Ewes for breeding" = "Ewes for breeding",
            "Other sheep one year old and over for breeding" = "Other sheep one year old and over for breeding",
            "Rams to be used for service" = "Rams for service",
            "Lambs" = "Lambs",
            "Total sheep" = "Total sheep"
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
            "Ewes used for breeding in previous season",
            "Rams to be used for service",
            "Sheep for breeding aged 1 year and over",
            "Other",
            "Total other sheep 1 year and over",
            "Lambs",
            "Total sheep"
          ),
          selected = c(
            "Ewes used for breeding in previous season",
            "Rams to be used for service",
            "Sheep for breeding aged 1 year and over",
            "Total other sheep 1 year and over",
            "Lambs"
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

# Define server logic for the sheep module
sheepServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter livestock_subregion dataset for the selected categories
    sheep_data <- livestock_subregion %>%
      filter(`Livestock by category` %in% c(
        "Ewes for breeding",
        "Other sheep one year old and over for breeding",
        "Rams for service",
        "Lambs",
        "Total sheep"
      )) %>%
      select(-Scotland) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = safe_as_numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        sheep_data %>% filter(`Livestock by category` == input$variable)
      }),
      variable = reactive(input$variable),
      title = "Sheep Distribution by Region"
    )
    
    output$timeseries <- renderHighchart({
      req(input$timeseries_variables)
      filtered_data <- number_of_sheep %>%
        filter(`Sheep by category` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Sheep by category`, names_to = "year", values_to = "value")
      
      highchart() %>%
        hc_add_series(data = filtered_data, hcaes(x = year, y = value, group = `Sheep by category`), type = "line") %>%
        hc_title(text = "Sheep Time Series Data") %>%
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "Number of Sheep")) %>%
        hc_chart(zoomType = "xy")
    })
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        req(input$variable)
        sheep_data %>%
          filter(`Livestock by category` == input$variable) %>%
          datatable()
      } else {
        req(input$timeseries_variables)
        number_of_sheep %>%
          filter(`Sheep by category` %in% input$timeseries_variables) %>%
          pivot_longer(cols = -`Sheep by category`, names_to = "year", values_to = "value") %>%
          datatable()
      }
    })
  })
}

# Testing module
sheep_demo <- function() {
  ui <- fluidPage(sheepUI("sheep_test"))
  server <- function(input, output, session) {
    sheepServer("sheep_test")
  }
  shinyApp(ui, server)
}

sheep_demo()
