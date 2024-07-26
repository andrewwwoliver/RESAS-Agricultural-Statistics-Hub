# File: module_sheep.R

library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(geojsonio)
library(DT)

# Load the required module
source("module_map.R")
source("module_area_chart.R")
source("module_line_chart.R")

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
            "Total sheep" = "Total sheep",
            "Ewes for breeding" = "Ewes for breeding",
            "Other sheep one year old and over for breeding" = "Other sheep one year old and over for breeding",
            "Rams to be used for service" = "Rams for service",
            "Lambs" = "Lambs"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabsetPanel === 'Time Series' || input.tabsetPanel === 'Area Chart'",
        ns = ns,
        checkboxGroupInput(
          ns("timeseries_variables"),
          "Select Time Series Variables",
          choices = c(
            "Ewes used for breeding in previous season",
            "Sheep for breeding aged 1 year and over",
            "Rams to be used for service",
            "Total other sheep 1 year and over",
            "Lambs",
            "Total sheep",
            "Other"
            
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
        tabPanel("Time Series", lineChartUI(ns("line"))),
        tabPanel("Area Chart", areaChartUI(ns("area"))),
        tabPanel("Data Table", DTOutput(ns("table")))
      )
    )
  )
}

sheepServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      variable = reactive(input$variable),
      title = "Sheep Distribution by Region"
    )
    
    chart_data <- reactive({
      req(input$timeseries_variables)
      filtered_data <- number_of_sheep %>%
        filter(`Sheep by category` %in% input$timeseries_variables) %>%
        pivot_longer(cols = -`Sheep by category`, names_to = "year", values_to = "value") %>%
        mutate(year = as.numeric(year))  # Ensure year is numeric
      filtered_data
    })
    
    areaChartServer(
      id = "area",
      chart_data = chart_data,
      title = "Sheep Area Chart Data",
      yAxisTitle = "Number of Sheep",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    lineChartServer(
      id = "line",
      chart_data = chart_data,
      title = "Sheep Area Chart Data",
      yAxisTitle = "Number of Sheep",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "year",
      y_col = "value"
    )
    
    output$table <- renderDT({
      req(input$tabsetPanel == "Data Table")
      if (input$table_data == "map") {
        req(input$variable)
        sheep_data %>%
          filter(`Livestock by category` == input$variable) %>%
          datatable()
      } else {
        number_of_sheep %>%
          pivot_longer(cols = -`Sheep by category`, names_to = "year", values_to = "value") %>%
          datatable()
      }
    })
  })
}

sheep_demo <- function() {
  ui <- fluidPage(sheepUI("sheep_test"))
  server <- function(input, output, session) {
    sheepServer("sheep_test")
  }
  shinyApp(ui, server)
}

sheep_demo()
