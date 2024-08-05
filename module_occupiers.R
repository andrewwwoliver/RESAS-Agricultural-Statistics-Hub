## File: module_occupiers.R

library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(DT)
library(geojsonio)

# Load the required modules
source("module_map.R")
source("module_line_chart.R")

occupiersUI <- function(id) {
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
        tabPanel("Map", mapUI(ns("map")), value = "map"),
        tabPanel("Population Pyramid", highchartOutput(ns("pyramid_chart"), height = "500px"), value = "bar_chart"),
        tabPanel("Time Series", lineChartUI(ns("line_chart")), value = "timeseries"),
        tabPanel("Data Table", 
                 DTOutput(ns("data_table")),
                 downloadButton(ns("downloadData"), "Download Data"),
                 value = "data_table")
      )
    )
  )
}
occupiersServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Data Processing for Bar Chart
    chart_data <- reactive({
      occupiers_age_gender %>%
        filter(`Occupier working time` == "Total working occupiers") %>%
        select(-`Male Total`, -`Female Total`) %>%
        pivot_longer(cols = -`Occupier working time`, names_to = "Gender_Age", values_to = "Count") %>%
        separate(Gender_Age, into = c("Gender", "Age"), sep = " ", extra = "merge") %>%
        mutate(Count = as.numeric(Count))
    })
    
    # Data Processing for Map
    regions_data <- reactive({
      occupiers_employees_subregion %>%
        select(-Scotland) %>%
        mutate(across(everything(), as.character)) %>%  # Ensure all columns are characters
        pivot_longer(cols = -`Occupiers and employees by category`, names_to = "sub_region", values_to = "value") %>%
        mutate(value = ifelse(is.na(as.numeric(value)), NA, as.numeric(value))) %>%
        filter(`Occupiers and employees by category` %in% c(
          "Total working Occupiers", 
          "Occupiers not working on the holding"
        ))
    })
    
    # Data Processing for Timeseries
    occupiers_employees <- occupiers_employees %>%
      mutate(across(starts_with("20"), safe_as_numeric))
    
    occupiers_timeseries_data <- reactive({
      occupiers_employees %>%
        pivot_longer(cols = -`Occupiers and employees by category`, names_to = "Year", values_to = "Value") %>%
        mutate(Year = as.numeric(Year)) %>%
        filter(grepl("occupiers", `Occupiers and employees by category`, ignore.case = TRUE))
    })
    
    # Sidebar UI
    output$sidebar_ui <- renderUI({
      req(input$tabs)
      if (input$tabs == "map") {
        radioButtons(ns("variable"), "Select Variable", choices = c(
          "Total working Occupiers", 
          "Occupiers not working on the holding"
        ))
      } else if (input$tabs == "data_table") {
        radioButtons(ns("data_source"), "Choose data to show:", choices = c("Chart Data", "Map Data", "Timeseries Data"))
      } else if (input$tabs == "timeseries") {
        checkboxGroupInput(
          ns("variables"), 
          "Choose variables to add to chart", 
          choices = unique(occupiers_timeseries_data()$`Occupiers and employees by category`), 
          selected = c('Occupiers - full time', 'Total working occupiers', 'Occupiers not working on the holding')
        )
      } else {
        checkboxGroupInput(ns("variables_bar"), "Choose age groups to display", choices = unique(chart_data()$Age), selected = unique(chart_data()$Age))
      }
    })
    
    # Bar Chart - Filtered Data
    filtered_data <- reactive({
      req(input$variables_bar)
      chart_data() %>%
        filter(Age %in% input$variables_bar) %>%
        mutate(Count = ifelse(Gender == "Female", -Count, Count))
    })
    
    # Bar Chart - Output
    output$pyramid_chart <- renderHighchart({
      data <- filtered_data()
      female_data <- data %>% filter(Gender == "Female") %>% select(y = Count) %>% pull()
      male_data <- data %>% filter(Gender == "Male") %>% select(y = Count) %>% pull()
      categories <- data %>% filter(Gender == "Male") %>% select(Age) %>% pull()
      max_count <- max(abs(c(male_data, female_data)), na.rm = TRUE)
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_xAxis(categories = categories, reversed = FALSE) %>%
        hc_yAxis(
          min = -max_count,
          max = max_count,
          title = list(text = "Number of working occupiers"), 
          labels = list(
            formatter = JS("function () {
          return Math.abs(this.value).toLocaleString();
        }")
          )
        ) %>%
        hc_plotOptions(bar = list(stacking = "normal")) %>%
        hc_add_series(name = "Female", data = as.list(female_data), color = "#002d54", tooltip = list(pointFormatter = JS("function() { return 'Female: ' + Math.abs(this.y).toLocaleString() + ' occupiers'; }"))) %>%
        hc_add_series(name = "Male", data = as.list(male_data), color = "#2b9c93", tooltip = list(pointFormatter = JS("function() { return 'Male: ' + this.y.toLocaleString() + ' occupiers'; }"))) %>%
        hc_tooltip(shared = FALSE) %>%
        hc_title(text = "Breakdown of Occupiers by Age and Gender", align = "left", style = list(fontSize = "20px", fontWeight = "bold")) %>%
        hc_legend(align = "center") %>%
        hc_xAxis(title = list(text = "Age group")) %>%
        hc_yAxis(title = list(text = "Number of working occupiers"),
                 labels = list(formatter = JS("function () {
               return Math.abs(this.value).toLocaleString();
             }")))
    })
    
    
    # Timeseries Chart - Using the modular line chart
    filtered_timeseries_data <- reactive({
      req(input$variables)
      data <- occupiers_timeseries_data()
      data %>% filter(`Occupiers and employees by category` %in% input$variables)
    })
    
    lineChartServer(
      id = "line_chart",
      chart_data = filtered_timeseries_data,
      title = "Agricultural Occupiers Time Series",
      yAxisTitle = "Occupiers (1,000)",
      xAxisTitle = "Year",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "Year",
      y_col = "Value"
    )
    
    # Map - Use the new map module
    mapServer(
      id = "map",
      data = regions_data,
      variable = reactive(input$variable),
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      title = "Occupiers by Region"
    )
    
    # Data Table - Output
    output$data_table <- renderDT({
      req(input$data_source)
      if (input$data_source == "Chart Data") {
        datatable(chart_data())
      } else if (input$data_source == "Map Data") {
        datatable(regions_data())
      } else {
        datatable(filtered_timeseries_data())
      }
    })
    
    # Data Table - Download Handler
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$data_source, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        if (input$data_source == "Chart Data") {
          write.csv(chart_data(), file, row.names = FALSE)
        } else if (input$data_source == "Map Data") {
          write.csv(regions_data(), file, row.names = FALSE)
        } else {
          write.csv(filtered_timeseries_data(), file, row.names = FALSE)
        }
      }
    )
  })
}

# Testing module
content_demo <- function() {
  ui <- fluidPage(occupiersUI("occupiers_map_test"))
  server <- function(input, output, session) {
    occupiersServer("occupiers_map_test")
  }
  shinyApp(ui, server)
}

content_demo()
