library(shiny)
library(highcharter)
library(DT)
library(dplyr)
library(tidyr)
library(geojsonio)

# Load the required module
source("module_map.R")

# Define UI for the cattle module
cattleUI <- function(id) {
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
              "Total Female Dairy Cattle" = "Total Female Dairy Cattle",
              "Total Female Beef Cattle" = "Total Female Beef Cattle",
              "Total Male Cattle" = "Total Male Cattle",
              "Total Calves" = "Total Calves",
              "Total Cattle" = "Total Cattle"
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
            choices = unique(number_of_cattle$`Cattle by category`),
            selected = c(
              "Total Female Dairy Cattle",
              "Total Female Beef Cattle",
              "Total Male Cattle",
              "Total Calves",
              "Total Cattle"
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

# Define server logic for the cattle module
cattleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter livestock_subregion dataset for the selected categories
    cattle_data <- livestock_subregion %>%
      filter(`Livestock by category` %in% c(
        "Total Female Dairy Cattle",
        "Total Female Beef Cattle",
        "Total Male Cattle",
        "Total Calves",
        "Total Cattle"
      )) %>%
      select(-Scotland) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -`Livestock by category`, names_to = "sub_region", values_to = "value") %>%
      mutate(value = as.numeric(value))
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        cattle_data %>% filter(`Livestock by category` == input$variable)
      }),
      variable = reactive(input$variable),
      title = "Cattle Distribution by Region"
    )
    
    # Timeseries data processing
    timeseries_data <- number_of_cattle %>%
      pivot_longer(cols = -`Cattle by category`, names_to = "year", values_to = "value") %>%
      mutate(year = as.numeric(year))
    
    output$timeseries <- renderHighchart({
      req(input$timeseries_variable)
      
      filtered_timeseries <- timeseries_data %>%
        filter(`Cattle by category` %in% input$timeseries_variable)
      
      highchart() %>%
        hc_chart(zoomType = "xy") %>%
        hc_title(text = "Cattle Time Series") %>%
        hc_xAxis(categories = unique(filtered_timeseries$year)) %>%
        hc_add_series_list(
          lapply(input$timeseries_variable, function(variable) {
            data_series <- filtered_timeseries %>% filter(`Cattle by category` == variable)
            list(name = variable, data = data_series$value)
          })
        )
    })
    
    output$datatable <- renderDT({
      req(input$data_type)
      
      if (input$data_type == "Map Data") {
        datatable(cattle_data)
      } else if (input$data_type == "Timeseries Data") {
        datatable(timeseries_data)
      }
    })
  })
}

# Testing module
cattle_demo <- function() {
  ui <- fluidPage(cattleUI("cattle_test"))
  server <- function(input, output, session) {
    cattleServer("cattle_test")
  }
  shinyApp(ui, server)
}

cattle_demo()
