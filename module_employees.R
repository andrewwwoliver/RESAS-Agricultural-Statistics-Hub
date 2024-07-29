## File: module_employees.R

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
  mutate(value = ifelse(value == "c", NA, safe_as_numeric(value)))

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
      uiOutput(ns("sidebar_ui"))
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("tabs"),
        tabPanel("Map", mapUI(ns("map")), value = "map"),
        tabPanel("Timeseries", 
                 highchartOutput(ns("line_chart")), 
                 div(
                   class = "note",
                   style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
                   HTML(
                     "<strong>Note:</strong><ul>
                       <li>To add a series to the chart, click inside the white box on the sidebar and select a variable.</li>
                       <li>To remove a series, click the x beside the variable name within the sidebar.</li>
                       <li>Select a year range by adjusting the slider on the sidebar or by zooming into the graph by clicking and dragging over an area you wish to see.</li>
                       <li>You can see data values for a specific year by hovering your mouse over the line.</li>
                     </ul>"
                   )
                 ),
                 value = "timeseries"),
        tabPanel("Data Table", DTOutput(ns("data_table")), downloadButton(ns("downloadData"), "Download Data"), value = "data_table")
      )
    )
  )
}

employeesMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Data Processing for Timeseries
    occupiers_employees <- occupiers_employees %>%
      mutate(across(starts_with("20"), safe_as_numeric))
    
    chart_data <- reactive({
      occupiers_employees %>%
        pivot_longer(cols = -`Occupiers and employees by category`, names_to = "Year", values_to = "Value") %>%
        mutate(Year = as.numeric(Year)) %>%
        filter(!grepl("occupiers", `Occupiers and employees by category`, ignore.case = TRUE))
    })
    
    output$sidebar_ui <- renderUI({
      req(input$tabs)
      if (input$tabs == "map") {
        radioButtons(ns("variable"), "Select Variable", choices = categories)
      } else if (input$tabs == "data_table") {
        radioButtons(ns("data_source"), "Choose data to show:", choices = c("Chart Data", "Map Data"))
      } else if (input$tabs == "timeseries") {
        tagList(
          selectizeInput(
            ns("variables"), 
            "Click within the box to add more variables", 
            choices = unique(chart_data()$`Occupiers and employees by category`), 
            selected = c('Regular full-time staff total', 'Regular part-time staff total', 'Total Casual and seasonal staff'), 
            multiple = TRUE, 
            options = list(plugins = list('remove_button'), placeholder = "Click to add more variables")
          ),
          sliderInput(
            ns("year_range"),
            "Select Year Range",
            min = 2012,
            max = 2023,
            value = c(2012, 2023),
            step = 1,
            sep = "",
            ticks = TRUE
          )
        )
      }
    })
    
    filtered_chart_data <- reactive({
      req(input$variables, input$year_range)
      data <- chart_data()
      data %>% 
        filter(
          `Occupiers and employees by category` %in% input$variables,
          Year >= input$year_range[1] & Year <= input$year_range[2]
        )
    })
    
    output$line_chart <- renderHighchart({
      data <- filtered_chart_data()
      if (nrow(data) == 0) return(NULL)
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_title(text = "Agricultural Employees Timeseries") %>%
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "Employees (1,000)")) %>%
        hc_add_series(data = data, hcaes(x = Year, y = Value, group = `Occupiers and employees by category`), type = "line") %>%
        hc_tooltip(shared = FALSE)
    })
    
    mapServer(
      id = "map",
      data = reactive({
        req(input$variable)
        filtered_regions_data %>%
          filter(`Occupiers and employees by category` == input$variable)
      }),
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      variable = reactive(input$variable),
      title = "Agricultural Employees by Region"
    )
    
    output$data_table <- renderDT({
      req(input$data_source)
      if (input$data_source == "Chart Data") {
        datatable(chart_data())
      } else {
        datatable(filtered_regions_data)
      }
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$data_source, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        if (input$data_source == "Chart Data") {
          write.csv(chart_data(), file, row.names = FALSE)
        } else {
          write.csv(filtered_regions_data(), file, row.names = FALSE)
        }
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

content_demo()
