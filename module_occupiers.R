## File: module_occupiers.R

library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(DT)
library(geojsonio)

# Load the module_map.R
source("module_map.R")

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
        tabPanel("Bar Chart", highchartOutput(ns("pyramid_chart"), height = "500px"), value = "bar_chart"),
        tabPanel("Timeseries", 
                 highchartOutput(ns("line_chart")), 
                 div(
                   class = "note",
                   style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
                   HTML(
                     "<strong>Note:</strong><ul>
                       <li>To add or remove a series from the chart, select/deselect the variable from the sidebar menu.</li>
                       <li>Select a year range by adjusting the slider on the sidebar or by zooming into the graph by clicking and dragging over an area you wish to see.</li>
                       <li>You can see data values for a specific year by hovering your mouse over the line.</li>
                     </ul>"
                   )
                 ), 
                 value = "timeseries"),
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
      mutate(across(starts_with("20"), as.numeric))
    
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
        tagList(
          checkboxGroupInput(
            ns("variables"), 
            "Select variables to display", 
            choices = unique(occupiers_timeseries_data()$`Occupiers and employees by category`), 
            selected = c('Occupiers - full time', 'Total working occupiers', 'Occupiers not working on the holding')
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
      } else {
        tagList(
          checkboxGroupInput(ns("variables_bar"), "Choose age groups to display", choices = unique(chart_data()$Age), selected = unique(chart_data()$Age)),
          actionButton(ns("select_all_button"), "Select All"),
          actionButton(ns("deselect_all_button"), "Deselect All")
        )
      }
    })
    
    filtered_timeseries_data <- reactive({
      req(input$variables, input$year_range)
      data <- occupiers_timeseries_data()
      data %>% 
        filter(
          `Occupiers and employees by category` %in% input$variables,
          Year >= input$year_range[1] & Year <= input$year_range[2]
        )
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
          labels = list(formatter = JS("function () {
            return Math.abs(this.value);
          }"))
        ) %>%
        hc_plotOptions(bar = list(stacking = "normal")) %>%
        hc_add_series(name = "Female", data = as.list(female_data), color = "#002d54", tooltip = list(pointFormatter = JS("function() { return 'Female: ' + Math.abs(this.y); }"))) %>%
        hc_add_series(name = "Male", data = as.list(male_data), color = "#2b9c93", tooltip = list(pointFormatter = JS("function() { return 'Male: ' + this.y; }"))) %>%
        hc_tooltip(shared = FALSE) %>%
        hc_title(text = "Labour Interactive Plots") %>%
        hc_subtitle(text = "Population pyramid") %>%
        hc_legend(align = "center") %>%
        hc_xAxis(title = list(text = "Age group")) %>%
        hc_yAxis(title = list(text = "Number of working occupiers"),
                 labels = list(formatter = JS("function () {
                   return Math.abs(this.value);
                 }")))
    })
    
    # Timeseries Chart - Output
    output$line_chart <- renderHighchart({
      data <- filtered_timeseries_data()
      if (nrow(data) == 0) return(NULL)
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_title(text = "Agricultural Occupiers Timeseries") %>%
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "Occupiers (1,000)")) %>%
        hc_add_series(data = data, hcaes(x = Year, y = Value, group = `Occupiers and employees by category`), type = "line") %>%
        hc_tooltip(shared = FALSE)
    })
    
    # Map - Use the new map module
    mapServer(
      id = "map",
      data = regions_data,
      variable = reactive(input$variable),
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
    
    # Select/Deselect all for bar chart
    observeEvent(input$select_all_button, {
      updateCheckboxGroupInput(session, ns("variables_bar"), selected = unique(chart_data()$Age))
    })
    
    observeEvent(input$deselect_all_button, {
      updateCheckboxGroupInput(session, ns("variables_bar"), selected = character(0))
    })
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
