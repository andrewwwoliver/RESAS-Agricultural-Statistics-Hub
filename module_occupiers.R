# File: module_occupiers.R

library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(DT)
library(geojsonio)

# Load the GeoJSON file
geojson_data <- geojson_read("subregions_simplified.geojson", what = "sp")
geojson_list <- geojson_list(geojson_data)

occupiersUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        checkboxGroupInput(ns("variables"), "Choose age groups to display", choices = NULL, selected = NULL),
        actionButton(ns("select_all_button"), "Select All"),
        actionButton(ns("deselect_all_button"), "Deselect All")
      ),
      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Occupiers", highchartOutput(ns("pyramid_chart"), height = "500px")),
          tabPanel("Map", highchartOutput(ns("map"), height = "500px")),  # Added new tab
          tabPanel("Data Table", 
                   DTOutput(ns("data_table")),
                   downloadButton(ns("downloadData"), "Download Data"))
        )
      )
    )
  )
}

occupiersServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Process the data to focus on "Total working occupiers"
    chart_data <- reactive({
      data <- occupiers_age_gender %>%
        filter(`Occupier working time` == "Total working occupiers") %>%
        select(-`Male Total`, -`Female Total`) %>%
        pivot_longer(cols = -`Occupier working time`, names_to = "Gender_Age", values_to = "Count") %>%
        separate(Gender_Age, into = c("Gender", "Age"), sep = " ", extra = "merge") %>%
        mutate(Count = as.numeric(Count))
      
      data
    })
    
    observe({
      choices <- unique(chart_data()$Age)
      updateCheckboxGroupInput(session, "variables", choices = choices, selected = choices)
    })
    
    observeEvent(input$select_all_button, {
      updateCheckboxGroupInput(session, "variables", selected = unique(chart_data()$Age))
    })
    
    observeEvent(input$deselect_all_button, {
      updateCheckboxGroupInput(session, "variables", selected = character(0))
    })
    
    filtered_data <- reactive({
      req(input$variables)
      data <- chart_data() %>%
        filter(Age %in% input$variables) %>%
        mutate(Count = ifelse(Gender == "Female", -Count, Count))
      data
    })
    
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
        hc_subtitle(text = "Occupiers") %>%
        hc_legend(align = "center") %>%
        hc_xAxis(title = list(text = "Age group")) %>%
        hc_yAxis(title = list(text = "Number of working occupiers"),
                 labels = list(formatter = JS("function () {
                   return Math.abs(this.value);
                 }")))
    })
    
    render_data_table(
      table_id = "data_table",
      chart_data = chart_data,
      output = output
    )
    
    handle_data_download(
      download_id = "downloadData",
      chart_type = "Occupiers Age and Gender",
      chart_data = chart_data,
      input = input,
      output = output,
      year_input = NULL
    )
    
    # Map logic
    map_data <- reactive({
      occupiers_employees_subregion %>%
        filter(`Occupiers and employees by category` %in% c("Total working occupiers", "Occupiers not working on the holding"))
    })
    
    output$map <- renderHighchart({
      data <- map_data()
      
      highchart(type = "map") %>%
        hc_add_series(
          mapData = geojson_list, 
          joinBy = c("sub_region", "sub_region"),
          data = data,
          borderColor = "#FFFFFF",
          borderWidth = 0.5,
          states = list(
            hover = list(
              color = "#BADA55"
            )
          ),
          dataLabels = list(
            enabled = FALSE
          ),
          tooltip = list(
            useHTML = TRUE,
            headerFormat = "<b>{point.key}</b><br/>",
            pointFormat = "Value: {point.value}"
          ),
          nullColor = '#E0E0E0'
        ) %>%
        hc_mapNavigation(enabled = TRUE) %>%
        hc_colorAxis(
          min = 0,
          stops = color_stops(5)
        ) %>%
        hc_title(text = "Total Working Occupiers and Occupiers Not Working on the Holding") %>%
        hc_chart(reflow = TRUE) %>%
        hc_legend(
          layout = "horizontal",
          align = "center",
          verticalAlign = "bottom",
          title = list(text = "Legend", style = list(fontSize = '15px')),
          itemStyle = list(width = '100px')
        )
    })
  })
}

# Testing module
content_demo <- function() {
  ui <- fluidPage(occupiersUI("occupiers_test"))
  server <- function(input, output, session) {
    occupiersServer("occupiers_test")
  }
  shinyApp(ui, server)
}

content_demo()
