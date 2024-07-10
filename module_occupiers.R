# File: module_occupiers.R

library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(DT)
library(geojsonio)

# Load the GeoJSON file
geojson_data <- geojson_read("subregions_simplified.geojson", what = "sp")

# Convert GeoJSON to a Highcharts-compatible format
geojson_list <- geojson_list(geojson_data)

# UI Function
occupiersUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        uiOutput(ns("sidebar_ui"))
      ),
      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Map", highchartOutput(ns("map"), height = "75vh")),
          tabPanel("Bar Chart", highchartOutput(ns("pyramid_chart"), height = "500px")),
          tabPanel("Data Table", 
                   DTOutput(ns("data_table")),
                   downloadButton(ns("downloadData"), "Download Data"))
        )
      )
    )
  )
}

# Server Function
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
        pivot_longer(cols = -`Occupiers and employees by category`, names_to = "sub_region", values_to = "value") %>%
        mutate(value = ifelse(is.na(as.numeric(value)), NA, as.numeric(value))) %>%
        filter(`Occupiers and employees by category` %in% c(
          "Regular full-time staff total", 
          "Regular part-time staff total", 
          "Total Casual and seasonal staff", 
          "Total agricultural workforce"
        ))
    })
    
    # Bar Chart - Observers
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
    
    # Bar Chart - Filtered Data
    filtered_data <- reactive({
      req(input$variables)
      chart_data() %>%
        filter(Age %in% input$variables) %>%
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
    
    # Map - Filtered Data
    filtered_map_data <- reactive({
      req(input$variable)
      regions_data() %>%
        filter(`Occupiers and employees by category` == input$variable)
    })
    
    # Map - Output
    output$map <- renderHighchart({
      data <- filtered_map_data()
      hc_data <- data %>%
        mutate(sub_region = as.character(sub_region)) %>%
        select(sub_region, value) %>%
        list_parse()
      
      variable_name <- input$variable
      
      highchart(type = "map") %>%
        hc_add_series(
          mapData = geojson_list, 
          joinBy = c("sub_region", "sub_region"),
          data = hc_data,
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
            pointFormatter = JS(sprintf("function() {
              return '<b>' + this.sub_region + '</b><br/>' +
                     '%s: ' + this.value;
            }", variable_name))
          ),
          nullColor = '#E0E0E0'
        ) %>%
        hc_mapNavigation(enabled = TRUE) %>%
        hc_colorAxis(
          min = 0,
          stops = color_stops(5),
          labels = list(
            format = "{value:,.0f}"
          )
        ) %>%
        hc_title(text = "Occupiers and Employees by Region") %>%
        hc_chart(reflow = TRUE) %>%
        hc_legend(
          layout = "horizontal",
          align = "center",
          verticalAlign = "bottom",
          title = list(text = "Legend", style = list(fontSize = '15px')),
          itemStyle = list(width = '100px')
        )
    })
    
    # Data Table - Output
    render_data_table <- function(table_id, chart_data, map_data, input, output) {
      output[[table_id]] <- renderDT({
        if (input$data_source == "Chart Data") {
          datatable(chart_data())
        } else {
          datatable(map_data())
        }
      })
    }
    
    # Data Table - Download Handler
    handle_data_download <- function(download_id, chart_data, map_data, input, output) {
      output[[download_id]] <- downloadHandler(
        filename = function() {
          paste(input$data_source, Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          if (input$data_source == "Chart Data") {
            write.csv(chart_data(), file)
          } else {
            write.csv(map_data(), file)
          }
        }
      )
    }
    
    # Render Data Table
    render_data_table(
      table_id = "data_table",
      chart_data = chart_data,
      map_data = regions_data,
      input = input,
      output = output
    )
    
    # Handle Data Download
    handle_data_download(
      download_id = "downloadData",
      chart_data = chart_data,
      map_data = regions_data,
      input = input,
      output = output
    )
    
    # Dynamic UI for Sidebar
    output$sidebar_ui <- renderUI({
      if (input$tabs == "Map") {
        radioButtons(ns("variable"), "Select Variable", choices = c(
          "Regular full-time staff total", 
          "Regular part-time staff total", 
          "Total Casual and seasonal staff", 
          "Total agricultural workforce"
        ))
      } else if (input$tabs == "Data Table") {
        radioButtons(ns("data_source"), "Choose data to show:", choices = c("Chart Data", "Map Data"))
      } else {
        tagList(
          checkboxGroupInput(ns("variables"), "Choose age groups to display", choices = unique(chart_data()$Age), selected = unique(chart_data()$Age)),
          actionButton(ns("select_all_button"), "Select All"),
          actionButton(ns("deselect_all_button"), "Deselect All")
        )
      }
    })
  })
}

# Testing Module
content_demo <- function() {
  ui <- fluidPage(occupiersUI("occupiers_test"))
  server <- function(input, output, session) {
    occupiersServer("occupiers_test")
  }
  shinyApp(ui, server)
}

content_demo()
