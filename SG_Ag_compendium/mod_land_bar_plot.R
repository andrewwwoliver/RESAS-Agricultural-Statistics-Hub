library(shiny)
library(tidyverse)
# plot module ----




barplotUI <- function(id) {
  ns <- NS(id)
  fluidRow( 
    tabsetPanel(id = "bar_plot_tab",
                tabPanel("Farm types (area)",
           highchartOutput(ns("area_plot"))),
           tabPanel("Farm types (holdings)",
                    highchartOutput(ns("holding_plot")))       
  )
  )
  
  
}

barplotServer <- function(id, plot_data) {
  
  moduleServer(id, function(input, output, session) {
    
    
   
    plot_area <- reactive({bar_chart_area(plot_data())})
    plot_holding <- reactive({bar_chart_holdings(plot_data())})
   
     output$area_plot <-renderHighchart({
       ns <- session$ns
       plot_area()})
     output$holding_plot <-renderHighchart({
       ns <- session$ns
       plot_holding()
       })
    

  })
}

#for testing module
bar_plot_demo <- function() {

  plot_data <- farm_types %>% filter(main_farm_type != "All")
  ui <- fluidPage(barplotUI("x"))
  server <- function(input, output, session) {
    
    barplotServer("x", reactive({plot_data}))
  }
  shinyApp(ui, server)

}
#bar_plot_demo()

