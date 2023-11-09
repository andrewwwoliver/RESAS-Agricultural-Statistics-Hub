library(shiny)
library(tidyverse)
# plot module ----




stackcolUI <- function(id) {
  ns <- NS(id)
  fluidRow( 
    column(11, 
           highchartOutput(ns("stacked_col"))),
  )
  
}

stackcolServer <- function(id, plot_data) {
  
  moduleServer(id, function(input, output, session) {
 
    
    
    #plot_item <- reactive({line_chart_item(plot_data_item())})
    #plot_total <- reactive({line_chart_total(plot_data_total())})
    plot <- reactive({stacked_col(plot_data())})
    output$stacked_col <-renderHighchart({
      ns <- session$ns
      # if(is.null(input$item)){
      # plot_item()}
      # else{plot_total()}
        plot()
        
        })
    
    
    
  })
}

#testing module
plot_demo <- function() {
  
  plot_data <- crops %>% filter(Crop %in% land_use & Crop != "Total agricultural area")

  ui <- fluidPage(stackcolUI("x"))
    
  server <- function(input, output, session) {
    
    stackcolServer("x", reactive({plot_data}))
  }
  shinyApp(ui, server)

}
 plot_demo()

