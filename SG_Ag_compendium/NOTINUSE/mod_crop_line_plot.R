library(shiny)
library(tidyverse)
# plot module ----




linechartUI <- function(id) {
  ns <- NS(id)
  fluidRow( 
    column(11, 
           highchartOutput(ns("line_plot"))),
  )
  
}

linechartServer <- function(id, plot_data) {
  
  moduleServer(id, function(input, output, session) {
 
    
    
    #plot_item <- reactive({line_chart_item(plot_data_item())})
    #plot_total <- reactive({line_chart_total(plot_data_total())})
    plot <- reactive({line_chart(plot_data())})
    output$line_plot <-renderHighchart({
      ns <- session$ns
      # if(is.null(input$item)){
      # plot_item()}
      # else{plot_total()}
        plot()
        
        })
    
    
    
  })
}

plot_demo <- function() {
  
  plot_data <- crops

  ui <- fluidPage(linechartUI("x"))
    
  server <- function(input, output, session) {
    
    linechartServer("x", reactive({plot_data}))
  }
  shinyApp(ui, server)

}
 plot_demo()

