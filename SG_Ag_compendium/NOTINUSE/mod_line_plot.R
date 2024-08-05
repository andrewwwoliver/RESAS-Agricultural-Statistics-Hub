library(shiny)
library(tidyverse)
load("JAC_crops.RData")
# plot module ----




plot_ui <- function(id) {
  
  fluidRow( 
    column(11, 
           highchartOutput(NS(id, "plot"))),
  )
  
}

plot_server <- function(id, plot_data, filter) {
  
  moduleServer(id, function(input, output, session) {
    
    plot <- reactive({line_chart(plot_data())})
    output$plot <-renderHighchart({plot()})
    
    
    
  })
}

plot_demo <- function() {

  plot_data <- crops
  ui <- fluidPage(plot_ui("x"))
  server <- function(input, output, session) {
    plot_server("x", reactive({plot_data}))
  }
  shinyApp(ui, server)

}
 plot_demo()

