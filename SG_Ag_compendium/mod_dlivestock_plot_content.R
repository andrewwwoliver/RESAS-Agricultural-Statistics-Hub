

library(shiny)
library(tidyverse)
#plot_content_module ----

dlivestocklinechartUI <-function(id){
  ns <-  NS(id)
  fluidRow(column(3, chooseSliderSkin("Flat"),
                  sliderInput(ns("year"),
                              "Select year range", 
                              value = c(2012, 2023), 
                              min = 2012,
                              max = 2023,
                              step = 1,
                              sep = "", 
                              ticks = TRUE
                  )),
           column(9,
                  highchartOutput(ns("linechart")))
  )
  }


dlivestocklinechartServer <- function(id, plot_data) {
  
  moduleServer(id, function(input, output, session) {


#dataframe and reactive filter need to be placed here     
    
    plot_data <- reactive({cattle %>% filter(`Cattle by category` == "Female dairy cattle aged 2 and over\r\n- with offspring") %>%
        filter(Year>= min(input$year) & Year<= max(input$year))})

    #
    plot <- reactive({line_chart_livestock(plot_data())})
    
    output$linechart <-renderHighchart({
      plot()
    })
    
  })
  
}

#testing module

livestock_demo <- function() {
     
  
  
    ui <- fluidPage(dlivestocklinechartUI("x"))
  server <- function(input, output, session) {
     dlivestocklinechartServer("x", plot_data)
    
  }
  
  shinyApp(ui, server)
}

livestock_demo()



