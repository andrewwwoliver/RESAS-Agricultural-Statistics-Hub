library(shiny)
library(tidyverse)
# plot module ----



lineselectUI <- function(id) {
  ns <- NS(id)
  fluidRow(tabsetPanel(id = "line_plot_tab",
                       tabPanel("Crops (all categories)",
                                column(2, selectInput(ns("item"),
                                            "Choose crop category to add to chart",
                                            choices= item,
                                            selected =NULL),
                                chooseSliderSkin("Flat"),
                                sliderInput(ns("year1"),
                                            "Select year range", 
                                            value = c(2012, 2023), 
                                            min = 2012,
                                            max = 2023,
                                            step = 1,
                                            sep = "", 
                                            ticks = TRUE
                                )),
                               column(10, highchartOutput(ns("item_plot")))),
                       tabPanel("Crops (totals)",
                                column(2, selectInput(ns("total_item"),
                                            "Choose crop  totals to add to chart",
                                            choices= total_item,
                                            selected =NULL),
                                chooseSliderSkin("Flat"),
                                sliderInput(ns("year2"),
                                            "Select year range", 
                                            value = c(2012, 2023), 
                                            min = 2012,
                                            max = 2023,
                                            step = 1,
                                            sep = "", 
                                            ticks = TRUE)),
                                column(10, highchartOutput(ns("total_plot"))))
  ))
  
}

lineselectServer <- function(id,  plot_data_one, plot_data_two) {
  
  moduleServer(id, function(input, output, session) {
    
    itemplot1 <- reactive({line_chart_items(plot_data_one())})
    itemplot2 <- reactive({line_chart_totals(plot_data_two())})
  
    output$item_plot <-renderHighchart({itemplot1()})
    
    output$total_plot <-renderHighchart({itemplot2()})


  
plot_data_one <-  reactive({crops %>% filter(!Crop %in% land_use) %>% filter(item %in% input$item) %>%  
                                             #& !grepl("total", Crop, ignore.case=TRUE)) %>%
    filter(Year>= min(input$year1) & Year<= max(input$year1))})

  plot_data_two <-reactive({crops %>% filter(!Crop %in% land_use) %>% filter(total_item %in% input$total_item) %>%  
    #& grepl("total", Crop, ignore.case=TRUE)) %>% 
  filter(Year>= min(input$year2) & Year<= max(input$year2))})
   
    
  })
}

plot_demo <- function() {

  
  ui <- fluidPage(lineselectUI("x"))
  server <- function(input, output, session) {
    lineselectServer("x", plot_data_one, plot_data_two)
  }
  shinyApp(ui, server)

}
 plot_demo()

