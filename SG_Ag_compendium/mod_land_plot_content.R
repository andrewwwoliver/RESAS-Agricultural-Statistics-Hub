#plot_content_module ----

stackedcolumnUI <-function(id){
  ns <- NS(id)
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
                  stackcolUI(ns("column")))
  )
  
}

stackedcolumnServer <- function(id, plot_data) {
  
  moduleServer(id, function(input, output, session) {


#dataframe and reactive filter need to be placed here     

    plot_data <- reactive({crops %>% filter(Crop %in% land_use & Crop != "Total agricultural area") %>% 
      filter(Year>= min(input$year) & Year<= max(input$year))})
    
    stackcolServer("column", plot_data)
    
  })
  
}

#testing module

content_demo <- function() {
  
  
  
    ui <- fluidPage(stackedcolumnUI("x"))
  server <- function(input, output, session) {
    stackedcolumnServer("x", plot_data)
    
  }
  
  shinyApp(ui, server)
}

content_demo()



