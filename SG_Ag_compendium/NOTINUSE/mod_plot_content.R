#plot_content_module ----

content_ui <-function(id){
  
  fluidRow(column(4,
                  selectInput("item",
                              "Choose crop category to add to chart",
                              choices= item,
                              selected =NULL),
                  selectInput("total_items",
                              "Or choose crop totals to add to chart",
                              choices= total_items,
                              selected =NULL),
                  
                  chooseSliderSkin("Flat"),
                  sliderInput( 
                    "year",
                    "Select year range", 
                    value = c(2003, 2023), 
                    min = 2003,
                    max = 2023,
                    step = 1,
                    sep = "", 
                    ticks = TRUE
                  )),
           column(8,
                  plot_ui(NS(id, "data")))
  )
  
}

content_server <- function(id, plot_data, filter) {
  
  moduleServer(id, function(input, output, session) {
    
    plot_server("data", plot_data, filter)
    
    
  })
  
}