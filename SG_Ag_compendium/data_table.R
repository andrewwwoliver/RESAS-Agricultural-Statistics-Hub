

tableUI <- function(id) {
  ns <- NS(id)
  fluidRow( 
    tableOutput(ns("data_table")),
  downloadLink(ns("downloadData"), "Download the data"))
  
  
}

tableServer <- function(id, plot_data) {
  
  moduleServer(id, function(input, output, session) {
    
    
   
    pretty_data_table <- reactive({plot_data})
    
    
    output$data_table <- renderTable(bordered = TRUE, striped = TRUE, 
                                    {pretty_data_table <- format.data.frame(pretty_data_table(),
                                                                            digits = 5, nsmall = 0, big.mark = ",", big.interval = 3L)  
                                      #pretty_data_table$Year <- pretty_data_table()$Year
                                      as_tibble(pretty_data_table)}
    )
    
    output$downloadData <- downloadHandler(
      filename = function(){"Scottish Agriculture data.csv" },
      content = function(file) {write.csv( pretty_data_table(), file)}
      ) 
    
    
  })
    
    
    
}
#for testing module
table_demo <- function() {
  plot_data <- dairy  
  
  ui <- fluidPage(tableUI("x"))
 
  
  server <- function(input, output, session) {
   
  tableServer("x", plot_data)
  }
  shinyApp(ui, server)
  
}
table_demo()



  


