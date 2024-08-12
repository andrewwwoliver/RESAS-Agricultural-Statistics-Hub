
# File: module_crops_summary.R

# Reactive data setup for Crops
full_data_crops <- reactive({
  crops_summary_data 
})



# UI for Summary Crops Module
cropsSummaryUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        sliderInput(ns("summary_current_year_crops"), "Current Year", min = 2012, max = 2023, value = 2023, step = 1, sep = ""),
        sliderInput(ns("summary_comparison_year_crops"), "Comparison Year", min = 2012, max = 2023, value = 2022, step = 1, sep = "")
      ),
      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Summary Page",
                   value = "Summary_Page",
                   fluidRow(
                     column(width = 6, valueBoxUI(ns("totalCombineHarvestedCrops")), style = "padding-right: 0; padding-left: 0; padding-bottom: 10px;"),
                     column(width = 6, valueBoxUI(ns("totalCropsForStockfeeding")), style = "padding-right: 0; padding-left: 0; padding-bottom: 10px;")
                   ),
                   fluidRow(
                     column(width = 6, valueBoxUI(ns("vegetablesForHumanConsumption")), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 6, valueBoxUI(ns("softFruit")), style = "padding-right: 0; padding-left: 0;")
                   )
          )
        )
      )
    )
  )
}

# Server for Summary Crops Module
cropsSummaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    current_year <- reactive({ input$summary_current_year_crops })
    comparison_year <- reactive({ input$summary_comparison_year_crops })
    
    valueBoxServer("totalCombineHarvestedCrops", full_data_crops, "Crop/Land use", reactive("Total combine harvested crops"), current_year, comparison_year, "ha")
    valueBoxServer("totalCropsForStockfeeding", full_data_crops, "Crop/Land use", reactive("Total crops for stockfeeding"), current_year, comparison_year, "ha")
    valueBoxServer("vegetablesForHumanConsumption", full_data_crops, "Crop/Land use", reactive("Vegetables for human consumption"), current_year, comparison_year, "ha")
    valueBoxServer("softFruit", full_data_crops, "Crop/Land use", reactive("Soft fruit"), current_year, comparison_year, "ha")
  })
}

# Testing module
content_demo <- function() {
  ui <- fluidPage(cropsSummaryUI("summary_crops_test"))
  server <- function(input, output, session) {
    cropsSummaryServer("summary_crops_test")
  }
  shinyApp(ui, server)
}

content_demo()
