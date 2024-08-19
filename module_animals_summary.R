# module_summary_animals.R

# Reactive data setup for Animals
full_data_animals <- reactive({
  total_animals %>%
    pivot_longer(
      cols = starts_with("Total"),
      names_to = "Animal_Type",
      values_to = "Value"
    ) %>%
    mutate(Value = as.numeric(Value))  # Ensure the 'Value' column is numeric
})


animalsSummaryUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div("Adjust the sliders to compare data from different years.", 
            style = "font-size: 14px; font-weight: bold; margin-bottom: 10px;"),
        sliderInput(ns("summary_current_year_animals"), "Year of Interest", min = 2012, max = 2023, value = 2023, step = 1, sep = ""),
        sliderInput(ns("summary_comparison_year_animals"), "Comparison Year", min = 2012, max = 2023, value = 2022, step = 1, sep = "")
      ),
      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Summary Page",
                   value = "Summary_Page",
                   fluidRow(
                     column(width = 6, valueBoxUI(ns("totalCattle")), style = "padding-right: 0; padding-left: 0; padding-bottom: 10px;"),
                     column(width = 6, valueBoxUI(ns("totalSheep")), style = "padding-right: 0; padding-left: 0; padding-bottom: 10px;")
                   ),
                   fluidRow(
                     column(width = 6, valueBoxUI(ns("totalPigs")), style = "padding-right: 0; padding-left: 0;"),
                     column(width = 6, valueBoxUI(ns("totalPoultry")), style = "padding-right: 0; padding-left: 0;")
                   ),
                   # Add the footer text
                   div(
                     style = "margin-top: 20px; padding: 10px; border-top: 1px solid #ddd;",
                     HTML("<strong>Note:</strong> Poultry estimates for 2023 are not comparable to previous years due to methodological improvements.")
                   )
          )
        )
      )
    )
  )
}


# Server for Summary Animals Module
animalsSummaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    current_year <- reactive({ input$summary_current_year_animals })
    comparison_year <- reactive({ input$summary_comparison_year_animals })
    valueBoxServer("totalCattle", full_data_animals, "Animal_Type", reactive("Total cattle"), current_year, comparison_year, "cattle")
    valueBoxServer("totalSheep", full_data_animals, "Animal_Type", reactive("Total sheep"), current_year, comparison_year, "sheep")
    valueBoxServer("totalPigs", full_data_animals, "Animal_Type", reactive("Total pigs"), current_year, comparison_year, "pigs")
    valueBoxServer("totalPoultry", full_data_animals, "Animal_Type", reactive("Total poultry"), current_year, comparison_year, "poultry")
    
    
  })
}

# Testing module
content_demo <- function() {
  ui <- fluidPage(animalsSummaryUI("summary_animals_test"))
  server <- function(input, output, session) {
    animalsSummaryServer("summary_animals_test")
  }
  shinyApp(ui, server)
}

content_demo()
