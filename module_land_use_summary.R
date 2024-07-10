# File: module_land_use_summary.R

landUseSummaryUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        checkboxGroupInput(ns("variables"), "Choose variables to add to chart", 
                           choices = c("Total crops, fallow, and set-aside", "Total grass", "Rough grazing"),
                           selected = c("Total crops, fallow, and set-aside", "Total grass", "Rough grazing"))
      ),
      mainPanel(
        id = ns("mainpanel"),
        width = 9,
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Summary",
                   div(
                     class = "container-fluid",  # Use container-fluid for full-width responsive container
                     tags$div(
                       style = "font-size: 24px; font-weight: bold;",
                       "Most of Scotland's area is used for agriculture"
                     ),
                     tags$div(
                       style = "margin-top: 20px; text-align: left;",  # Align the image container to the left
                       tags$img(src = "land_use_map.svg", alt = "Land Use Map", style = "max-width: 100%; height: auto; max-height: 500px;")
                     ),
                     tags$div(
                       style = "margin-top: 20px; font-size: 16px;",
                       HTML("
                         <p>This map shows the main farming types found in each area. Large areas of Scotland have hilly or rocky land suitable for livestock, but limited growing conditions. These areas are shown in light green on the map. The areas in black have better soil and can support crops usually grown for animal feed. Dark green areas can support vegetables, fruit and cereal farming for human consumption.</p>
                         <p>The total Scottish agricultural area in 2023 was 5.33 million hectares, 69 per cent of Scotland’s total land. However, it should be noted that large areas of agricultural land are only lightly farmed. For example, hilly or mountainous areas are mostly used for rough grazing. The total Scottish agricultural area excludes common grazing land.</p>
                         <p>More information about land use is available in the Scottish Agricultural Census.</p>
                         <p>The twin climate change and environment crises mean considerations as to how we own, use and manage our land have never been as important as they are now. Scotland’s land and the natural capital it supports are some of our most valuable assets. It is vital to our environment, economy and wellbeing as individuals and communities. Information about land use policy is available on <a href='https://www.gov.scot' target='_blank'>gov.scot</a>.</p>
                       ")
                     )
                   )
          ),
          tabPanel("Bar Chart",
                   barChartUI(ns("bar_chart"))
          )
        )
      )
    )
  )
}

landUseSummaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    chart_data <- reactive({
      agricultural_area_hectares %>%
        filter(`Crop/Land use` %in% input$variables) %>%
        select(`Crop/Land use`, `2023 Area`) %>%
        rename(Variable = `Crop/Land use`, Value = `2023 Area`)
    })
    
    barChartServer(
      id = "bar_chart",
      chart_data = chart_data,
      title = "Agricultural Area in 2023 by Land Use Type",
      yAxisTitle = "Area (1,000 hectares)",
      xAxisTitle = "Land Use Type",
      footer = '<div style="font-size: 16px; font-weight: bold;"><a href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/">Source: Scottish Agricultural Census: June 2023</a></div>',
      x_col = "Variable",
      y_col = "Value",
      tooltip_format = "Area (hectares): {point.y:.2f}"
    )
  })
}
