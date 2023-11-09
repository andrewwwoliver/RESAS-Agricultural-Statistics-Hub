#plot_content_module ----
library(shiny)
lineselectUI <-function(id){
  ns <- NS(id)
  fluidRow(column(3,
                  selectInput(ns("item"),
                              "Choose crop category to add to chart",
                              choices= item,
                              selected ="Cereals"),
                  selectInput(ns("total_item"),
                              "Or choose crop  totals to add to chart",
                              choices= total_item,
                              selected =NULL),
                  
                  chooseSliderSkin("Flat"),
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
                  linechartUI(ns("inner1")))
  )
  
}

lineselectServer <- function(id, plot_data, ui=item) {
  
  moduleServer(id, function(input, output, session) {
#values need to be set correctly


    #update selection boxes- if one is selected, other is NULL and vice versa
observeEvent(input$item,{
item_value <- input$total_item
  if(is.null(item_value))
    {updateSelectInput(session, "item", choices = item, selected = NULL)
  }
  else{
    updateSelectInput(session, "total_item", choices = total_item , selected = input$item)
 
  }
},#ignoreInit = TRUE,
ignoreNULL = FALSE
)

observeEvent(input$total_item,{
  total_item_value <- input$total_item
  if(is.null(total_item_value))
    {updateSelectInput(session, "total_item", choices = total_item, selected= NULL)
    }
  else{
    updateSelectInput(session, "item", choices = item, selected = input$total_item)
                      
  }
},#ignoreInit = TRUE,
ignoreNULL = FALSE
)

#return(item)
#return(total_item)

#dataframe and reactive filter need to be placed here     

just_crops <- crops %>% filter(!(Crop %in% land_use))

 plot_data <- reactive({
  if(!is.null(input$item)){filter(just_crops, item %in% input$item)%>%
  filter(Year>= min(input$year) & Year<= max(input$year))}
  else{filter(just_crops, total_item %in% input$total_item)%>%
    filter(Year>= min(input$year) & Year<= max(input$year))}

    })



# plot_data_item<- reactive({
#   filter(crops, item %in% input$item)%>%
#     filter(Year>= min(input$year) & Year<= max(input$year)) 
#     })
# 
# plot_data_total <- reactive({filter(crops, total_item %in% total_item) %>%
#       filter(Year>= min(input$year) & Year<= max(input$year))})


      # 
      # Testing each separately 
      # plot_data <- reactive({filter(crops, item %in% input$item)%>%
      #   filter(Year>= min(input$year) & Year<= max(input$year))})
      # 
      # plot_data <- reactive({filter(crops, total_item %in% input$total_item)%>%
      #     filter(Year>= min(input$year) & Year<= max(input$year))})


    
    linechartServer("inner1", plot_data)
    
  })
  
}

#testing module

content_demo <- function() {

    ui <- fluidPage(lineselectUI("x"))
  server <- function(input, output, session) {


    lineselectServer("x", plot_data)
    
    
  }
  
  shinyApp(ui, server)
}

content_demo()



