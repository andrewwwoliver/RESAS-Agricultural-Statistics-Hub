# load packages
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(highcharter)
library(plotly)
library(tidyverse)
library(shinyjs)
library(BH)

# Define UI for the landing page
landing_ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(".btn-custom { border: none; }"),
      HTML(".btn-row { margin-top: 20px; }"),
      HTML(".page-content { display: none; }")  # Hide all page contents initially
    )
  ),
  
  fluidRow(
    column(width = 2, align = "left", 
           actionButton("btn_back_home", 
                        tags$img(src = "RESAS Logo.png", width = "100%"), 
                        class = "btn btn-custom")),
    column(width = 8, align = "center", h1("Scottish Agricultural Statistics Hub")),
    column(width = 2, align = "right", tags$img(src = "sg.png", width = "100%")),
    column(width = 12, align = "center", h4("Collection of all Scottish agriculture statistics produced by RESAS (Scottish Government)"))
  ),
  
  fluidRow(
    class = "btn-row",
    column(
      width = 4,
      actionButton("btn_agri_econ",
                   img(src = "agri_econ.png", width = "100%"),
                   class = "btn btn-custom"
      )
    ),
    column(
      width = 4,
      actionButton("btn_struct_agri",
                   img(src = "structures.png", width = "100%"),
                   class = "btn btn-custom"
      )
    ),
    column(
      width = 4,
      actionButton("btn_crops",
                   img(src = "crops.png", width = "100%"),
                   class = "btn btn-custom"
      )
    )
  ),
  fluidRow(
    class = "btn-row",
    column(
      width = 4,
      actionButton("btn_livestock",
                   img(src = "livestock.png", width = "100%"),
                   class = "btn btn-custom"
      )
    ),
    column(
      width = 4,
      actionButton("btn_agri_env",
                   img(src = "agri_env.png", width = "100%"),
                   class = "btn btn-custom"
      )
    ),
    column(
      width = 4,
      actionButton("btn_food_drink",
                   img(src = "food_drink.png", width = "100%"),
                   class = "btn btn-custom"
      )
    )
  ),
  # Main panel for displaying images when section action buttons are clicked
  mainPanel(
    uiOutput("main_panel_image")
  )
  
)

# Define UI for each section
section_ui <- function(title, image, content, sections) {
  fluidPage(
    fluidRow(
      column(width = 2, align = "left", 
             actionButton("btn_back_home", 
                          tags$img(src = "RESAS Logo.png", width = "100%"), 
                          class = "btn btn-custom")),
      column(width = 8, align = "center", h1("Scottish Agricultural Statistics Hub")),
      column(width = 2, align = "right", tags$img(src = "sg.png", width = "100%")),
      column(width = 12, align = "center", h4("Collection of all Scottish agriculture statistics produced by RESAS (Scottish Government)"))
    ),
    
    sidebarPanel(
      actionButton("btn_home", "Home", class = "btn btn-custom"),
      img(src = image, width = "100%"),
      if (title == "Crops") {
        list(
          div(
            class = "btn-group-vertical",
            style = "width: 100%;",
            actionButton("btn_crops_area", "Area", class = "btn btn-custom"),
            actionButton("btn_crops_production", "Production", class = "btn btn-custom"),
            actionButton("btn_crops_value", "Value", class = "btn btn-custom")
          )
        )
      } else if (title == "Structures of Scottish Agriculture") {
        list(
          div(
            class = "btn-group-vertical",
            style = "width: 100%;",
            actionButton("btn_land_use", "Land Use", class = "btn btn-custom"),
            actionButton("btn_agri_area", "Agricultural Area", class = "btn btn-custom"),
            actionButton("btn_livestock_01", "Livestock", class = "btn btn-custom"),
            actionButton("btn_farm_types", "Farm Types", class = "btn btn-custom"),
            actionButton("btn_labour", "Labour", class = "btn btn-custom")
          )
        )
      } else if (title == "Agriculture and the Economy") {
        list(
          div(
            class = "btn-group-vertical",
            style = "width: 100%;",
            actionButton("btn_farm_income", "Farm Income", class = "btn btn-custom"),
            actionButton("btn_support_payments", "Support Payments", class = "btn btn-custom"),
            actionButton("btn_output", "Output", class = "btn btn-custom"),
            actionButton("btn_input_costs", "Input Costs", class = "btn btn-custom"),
            actionButton("btn_total_income", "Total Income From Farming", class = "btn btn-custom"),
            actionButton("btn_avg_net_worth", "Average net Worth", class = "btn btn-custom")
          )
        )
      } else if (title == "Livestock") {
        list(
          div(
            class = "btn-group-vertical",
            style = "width: 100%;",
            actionButton("btn_dairy", "Dairy", class = "btn btn-custom"),
            actionButton("btn_beef", "Beef", class = "btn btn-custom"),
            actionButton("btn_sheep", "Sheep", class = "btn btn-custom"),
            actionButton("btn_pigs", "Pigs", class = "btn btn-custom")
          )
        )
      } else if (title == "Agriculture and the Environment") {
        list(
          div(
            class = "btn-group-vertical",
            style = "width: 100%;",
            actionButton("btn_ghg_emissions", "Agriculture GHG Emissions", class = "btn btn-custom"),
            actionButton("btn_farm_emissions", "Farm Emissions", class = "btn btn-custom"),
            actionButton("btn_fertiliser_use", "Fertiliser Use", class = "btn btn-custom"),
            actionButton("btn_legume_cover", "Legume Cover", class = "btn btn-custom")
          )
        )
      } else if (title == "Food and Drink") {
        list(
          div(
            class = "btn-group-vertical",
            style = "width: 100%;",
            actionButton("btn_employment", "Employment", class = "btn btn-custom"),
            actionButton("btn_production", "Production", class = "btn btn-custom"),
            actionButton("btn_gross_value", "Gross Value by Sub-Sector", class = "btn btn-custom"),
            actionButton("btn_trade", "Trade In Food, Feed and Drink", class = "btn btn-custom")
          )
        )
      } 
      else {
        list(
          div(
            class = "btn-group-vertical",
            style = "width: 100%;",
            # actionButton("btn_default_sub_section", "Default Sub-section", class = "btn btn-custom")
            actionButton("btn_farm_income", "Farm Income", class = "btn btn-custom"),
            actionButton("btn_support_payments", "Support Payments", class = "btn btn-custom"),
            actionButton("btn_output", "Output", class = "btn btn-custom"),
            actionButton("btn_input_costs", "Input Costs", class = "btn btn-custom"),
            actionButton("btn_total_income", "Total Income From Farming", class = "btn btn-custom"),
            actionButton("btn_avg_net_worth", "Average net Worth", class = "btn btn-custom")
          )
        )
      },
      style = "width: auto; max-width: 300px; margin-right: 10px;",
      
      # Add a search bar specific to this section
      textInput("section_search_query", "Search", placeholder = paste("Search within", title, "...")),
    ),
    
    # Add conditional panels for each sub-section   
    mainPanel(
      tabsetPanel(
        id = "section_tabs",
        tabPanel("Summary"),
        tabPanel("Plot",
                 sidebarPanel( 
                   p(strong(h4("User Sidebar")),
                     h5("."),
                     h5("."),
                     h5("Drop-down Menu"),
                     h5("."), 
                     h5("Slider"),
                     h5("."),
                     h5("Action Button"), 
                     h5("."),
                     h5("Go Button"),
                     h5("."), 
                     h5("."),
                     h5("."),
                     h5("."),
                     h5(".")
                   ),
                 ),
                 mainPanel(
                   p(strong(h3("Plot")),
                     h4("Under development."), 
                     h4("Interactive plots will be displayed here"),
                   ),
                 )),
        tabPanel("Data Table",
                 mainPanel(
                   p(strong(h3("Data Table")),
                     h4("Under development."), 
                     h4("This sub-section data table will be displayed here"),
                     h5("."), 
                     h5("."),
                     h5("."),
                     h5("."), 
                     h5("."),
                     h5("."),
                     h5("."), 
                     h5("."),
                     h5("."),
                     h5("."),
                     h5("."),
                     h5("."), 
                     h5("."),
                     h5("."),
                     h5("."),
                   ),
                 )),
      ),
      
      #Use shinyjs to hide/show images based on button clicks
      useShinyjs(),
      uiOutput("section_image_output"),  # Display the image here
      conditionalPanel(
        condition = "input.btn_land_use > 0",
        shinyjs::show("section_image_land_use"),
        #img(src = "land_use_map_01.png", width = "80%")
      ),
      conditionalPanel(
        condition = "input.btn_agri_area > 0",
        shinyjs::show("section_image_agri_area"),
        #img(src = "agricultural_area.GIF", width = "60%")
      ),
      conditionalPanel(
        condition = "input.btn_livestock_01 > 0",
        shinyjs::show("section_image_livestock_01"),
        #img(src = "livestock_01.png", width = "90%")
      ),
      conditionalPanel(
        condition = "input.btn_farm_types > 0",
        shinyjs::show("section_image"),
        #img(src = "farm_types.png", width = "90%")
      ),
      conditionalPanel(
        condition = "input.btn_labour > 0",
        shinyjs::show("section_image"),
        #img(src = "labour_01.png", width = "80%")
      ),
      conditionalPanel(
        condition = "input.btn_crops_area > 0",
        shinyjs::show("section_image"),
        #img(src = "crops_03.png", width = "90%")
      ),
      conditionalPanel(
        condition = "input.btn_crops_production > 0",
        shinyjs::show("section_image"),
        #img(src = "crops_prod.png", width = "90%")
      ),
      conditionalPanel(
        condition = "input.btn_crops_value > 0",
        shinyjs::show("section_image"),
        #img(src = "crops_04.png", width = "90%")
      ),
      conditionalPanel(
        condition = "input.btn_farm_income > 0",
        #img(src = "fbi.png", width = "90%")
      ),
      conditionalPanel(
        condition = "input.btn_support_payments > 0",
        shinyjs::show("section_image"),
        #img(src = "farm_types.png", width = "100%")
      ),
      conditionalPanel(
        condition = "input.btn_output > 0",
        shinyjs::show("section_image"),
        #img(src = "", width = "80%")
      ),
      conditionalPanel(
        condition = "input.btn_input_costs > 0",
        shinyjs::show("section_image"),
        #img(src = "", width = "60%")
      ),
      conditionalPanel(
        condition = "input.btn_total_income > 0",
        shinyjs::show("section_image"),
        #img(src = "", width = "90%")
      ),
      conditionalPanel(
        condition = "input.btn_avg_net_worth > 0",
        shinyjs::show("section_image"),
        #img(src = "farm_types.png", width = "90%")
      ),
      conditionalPanel(
        condition = "input.btn_dairy > 0",
        shinyjs::show("section_image"),
        #img(src = "dairy_01.png", width = "80%")
      ),
      conditionalPanel(
        condition = "input.btn_beef > 0",
        shinyjs::show("section_image"),
        #img(src = "beef_02.png", width = "80%")
      ),
      conditionalPanel(
        condition = "input.btn_sheep > 0",
        shinyjs::show("section_image"),
        #img(src = "sheep.png", width = "100%")
      ),
      conditionalPanel(
        condition = "input.btn_pigs > 0",
        shinyjs::show("section_image"),
        #img(src = "pigs.png", width = "60%")
      ),
      conditionalPanel(
        condition = "input.btn_ghg_emissions > 0",
        shinyjs::show("section_image"),
        #img(src = "", width = "90%")
      ),
      conditionalPanel(
        condition = "input.btn_fertiliser_use > 0",
        shinyjs::show("section_image"),
        #img(src = "", width = "80%")
      ),
      conditionalPanel(
        condition = "input.btn_farm_emissions > 0",
        shinyjs::show("section_image"),
        #img(src = "", width = "80%")
      ),
      conditionalPanel(
        condition = "input.btn_legume_cover > 0",
        shinyjs::show("section_image"),
        #img(src = "", width = "80%")
      ),
      conditionalPanel(
        condition = "input.btn_employment > 0",
        shinyjs::show("section_image"),
        #img(src = "labour_01.png", width = "60%")
      ),
      conditionalPanel(
        condition = "input.btn_production > 0",
        shinyjs::show("section_image"),
        #img(src = "", width = "90%")
      ),
      conditionalPanel(
        condition = "input.btn_gross_value > 0",
        shinyjs::show("section_image"),
        #img(src = "", width = "90%")
      ),
      conditionalPanel(
        condition = "input.btn_trade > 0",
        shinyjs::show("section_image"),
        #img(src = "labour_01.png", width = "80%")
      ),
      
      # Use shinyjs to hide/show the default content
      conditionalPanel(
        condition = "input.btn_land_use == 0 && input.btn_agri_area == 0 
    && input.btn_livestock == 0 && input.btn_farm_types == 0 
    && input.btn_labour == 0 && input.btn_crops_area == 0 && input.btn_crops_production == 0
    && input.btn_crops_value == 0 && input.btn_farm_income == 0
    && input.btn_support_payments == 0 && input.btn_output == 0
    && input.btn_input_costs == 0 && input.btn_total_income == 0
    && input.btn_avg_net_worth == 0 && input.btn_dairy == 0
    && input.btn_beef == 0 && input.btn_sheep == 0 && input.btn_pigs == 0
    && input.btn_ghg_emissions == 0 && input.btn_fertiliser_use == 0
    && input.btn_farm_emissions == 0 && input.btn_legume_cover == 0
    && input.btn_employment == 0 && input.btn_production == 0
    && input.btn_gross_value == 0 && input.btn_trade == 0",
        shinyjs::hide("section_image")
      )
    ),
  )
  
}

# Define UI for the app
ui <- fluidPage(
  uiOutput("page")  # Dynamic UI content will be rendered here
)

# Define server logic
server <- function(input, output, session, content) {
  
  sections <- list(
    list(id = "btn_agri_econ", title = "Agriculture and Economy"),
    list(id = "btn_struct_agri", title = "Structures of Scottish Agriculture"),
    list(id = "btn_crops", title = "Crops"),
    list(id = "btn_livestock", title = "Livestock"),
    list(id = "btn_agri_env", title = "Agriculture and the Environment"),
    list(id = "btn_food_drink", title = "Food and Drink")
  )
  
  # Add observeEvent to create home button
  observeEvent(input$btn_home, {
    output$page <- renderUI({
      landing_ui
    })
    
    # Clear the current section and image when returning to the landing page
    current_section(NULL)
    current_image(NULL)
  }) 
  
  # Initialize a reactive value to store the currently displayed image and section
  current_image <- reactiveVal(NULL)
  current_section <- reactiveVal(NULL)
  
  # Function to render the image based on the reactive value
  output$section_image_output <- renderUI({
    if (!is.null(current_image())) {
      img(src = current_image(), width = "90%")
    }
  })
  
  # Function to update the currently displayed image and section
  update_image <- function(image, section) {
    current_image(image)
    current_section(section)
  }
  
  # Function to clear the current image and section
  clear_image <- function() {
    current_image(NULL)
    current_section(NULL)
  }
  
  # Render images onto the main panel when section action buttons on the landing page are clicked
  output$main_panel_image <- renderUI({
    req(input$btn_agri_econ)  # Ensure a section button is clicked before rendering an image
    
    # Check which section button was clicked and display the corresponding image
    if (input$btn_agri_econ > 0) {
      img(src = "agri_econ_main.png", width = "90%")
    } else if (input$btn_struct_agri > 0) {
      img(src = "Intro.png", width = "90%")
    } else if (input$btn_crops > 0) {
      img(src = "", width = "90%")
    } else if (input$btn_livestock > 0) {
      img(src = "", width = "90%")
    } else if (input$btn_agri_env > 0) {
      img(src = "farm_emmisions_main.png", width = "90%")
    } else if (input$btn_food_drink > 0) {
      img(src = "", width = "90%")
    }
  })
  
  
  
  # Add observeEvents for sub-section buttons
  
  observeEvent(input$btn_land_use, {
    update_image("land_use_map_01.png") 
  })
  
  observeEvent(input$btn_agri_area, {
    update_image("agricultural_area.GIF")
  })
  
  observeEvent(input$btn_livestock_01, {
    update_image("livestock_01.png")
  })
  
  observeEvent(input$btn_farm_types, {
    update_image("Farm_Types.png")
  })
  
  observeEvent(input$btn_labour, {
    update_image("crops_prod.png")
  })
  
  observeEvent(input$btn_crops_area, {
    update_image("crops_03.png")
  })
  
  observeEvent(input$btn_crops_production, {
    update_image("crops_prod.png")
  })
  
  observeEvent(input$btn_crops_value, {
    update_image("crops_04.png")
  })
  
  observeEvent(input$btn_farm_income, {
    update_image("fbi.png")
  })
  
  observeEvent(input$btn_support_payments, {
    update_image("Farm_Types.png")
  })
  
  observeEvent(input$btn_output, {
    update_image("")
  })
  
  observeEvent(input$btn_input_costs, {
    update_image("input_costs.png")
  })
  
  observeEvent(input$btn_total_income, {
    update_image("total_income.png")
  })
  
  observeEvent(input$btn_avg_net_worth, {
    update_image("average_income.png")
  })
  
  observeEvent(input$btn_dairy, {
    update_image("dairy_01.png")
  })
  
  observeEvent(input$btn_beef, {
    update_image("beef_02.png")
  })
  
  observeEvent(input$btn_sheep, {
    update_image("sheep.png")
  })
  
  observeEvent(input$btn_pigs, {
    update_image("pigs.png")
  })
  
  observeEvent(input$btn_ghg_emissions, {
    update_image("ghg_emissions.png")
  })
  
  observeEvent(input$btn_fertiliser_use, {
    update_image("")
  })
  
  observeEvent(input$btn_farm_emissions, {
    update_image("farm_emissions_02.png")
  })
  
  observeEvent(input$btn_legume_cover, {
    update_image("")
  })
  
  observeEvent(input$btn_employment, {
    update_image("labour_02.png")
  })
  
  observeEvent(input$btn_production, {
    update_image("production.png")
  })
  
  observeEvent(input$btn_gross_value, {
    update_image("gross_value.png")
  })
  
  observeEvent(input$btn_trade, {
    update_image("")
  })
  
  # Add observeEvents for other section buttons...
  
  observeEvent(input$btn_agri_econ, {
    output$page <- renderUI({
      section_ui(
        title = "Agriculture and Economy",
        image = "agri_econ.png",
        content = img(src = "agri_econ_main.png", width = "100%"),
        sections = sections
      )
    })
  })
  
  
  observeEvent(input$btn_struct_agri, {
    output$page <- renderUI({
      section_ui(
        title = "Structures of Scottish Agriculture",
        image = "structures.png",
        content = img(src = "Intro.png", width = "80%"),
        sections = sections
      )
    })
  })
  
  
  observeEvent(input$btn_crops, {
    output$page <- renderUI({
      section_ui(
        title = "Crops",
        image = "crops.png",
        content = "This is the Crops page content. Tables, Plots and Graphics will display here",
        sections = sections
      )
    })
  })
  
  observeEvent(input$btn_livestock, {
    output$page <- renderUI({
      section_ui(
        title = "Livestock",
        image = "livestock.png",
        content = img(src = "livestock_03.png", width = "100%"),
        sections = sections
      )
    })
  })
  
  observeEvent(input$btn_agri_env, {
    output$page <- renderUI({
      section_ui(
        title = "Agriculture and the Environment",
        image = "agri_env.png",
        content = "This is the Agriculture and the Environment page content. 
        Tables, Plots and Graphics will display here",
        sections = sections
      )
    })
  })
  
  observeEvent(input$btn_food_drink, {
    output$page <- renderUI({
      section_ui(
        title = "Food and Drink",
        image = "food_drink.png",
        content = "This is the Food and Drink page content. Tables, Plots and Graphics will display here",
        sections = sections
      )
    })
  })
  
  # Add observeEvents for other section images...
  
  observeEvent(input$btn_agri_econ, {
    update_image("agri_econ_main.png")
  })
  
  observeEvent(input$btn_struct_agri, {
    update_image("Intro.png")
  })
  
  observeEvent(input$btn_crops, {
    update_image("Intro.png")
  })
  
  observeEvent(input$btn_livestock, {
    update_image("Intro.png")
  })
  
  observeEvent(input$btn_agri_env, {
    update_image("farm_emissions_main.png")
  })
  
  observeEvent(input$btn_food_drink, {
    update_image("Intro.png")
  })
  
  
  
  # Initial landing page
  output$page <- renderUI({
    landing_ui
  })
  
}

# Run the application
shinyApp(ui, server)