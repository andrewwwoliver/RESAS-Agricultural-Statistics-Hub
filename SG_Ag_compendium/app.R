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


#load resources----
#data
source("data_sort.R")

#land use section modules
#content suffix == reactivity with year slider or select 
#(module within a module e.g. mod_land_plot_content calls in mod_land_stacked_plot which calls in stacked_col) 
source("land_line_plot.R")
source("mod_land_stacked_plot.R")
source("mod_land_plot_content.R")
source("bar_plot.R")
source("mod_land_bar_plot.R")
source("stacked_col.R")

#crops section modules
source("mod_tab_line_plot.R")

#data tables module
source("data_table.R")

#livestock modules
#beef
source("livestock_line_plot.R")
source("mod_livestock_plot_content.R")


#dairy
source("mod_dlivestock_plot_content.R")

#full application ----

# Define UI for the landing page
landing_ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(".btn-custom { border: none; }"),
      HTML(".btn-row { margin-top: 20px; }"),
      HTML(".page-content { display: none; }")  
    )
  ),
  
  fluidRow(
    column(width = 2, align = "left", 
           # actionButton("btn_home", 
           tags$img(src = "RESAS Logo.png", width = "100%")), 
    # class = "btn btn-custom")),
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
  )
  
)

# Function to clear the main panel content for Agriculture and the Economy section
clear_agri_econ_main_content <- function() {
  output$main_panel <- renderUI(NULL)
}

# Function to clear the main panel content for Structures of Scottish Agriculture section
clear_struct_agri_main_content <- function() {
  output$main_panel <- renderUI(NULL)
}

# Function to clear the main panel content for Crops section
clear_crops_main_content <- function() {
  output$main_panel <- renderUI(NULL)
}

# Function to clear the main panel content for Livestock section
clear_livestock_main_content <- function() {
  output$main_panel <- renderUI(NULL)
}

# Function to clear the main panel content for Agriculture and the environment section
clear_agri_env_main_content <- function() {
  output$main_panel <- renderUI(NULL)
}

# Function to clear the main panel content for Food and drink section
clear_food_drink_main_content <- function() {
  output$main_panel <- renderUI(NULL)
}

# Create a reactive value to store the main content
main_content <- reactiveVal(NULL)

# Create a reactive value to store the section content
section_content <- reactiveVal(NULL)

# Create a reactive value to store the sub-section content
sub_section_content <- reactiveVal(NULL)

# Create a reactive value to store the home button content
home_button_clicked <- reactiveVal(FALSE)


# Function to clear the main content
clear_main_content <- function() {
  main_content(NULL)
}

# Function to clear the sub-section content
clear_section_content <- function() {
  sub_content(NULL)
}

# Function to clear the sub-section content
clear_sub_section_content <- function() {
  sub_section_content(NULL)
}


clear_section_main_page <- function() {
  output$page <- renderUI(NULL)
}

# Define UI for each sub-section
sub_section_ui <- function(sub_section_title) {
  fluidPage(
    mainPanel(
      uiOutput("sub_section_content")
    )
  )
  
}

# Define UI for each section
section_ui <- function(title, image, content, sections) {
  fluidPage(
    fluidRow(
      column(width = 2, align = "left", 
             # actionButton("btn_home", 
             tags$img(src = "RESAS Logo.png", width = "100%")), 
      # class = "btn btn-custom")),
      column(width = 8, align = "center", h1("Scottish Agricultural Statistics Hub")),
      column(width = 2, align = "right", tags$img(src = "sg.png", width = "100%")),
      column(width = 12, align = "center", h4("Collection of all Scottish agriculture statistics produced by RESAS (Scottish Government)"))
    ),
    
    sidebarPanel(
      actionButton("btn_home", "Home", class = "btn btn-custom",
                   #Define style of Home button here
                   style="font-weight: bold; font-size: 20px"),
      img(src = image, width = "100%"),
      if (title == "Crops") {
        list(
          div(
            class = "btn-group-vertical",
            style = "width: 100%;",
            actionButton("btn_crops_area", "Area", class = "btn btn-custom"),
            actionButton("btn_crops_production", "Production", class = "btn btn-custom")
            # actionButton("btn_crops_value", "Value", class = "btn btn-custom")
          )
        )
      } else if (title == "Structure of Scottish agriculture") {
        list(
          div(
            class = "btn-group-vertical",
            style = "width: 100%;",
            actionButton("btn_land_use", "Land Use", class = "btn btn-custom"),
            #commented out because probably ok to delete ub sections
            #actionButton("btn_agri_area", "Agricultural Area", class = "btn btn-custom"),
            #actionButton("btn_livestock_01", "Livestock", class = "btn btn-custom"),
            actionButton("btn_farm_types", "Farm Types", class = "btn btn-custom"),
            actionButton("btn_labour", "Labour", class = "btn btn-custom")
          )
        )
      } else if (title == "Agriculture and the economy") {
        #commented out whilst under development
        # list(
        #   div(
        #     class = "btn-group-vertical",
        #     style = "width: 100%;",
        #     actionButton("btn_farm_income", "Farm Income", class = "btn btn-custom"),
        #     actionButton("btn_support_payments", "Support Payments", class = "btn btn-custom"),
        #     actionButton("btn_output", "Output", class = "btn btn-custom"),
        #     actionButton("btn_input_costs", "Input Costs", class = "btn btn-custom"),
        #     actionButton("btn_total_income", "Total Income From Farming", class = "btn btn-custom"),
        #     actionButton("btn_avg_net_worth", "Average net Worth", class = "btn btn-custom")
        #   )
        #)
        list(
          div(
            class = "btn-group-vertical",
            style = "width: 100%;",
            # actionButton("btn_in_dev", "Content under development", class = "btn btn-custom")
          ))
        
        
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
      } else if (title == "Agriculture and the environment") {
        #commented out whilst under development
        # list(
        #   div(
        #     class = "btn-group-vertical",
        #     style = "width: 100%;",
        #     actionButton("btn_ghg_emissions", "Agriculture GHG Emissions", class = "btn btn-custom"),
        #     actionButton("btn_farm_emissions", "Farm Emissions", class = "btn btn-custom"),
        #     actionButton("btn_fertiliser_use", "Fertiliser Use", class = "btn btn-custom"),
        #     actionButton("btn_legume_cover", "Legume Cover", class = "btn btn-custom")
        #   )
        
        #)
        list(
          div(
            class = "btn-group-vertical",
            style = "width: 100%;",
            # actionButton("btn_in_dev_env", "Content under development", class = "btn btn-custom")
          )
        )
        
      } else if (title == "Food and drink") {
        list(
          div(
            class = "btn-group-vertical",
            style = "width: 100%;",
            # actionButton("btn_employment", "Employment", class = "btn btn-custom"),
            # actionButton("btn_production", "Production", class = "btn btn-custom"),
            # actionButton("btn_gross_value", "Gross Value Added", class = "btn btn-custom"),
            # actionButton("btn_labour_productivity", "Labour Productivity", class = "btn btn-custom")
          )
        )
      } else {
        list(
          div(
            class = "btn-group-vertical",
            style = "width: 100%;",
            actionButton("btn_example", "Example", class = "btn btn-custom")
          )
        )
      },
      style = "width: auto; max-width: 300px; margin-right: 10px;",
      
      # Add a search bar specific to this section
      # textInput("section_search_quercy", "Search", placeholder = paste("Search within", title, "...")),   
      
    ),
    
    mainPanel(
      uiOutput("sub_section_content"),
    )
    
  )
}


# Define UI for the app
# Dynamic UI content will be rendered here
# MAIN PAGE- WHAT YOU SEE WHEN YOU CLICK EACH BUTTON
ui <- fluidPage(
  
  uiOutput("page"),
  
)

# Define server logic
server <- function(input, output, session, content) {
  
  sections <- list(
    list(id = "btn_agri_econ", title = "Agriculture and the economy"),
    list(id = "btn_struct_agri", title = "Structure of Scottish agriculture"),
    list(id = "btn_crops", title = "Crops"),
    list(id = "btn_livestock", title = "Livestock"),
    list(id = "btn_agri_env", title = "Agriculture and the environment"),
    list(id = "btn_food_drink", title = "Food and drink")
  )
  
  # Add observeEvent to create home button
  observeEvent(input$btn_home, {
    # home_button_clicked(TRUE)
    # clear_main_content()
    # clear_sub_section_content()
    output$page <- renderUI({
      landing_ui
    })
    output$sub_section_content <- NULL
  })
  
  observeEvent(input$btn_home2, {
    # home_button_clicked(TRUE)
    # clear_main_content()
    # clear_sub_section_content()
    output$page <- renderUI({
      landing_ui
    })
    output$sub_section_content <- NULL
  })
  
  # # Function to update the currently displayed sub-section and tab
  # update_sub_section <- function(sub_section, tab) {
  #   current_sub_section(sub_section)
  #   current_tab(tab)
  # }
  # 
  # # Function to clear the current sub-section and tab
  # clear_sub_section <- function() {
  #   current_sub_section(NULL)
  #   current_tab(NULL)
  # }
  # 
  # Function to clear the main content
  clear_main_content <- function() {
    output$main_panel <- renderUI(NULL)
  }
  
  # Create reactive values to store section and sub-section state
  current_section <- reactiveVal(NULL)
  current_sub_section <- reactiveVal(NULL)
  
  #commented out because only referenced here and not in ui....possibly delete. (Jackie)
  # Add observeEvent for action button button for Agriculture and the Economy section
  # observeEvent(input$btn_agri_econ_home, {
  #   clear_agri_econ_main_content()
  #   clear_sub_section_content()
  #   output$page <- renderUI({
  #     section_ui(
  #       title = "Agriculture and the economy",
  #       image = "agri_econ.png"
  #       #sections = sections
  #     )
  #   })
  # })
  
  # Add observeEvents for other section action buttons...
  
  observeEvent(input$btn_agri_econ, {
    # clear_main_content()
    #    clear_sub_section_content()
    #clear_section_main_page
    output$page <- renderUI({
      
      fluidPage(
        
        section_ui(
          title = "Agriculture and the economy",
          image = "agri_econ.png"
          #below is commented out because it doesn't seem to have a working function! (Jackie)
          # content = HTML("Content Under Development. For More Information on
          #                Agriculture and the economy click <a href='enterURL_here' target='_blank'>here</a>"),
          # sections = sections
        )
      )
    })
    output$sub_section_content <- renderUI({
      fluidPage(
        HTML("<div style='margin-top: 40px; font-weight: 
                                 bold;'>Content under development</a></div>"),
        HTML("<div style='margin-top: 30px'>
                              Estimates for the net income gained by the agriculture industry in Scotland are available in the
                              <a href='https://www.gov.scot/collections/total-income-from-farming/'
                              target='_blank'>Total income from farming (TIFF) publication.</a></div>"),
        
        HTML("<div style='margin-top: 30px'>
                              Farm business level estimates of average incomes from commercial farms in Scotland are available in the 
                              <a href='https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/'
                              target='_blank'>Scottish farm business income publication.</a></div>"),
        br(),
        br(),
        actionLink("btn_home2", strong("Return to home page")),
        br(),
        br(),
        br()
      )
    })
  })
  
  
  observeEvent(input$btn_struct_agri, {
    clear_main_content()
    clear_sub_section_content()
    output$page <- renderUI({
      section_ui(
        title = "Structure of Scottish agriculture",
        image = "structures.png"
        # below is commented out because it doesn't seem to have a working function! (Jackie)
        # content = HTML("Content Under Development. For More Information on
        # the structure of Scottish Agriculture click <a href='enterURL_here' target='_blank'>here</a>"),
        # sections = sections
      )
    })
    output$sub_section_content <- renderUI({
      fluidPage(
        HTML("<div style='margin-top: 60px; font-weight: bold;'>Content under development</a></div>"),
        br(),
        br(),
        #text found in "data_sort.R" script.
        p(strong(struct_agri_txt1)),
        tags$ul(
          tags$li(struct_agri_txt2),
          tags$li(struct_agri_txt3),
          tags$li(struct_agri_txt4)
        ),
        br(),
        p(struct_agri_txt5, struct_agri_txt6),
        br(),
        p(struct_agri_txt7, struct_agri_txt8, struct_agri_txt9, struct_agri_txt10, struct_agri_txt11, struct_agri_txt12),
        br(),
        br(),
        actionLink("btn_home2", strong("Return to home page")),
        br(),
        br(),
        br()
      )
    })
  })
  
  observeEvent(input$btn_crops, {
    clear_main_content()
    clear_sub_section_content()
    output$page <- renderUI({
      section_ui(
        title = "Crops",
        image = "crops.png"
        #below is commented out because it doesn't seem to have a working function! (Jackie)
        # content = HTML("Content Under Development. For More Information on 
        #                Structures of Scottish Agriculture click <a href='enterURL_here' target='_blank'>here</a>"),
        # sections = sections
      )
    })
    output$sub_section_content <- renderUI({
      fluidPage(
        br(),
        p(strong("Use the links on the left to explore our data on:")),
        tags$ul(
          tags$li("Crop areas"),
          tags$li("Crop production")),
        br(),
        p("More data on crop areas are available in the ",
          tags$a(href="https://www.gov.scot/collections/june-scottish-agricultural-census/", target = "_blank", "Scottish Agricultural Census: results.")),
        br(),
        p("More data on crop production are available in the ",
          tags$a(href="https://www.gov.scot/collections/scottish-cereal-harvest-estimates/", target = "_blank", "Scottish cereal harvest: estimates"),
          ", ",
          tags$a(href="https://www.gov.scot/collections/total-income-from-farming/", target = "_blank", "Total income from farming"),
          "publications and ",
          tags$a(href="https://www.gov.scot/collections/economic-report-on-scottish-agriculture/", target = "_blank", "Scottish agriculture: economic reports.")),
        br(),
        p("Financial performance of cropping farms is available through ",
          tags$a(href="https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/", target = "_blank", "Scottish farm business income estimates.")),
        p("Data on cereal and oilseed markets, including prices and estimates of supply, demand, imports and exports are published on the ",
          tags$a(href="https://ahdb.org.uk/cereals-oilseeds-markets", target = "_blank", "AHDB website.")),
        br(),
        br(),
        actionLink("btn_home2", strong("Return to home page")),
        br(),
        br(),
        br()
      )
    })
  })
  
  observeEvent(input$btn_livestock, {
    clear_main_content()
    clear_sub_section_content()
    output$page <- renderUI({
      section_ui(
        title = "Livestock",
        image = "livestock.png",
        #below is commented out because it doesn't seem to have a working function! (Jackie)
        # content = HTML("Content Under Development. For More Information on 
        #                Structures of Scottish Agriculture click <a href='enterURL_here' target='_blank'>here</a>"),
        # sections = sections
      )
    })
    output$sub_section_content <- renderUI({
      fluidPage(
        br(),
        p("Use the links on the left to explore our data on livestock numbers by sector."),
        p("More data on livestock numbers are available in the "),
          tags$a(href="https://www.gov.scot/collections/june-scottish-agricultural-census/", target="_blank", "Scottish Agricultural Census: results"),
        p("Data on output are available in ",
          tags$a(href="https://www.gov.scot/collections/total-income-from-farming/", target="_blank", "Total income from farming"),
          " and ",
          tags$a(href="https://www.gov.scot/collections/economic-report-on-scottish-agriculture/", target="_blank", "Scottish agriculture: economic reports")),
        p("Financial performance of livestock farms is available through ",
          tags$a(href="https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/", target="_blank", "Scottish farm business income estimates")),
        p("More data on livestock prices can be found on the ",
          tags$a(href="https://ahdb.org.uk/markets-and-prices", target="_blank", "AHDB website")),
        br(),
        br(),
        actionLink("btn_home2", strong("Return to home page")),
        br(),
        br(),
        br()
      )
    })
  })
  
  # output$sub_section_content <- renderUI({
  #   fluidPage(
  #     HTML("<div style='margin-top: 60px; font-weight: bold;'>Content under development</a></div>"),
  
  observeEvent(input$btn_agri_env, {
    clear_main_content()
    clear_sub_section_content()
    output$page <- renderUI({
      fluidPage(
        section_ui(
          title = "Agriculture and the environment",
          image = "agri_env.png",
          #below is commented out because it doesn't seem to have a working function! (Jackie)
          # content = HTML("Content Under Development. For More Information on 
          #                Structures of Scottish Agriculture click <a href='enterURL_here' target='_blank'>here</a>"),
          # sections = sections
        )
      )}
    )    
    output$sub_section_content <- renderUI({
      fluidPage(
        HTML("<div style='margin-top: 60px; font-weight: bold;'>Content under development</a></div>"),
        br(),
        br(),
        #text found in "data_sort.R" script.
        p(strong(agri_env_txt100)),
        p(agri_env_txt101, agri_env_txt102),
        br(),
        p(strong(agri_env_txt103)),
        p(agri_env_txt104, agri_env_txt105),
        br(),
        p(strong(agri_env_txt106)),
        p(agri_env_txt5, agri_env_txt6),
        br(),
        p(strong(agri_env_txt108)),
        p(agri_env_txt109, agri_env_txt110),
        br(),
        p(strong(agri_env_txt7)),
        p(agri_env_txt8),
        br(),
        p(strong(agri_env_txt9)),
        p(agri_env_txt10),
        p(agri_env_txt11),
        p(agri_env_txt12),
        p(agri_env_txt13),
        br(),
        p(strong(agri_env_txt14)),
        p(agri_env_txt15),
        p(agri_env_txt16),
        p(agri_env_txt17),
        br(),
        p(strong(agri_env_txt18)),
        p(agri_env_txt19),
        p(agri_env_txt20),
        p(agri_env_txt21),
        p(agri_env_txt22),
        p(agri_env_txt23),
        p(agri_env_txt24),
        p(agri_env_txt25),
        p(agri_env_txt26),
        p(agri_env_txt27),
        br(),
        br(),
        actionLink("btn_home2", strong("Return to home page")),
        br(),
        br(),
        br()
      )
    })
  })
  
  observeEvent(input$btn_food_drink, {
    output$page <- renderUI({
      clear_main_content()
      clear_sub_section_content()
      section_ui(
        title = "Food and drink",
        image = "food_drink.png",
        #below is commented out because it doesn't seem to have a working function! (Jackie)
        # content = HTML("Content Under Development. For More Information on 
        #                Structures of Scottish Agriculture click <a href='enterURL_here' target='_blank'>here</a>"),
        # sections = sections
      )
    })
    output$sub_section_content <- renderUI({
      fluidPage(
        br(),
        p(strong("Content under development")),
        p(tags$a(href="https://www.gov.scot/policies/food-and-drink/", target="_blank", "Food and drink is very important to Scotland. It creates jobs and wealth, impacts on health and sustainability, and helps attract people to visit our country.")),
        p("The Scottish food and drink industry spans the activities of agriculture, fishing & aquaculture, food manufacturing and drink manufacturing. The food and drink manufacturing sector is important to Scotland’s manufacturing workforce and accounts for a significant proportion of manufacturing exports. The production of Scotch whisky and other spirit drinks is of particular importance to Scotland. The agriculture industry provides input to the Scottish food and drink industry and, together with the fishing sector, is a major supplier of raw materials to the UK industry."),
        p("Find out more about Scotland's food and drink sector in ",
          tags$a(href="https://www.gov.scot/publications/growth-sector-statistics/", target = "_blank", "Growth Sector Statistics")),
        br(),
        br(),
        actionLink("btn_home2", strong("Return to home page")),
        br(),
        br(),
        br()
      )
    })
  })
  
  
  
  
  # sub-section content ----
  
  #Structure of Scottish Agriculture sub section content----
  observeEvent(input$btn_land_use, {
    clear_sub_section_content()
    sub_section_content({
      #load data for plots/data table
      
      
      output$sub_section_content <- renderUI({
        fluidPage(
          tabsetPanel(
            tabPanel("Summary",  h4("Most of Scotland's area is used for agriculture"),
                     div(
                       # img(src = "land_use_map.svg", width = "50%", align = "center")), #height = "40%"),
                       img(src = "land_use_map_01.png", width = "50%", align = "center")), #height = "40%"),
                     #text found in "data_sort.R" script. Taken from June Census 2023 publication 
                     p(land_use_summary_txt1),
                     p(land_use_summary_txt2),
                     p(strong(land_use_summary_txt3, land_use_summary_txt4)),
                     p(land_use_summary_txt5, land_use_summary_txt6, land_use_summary_txt7),
                     br(),
                     br(),
                     actionLink("btn_home2", strong("Return to home page")),
                     br(),
                     br(),
                     br()
            ),
            tabPanel("Land use chart", h4("Explore agricultural land use results from the Agricultural Census in the interactive plot"),
                     p(land_use_plot_txt1),
                     p(land_use_plot_txt2),
                     p(land_use_plot_txt3),
                     br(),
                     #module UI
                     
                     p(stackedcolumnUI("land_plot"))),
            
            tabPanel("Data", h4("Land Use Data Tables"),
                     p(land_use_data_txt1),
                     p(land_use_data_txt2, land_use_data_txt3),
                     br(),
                     #moduleUI
                     p(tableUI("land_table"))
            )
          ))
      })
      
      #module server
      plot_data_one <- crops %>% 
        filter(Crop %in% land_use) %>% 
        select(Crop, Year, Area) %>% 
        pivot_wider(names_from = Year, values_from = Area)
      #plot_data <- filter(crops, item %in% input$item)%>%
      #filter(Year>= min(input$year) & Year<= max(input$year))
      #
      stackedcolumnServer("land_plot", plot_data)
      tableServer("land_table", plot_data_one)
      
    })
  })
  
  
  observeEvent(input$btn_agri_area, {
    output$sub_section_content <- renderUI({
      fluidPage(
        tabsetPanel(
          tabPanel("Summary",  h4("Agricultural Area Summary Content"),
                   HTML("<div style='margin-top: 20px; font-weight: 
                                   bold;'>Content under development</a></div>"),
                   HTML("<div style='margin-top: 20px; font-weight: 
                                    bold;'>Summary content for agricultural area is within the
                                    <a href='https://www.gov.scot/collections/june-scottish-agricultural-census/' 
                                    target='_blank'>June Scottish Agricultural Census</a></div>")),
          tabPanel("Plot", h4("Agricultural Area Interactive Plots"),
                   HTML("<div style='margin-top: 20px; font-weight: 
                               bold;'>Content under development</a></div>")),
          tabPanel("Data", h4("Agricultural Area Data Tables"),
                   HTML("<div style='margin-top: 20px; font-weight: bold;'>Content under development</a></div>"))
        )
      )
    })
  })
  
  
  
  observeEvent(input$btn_farm_types, {
    output$sub_section_content <- renderUI({
      fluidPage(
        tabsetPanel(
          tabPanel("Summary", h4("Farm types Summary Content"),
                   HTML("<div style='margin-top: 20px; font-weight: 
                                   bold;'>Content under development</a></div>"),
                   br(),
                   p(farm_types_summary_txt1),
                   br(),
                   p(farm_types_summary_txt2),
                   tags$ul(
                     tags$li(farm_types_summary_txt3, farm_types_summary_txt4),
                     tags$li(farm_types_summary_txt5),
                     tags$li(farm_types_summary_txt6),
                     tags$li(farm_types_summary_txt7),
                     tags$li(farm_types_summary_txt8),
                     tags$li(farm_types_summary_txt9),                    
                     tags$li(farm_types_summary_txt10),
                     tags$li(farm_types_summary_txt11),
                     tags$li(farm_types_summary_txt12),
                     tags$li(farm_types_summary_txt13)
                   ),
                   br(),
                   br(),
                   actionLink("btn_home2", strong("Return to home page")),
                   br(),
                   br(),
                   br()
          ),
          tabPanel("Plot", h4("Explore farm type results from the Agricultural Census in the interactive plot"),
                   br(),
                   p("The interactive charts display results by farm type from the 2023 Agricultural Census."),
                   br(),
                   p("You can choose which data to display by selecting a tab."),
                   #module UI
                   barplotUI("bar_plot_tab"),
                   br(),
                   br(),
                   actionLink("btn_home2", strong("Return to home page")),
                   br(),
                   br(),
                   br()
          ),
          tabPanel("Data", h4("Farm Types Data Tables"),
                   #module UI
                   tableUI("farm_type_table"))
          
        )
      )
      
    })
    #module server and data
    plot_data_one <- reactive({
      farm_types %>% 
        filter(main_farm_type != "All")})
    plot_data_two <- farm_types
    barplotServer("bar_plot_tab", plot_data_one)
    tableServer("farm_type_table", plot_data_two)
    
  })
  
  observeEvent(input$btn_labour, {
    output$sub_section_content <- renderUI({
      fluidPage(
        tabsetPanel(
          tabPanel("Summary",  h4("Results from the 2023 Agricultural Census showed that:"),
                   tags$ul(
                     tags$li("the majority of the workforce are owner-occupiers, made up of people who own or rent the farm and work on it"),
                     tags$li("of all working occupiers, 65 per cent are male and 35 per cent are female"),
                     tags$li("the vast majority of working occupiers are aged 45 years or over, only 15 per cent were 44 or under"),
                     tags$li("male and female working occupiers are generally similar ages"),
                     tags$li("the total workforce on agricultural holdings remained stable in 2023 at around 67,000 people.")
                   ),
                   br(),
                   br(),
                   actionLink("btn_home2", strong("Return to home page")),
                   br(),
                   br(),
                   br()
          )
          # ,
          # tabPanel("Plot", h4("Labour Interactive Plots"),
          #          HTML("<div style='margin-top: 20px; font-weight: 
          #                      bold;'>Content under development</a></div>")),
          # tabPanel("Data", h4("Labour Data Tables"),
          #          HTML("<div style='margin-top: 20px; font-weight: 
          #               bold;'>Content under development</a></div>"))
        )
      )
    })
  })
  # 
  # 
  # 
  
  #  Agriculture and economy sub_sections -----------------------------------
  
  
  # 
  # section under development - comment out when developing subsection
  observeEvent(input$btn_in_dev, {
    output$sub_section_content <- renderUI({
      fluidPage(
        HTML("<div style='margin-top: 40px; font-weight: 
                                 bold;'>Content under development</a></div>"),
        HTML("<div style='margin-top: 30px'>
                              Estimates for the net income gained by the agriculture industry in Scotland are available in the
                              <a href='https://www.gov.scot/collections/total-income-from-farming/'
                              target='_blank'>Total income from farming (TIFF) publication.</a></div>"),
        
        HTML("<div style='margin-top: 30px'>
                              Farm business level estimates of average incomes from commercial farms in Scotland are available in the 
                              <a href='https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/'
                              target='_blank'>Scottish farm business income publication.</a></div>"),
        br(),
        br(),
        actionLink("btn_home2", strong("Return to home page")),
        br(),
        br(),
        br()
      )
    })
  })
  
  #temporarily commented out- comment back when developing
  # observeEvent(input$btn_farm_income, {
  #   output$sub_section_content <- renderUI({
  #     fluidPage(
  #       tabsetPanel(
  #         tabPanel("Summary",  h4("Farm Income Summary Content"),
  #                  HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>"), 
  #                  HTML("<div style='margin-top: 20px; font-weight: bold;'> 
  #                       Total Income information can be found within the
  #                       <a href='https://www.gov.scot/collections/total-income-from-farming/' 
  #                       target='_blank'>Total Income From Farming(TIFF) Publications</a></div>")),
  #         tabPanel("Plot", h4("Farm Income Interactive Plots"),
  #                  HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>")),
  #         tabPanel("Data", h4("Farm Income Data Tables"),
  #                  HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>"),
  #                  HTML("<div style='margin-top: 20px; font-weight: bold;'> 
  #                       Farm income data tables can be found within the
  #                       <a href='https://www.gov.scot/collections/total-income-from-farming/' 
  #                       target='_blank'>Total Income From Farming(TIFF) Publications</a></div>")),
  #       )
  #     )
  #   })
  # })
  # 
  # observeEvent(input$btn_support_payments, {
  #   output$sub_section_content <- renderUI({
  #     fluidPage(
  #       tabsetPanel(
  #         tabPanel("Summary",  h4("Support Payments Summary Content"),
  #                  HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>"),
  #                  HTML("<div style='margin-top: 20px; font-weight: bold;'> 
  #                       Support Payments information can be found within the
  #                       <a href='https://www.gov.scot/publications/total-income-farming-estimates-2018-2020/pages/9/' 
  #                       target='_blank'>Total Income From Farming(TIFF) Publications - Support Payments</a></div>")),
  #         tabPanel("Plot", h4("Support Payments Interactive Plots"),
  #                  HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>")),
  #         tabPanel("Data", h4("Support Payments Data Tables"),
  #                  HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>"),
  #                  HTML("<div style='margin-top: 20px; font-weight: bold;'> 
  #                       Support payments data tables can be found within the
  #                       <a href='https://www.gov.scot/collections/total-income-from-farming/' 
  #                       target='_blank'>Total Income From Farming(TIFF) Publications</a></div>")),
  #       )
  #     )
  #   })
  # })
  # 
  # observeEvent(input$btn_output, {
  #   output$sub_section_content <- renderUI({
  #     fluidPage(
  #       tabsetPanel(
  #         tabPanel("Summary",  h4("Output Summary Content"),
  #                             HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>")),
  #         tabPanel("Plot", h4("Output Interactive Plots"),
  #                          HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>")),
  #         tabPanel("Data", h4("Output Data Tables"),
  #                         HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>"),
  #                         HTML("<div style='margin-top: 20px; font-weight: bold;'> 
  #                             Output data can be found within the
  #                             <a href='https://www.gov.scot/collections/total-income-from-farming/' 
  #                             target='_blank'>Total Income From Farming(TIFF) Publications</a></div>"))
  #       )
  #     )
  #   })
  # })
  # 
  # observeEvent(input$btn_input_costs, {
  #   output$sub_section_content <- renderUI({
  #     fluidPage(
  #       tabsetPanel(
  #         tabPanel("Summary",  h4("Input Costs Summary Content"),
  #                             HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>")),
  #         tabPanel("Plot", h4("Input Costs Interactive Plots"),
  #                         HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>")),
  #         tabPanel("Data", h4("Input Costs Data"),
  #                         HTML("<div style='margin-top: 20px; font-weight: bold;'>
  #                         Input Costs data can be found within the 
  #                         <a href='https://www.gov.scot/collections/total-income-from-farming/' 
  #                         target='_blank'>Total Income From Farming(TIFF) Publications</a></div>"))
  #       )
  #     )
  #   })
  # })
  # 
  # observeEvent(input$btn_total_income, {
  #   output$sub_section_content <- renderUI({
  #     fluidPage(
  #       tabsetPanel(
  #         tabPanel("Summary",  h4("Total Income From Farming Summary Content"),
  #                  HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>"),
  #                  HTML("<div style='margin-top: 20px; font-weight: bold;'>Content under development. 
  #                       Total Income From Farming information can be found within the
  #                       <a href='https://www.gov.scot/collections/total-income-from-farming/' 
  #                       target='_blank'>Total Income From Farming(TIFF) Publications</a></div>")),
  #         tabPanel("Plot", h4("TIFF Interactive Plots"),
  #                  HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>")),
  #         tabPanel("Data", h4("TIFF Data Tables"),
  #                  HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>"),
  #                  HTML("<div style='margin-top: 20px; font-weight: bold;'> 
  #                       TIFF data tables can be found within the
  #                       <a href='https://www.gov.scot/collections/total-income-from-farming/' 
  #                       target='_blank'>Total Income From Farming(TIFF) Publications</a></div>")),
  #       )
  #     )
  #   })
  # })
  # 
  # observeEvent(input$btn_avg_net_worth, {
  #   output$sub_section_content <- renderUI({
  #     fluidPage(
  #       tabsetPanel(
  #         tabPanel("Summary",  h4("Average Net Worth Summary Content"),
  #                  HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>"),
  #                  HTML("<div style='margin-top: 20px; font-weight: bold;'> 
  #                       Average Net Worth summary content is within the
  #                       <a href='https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/' 
  #                       target='_blank'>Scottish Farm Business Income (FBI) Annual Estimates Publication</a></div>")
  #         ),
  #         tabPanel("Plot", h4("Average Net Worth Interactive Plots"),
  #                  HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>")),
  #         tabPanel("Data", h4("Average Net Worth Data"),
  #                  HTML("<div style='margin-top: 20px; font-weight: 
  #                                  bold;'>Content under development</a></div>"))
  #       )
  #     )
  #   })
  # })
  
  # Crops sub sections -------------------------------------------------------
  
  
  observeEvent(input$btn_crops_area, {
    output$sub_section_content <- renderUI({
      fluidPage(
        tabsetPanel(
          tabPanel("Summary",  h4("Crop area results from the Agricultural Census"),
                   p("In 2023 winter planting rose by four per cent compared with the five year average (2017 – 2021). This was followed by a decrease of two per cent in spring planting."),
                   p("Compared to the previous five year average (2017 – 2021, data are not currently available for 2022):"),
                   tags$ul(
                     tags$li("the area of land used for growing cereals remained stable"),
                     tags$li("the area used for planting oilseeds rose by 25 per cent"),
                     tags$li("growing areas of vegetables for human consumption, excluding potatoes, grew by seven per cent"),
                     tags$li("the amount of area used to grow vegetables for stockfeed decreased by five per cent"),
                     tags$li("the area of potatoes dropped by six per cent")
                   ),
                   p("More data on crop areas are available in the ",
                     tags$a(href="https://www.gov.scot/collections/june-scottish-agricultural-census/", target="_blank", "Scottish Agricultural Census: results")),
                   br(),
                   br(),
                   actionLink("btn_home2", strong("Return to home page")),
                   br(),
                   br(),
                   br()
          ),
          
          tabPanel("Plot", h4("Explore crop area results from the Agricultural Census in the interactive plot"),
                   #module UI
                   lineselectUI("crop_plot")) ,
          tabPanel("Data", h4("Crop area data table from the Agricultural Census"),
                   #module UI
                   tableUI("crop_table")))
      )
      
    })
    
    #module server
    plot_data_one <- crops %>% filter(!(Crop %in% land_use)) %>%
      select(Crop, Year, Area) %>%
      pivot_wider(names_from = Year, values_from = Area)
    
    lineselectServer("crop_plot", plot_data_one, plot_data_two)
    tableServer("crop_table", plot_data_one)
    
  })
  
  
  observeEvent(input$btn_crops_production, {
    output$sub_section_content <- renderUI({
      fluidPage(
        tabsetPanel(
          tabPanel("Summary",  h4("Crop production results from the Scottish cereal harvest estimates"),
                   p("Industry experts predict an average year for cereal production. Total cereal production is expected to be around 3.0 million tonnes, in line with the ten-year average. Find out more in the ",
                     tags$a(href="https://www.gov.scot/publications/cereal-oilseed-rape-harvest-2023-first-estimates/", target="_blank", "Cereal and oilseed rape harvest - first estimates: 2023")),
                   br(),
                   p("Interactive plots of cereal and oilseed rape data can be found in the ",
                     tags$a(href="https://scotland.shinyapps.io/sg-cereal-oilseed-rape-harvest/", target="_blank", "Cereals Oilseed Rape Harvest Shiny App")),
                   br(),
                   p("More data on crop production are available in the ",
                     tags$a(href="https://www.gov.scot/collections/scottish-cereal-harvest-estimates/", target = "_blank", "Scottish cereal harvest: estimates"),
                     ", ",
                     tags$a(href="https://www.gov.scot/collections/total-income-from-farming/", target = "_blank", "Total income from farming"),
                     "publications and ",
                     tags$a(href="https://www.gov.scot/collections/economic-report-on-scottish-agriculture/", target = "_blank", "Scottish agriculture: economic reports.")),
                   br(),
                   br(),
                   actionLink("btn_home2", strong("Return to home page")),
                   br(),
                   br(),
                   br()
                   
          )
          # tabPanel("Plot", h4("Crops Production Interactive Plots"),
          #                 HTML("<div style='margin-top: 20px; font-weight: 
          #                      bold;'>Content under development</a></div>"),
          #                 HTML("<div style='margin-top: 20px; font-weight: 
          #                       bold;'>Interactive plots of cereal and oilseed rape data can be found in the 
          #                       <a href='https://scotland.shinyapps.io/sg-cereal-oilseed-rape-harvest/' 
          #                       target='_blank'>Cereals Oilseed Rape Harvest Shiny App</a></div>")),
          # tabPanel("Data", h4("Crops Production Data tables"),
          #          HTML("<div style='margin-top: 20px; font-weight: bold;'>Content under development</a></div>"))
        )
      )
    })
  })
  
  # observeEvent(input$btn_crops_value, {p("More data on crop production are available in the ",
#   output$sub_section_content <- renderUI({
#     fluidPage(
#       tabsetPanel(
#         tabPanel("Summary",  h4("Crops Value Summary Content"),
#                  HTML("<div style='margin-top: 20px; font-weight: 
#                                  bold;'>Content under development</a></div>"),
#                  HTML("<div style='margin-top: 20px; font-weight: 
#                                   bold;'>Crops value summarised content is within the
#                                   <a href='https://www.gov.scot/collections/june-scottish-agricultural-census/' 
#                                   target='_blank'>June Scottish Agricultural Census</a></div>")),
#         tabPanel("Plot", h4("Crops Area Interactive Plots"),
#                  HTML("<div style='margin-top: 20px; font-weight: 
#                              bold;'>Content under development</a></div>")),
#         tabPanel("Data", h4("Crops Area Data Tables"),
#                  HTML("<div style='margin-top: 20px; font-weight: bold;'>Content under development</a></div>"))
#       )
#     )
#   })
# })

# Livestock sub-sections --------------------------------------------------


observeEvent(input$btn_dairy, {
  #data for table
  plot_data <- dairy
  
  output$sub_section_content <- renderUI({
    fluidPage(
      tabsetPanel(
        tabPanel("Summary",  h4("Dairy Summary Content"),
                 #moduleUI
                 tableUI("sector_summary"),
                 #moduleserver
                 tableServer("sector_summary", plot_data ))
        ,
        tabPanel("Plot", h4("Dairy Interactive Plots"),
                 #moduleUI
                 dlivestocklinechartUI("dairy_plot")
        ),
        tabPanel("Data", h4("Dairy Data Tables"),
                 HTML("<div style='margin-top: 20px; font-weight: bold;'>Content under development</a></div>"))
      )
    )
  })
  dlivestocklinechartServer("dairy_plot") 
  
})

observeEvent(input$btn_beef, {
  plot_data <- beef
  output$sub_section_content <- renderUI({
    fluidPage(
      tabsetPanel(
        tabPanel("Summary",  h4("Beef Summary Content"),
                 #moduleui
                 tableUI("bsector_summary"),
                 #moduleserver
                 tableServer("bsector_summary", plot_data)),
        tabPanel("Plot", h4("Beef Interactive Plots"),
                 #moduleUI
                 livestocklinechartUI("beef_plot")
                 
        ),
        tabPanel("Data", h4("Beef Data Tables"),
                 #moduleUI
                 #tableUI("beef_plot_table"))
        )
      )
    )
  })
  
  #module server and data
  #ideally want to define plot_data dataframe here not in module. but problem with reactivity....
  # plot_data <- reactive({cattle %>% filter(`Cattle by category` == "Female beef cattle aged 2 and over\r\n- with offspring") #%>% 
  #                # filter(Year>= min(input$year) & Year<= max(input$year))
  #                 }) 
  livestocklinechartServer("beef_plot", plot_data)
  #commented out- erro as relies on plot_data dataframe as defined in the livestocklinechartServer (which has reactivity with the input$year))
  #tableServer("beef_plot_table", plot_data)
  
})

observeEvent(input$btn_sheep, {
  plot_data <- sheep
  output$sub_section_content <- renderUI({
    fluidPage(
      tabsetPanel(
        tabPanel("Summary",  h4("Sheep Summary Content"),
                 #moduleui
                 tableUI("ssector_summary"),
                 #moduleserver
                 tableServer("ssector_summary", plot_data)),
        tabPanel("Plot", h4("Sheep Interactive Plots"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                               bold;'>Content under development</a></div>")),
        tabPanel("Data", h4("Sheep Data Tables"),
                 HTML("<div style='margin-top: 20px; font-weight: bold;'>Content under development</a></div>"))
      )
    )
  })
})

observeEvent(input$btn_pigs, {
  output$sub_section_content <- renderUI({
    fluidPage(
      tabsetPanel(
        tabPanel("Summary",  h4("Pigs Summary Content"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                                   bold;'>Content under development</a></div>"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                                    bold;'>Summary content for pigs is within the
                                    <a href='https://www.gov.scot/collections/june-scottish-agricultural-census/' 
                                    target='_blank'>June Scottish Agricultural Census</a></div>")),
        tabPanel("Plot", h4("Pigs Interactive Plots"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                               bold;'>Content under development</a></div>")),
        tabPanel("Data", h4("Pigs Data Tables"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                        bold;'>Content under development</a></div>"))
      )
    )
  })
})


# Agriculture and the environment sub section -----------------------------
# observeEvent(input$btn_in_dev_env, {
# output$sub_section_content <- renderUI({
#   fluidPage(
#     HTML("<div style='margin-top: 60px; font-weight: bold;'>Content under development</a></div>"),
#     br(),
#     br(),
#     #text found in "data_sort.R" script.
#     p(strong(agri_env_txt100)),
#     p(agri_env_txt101, agri_env_txt102),
#     br(),
#     p(strong(agri_env_txt103)),
#     p(agri_env_txt104, agri_env_txt105),
#     br(),
#     p(strong(agri_env_txt106)),
#     p(agri_env_txt5, agri_env_txt6),
#     br(),
#     p(strong(agri_env_txt108)),
#     p(agri_env_txt109, agri_env_txt110),
#     br(),
#     p(strong(agri_env_txt7)),
#     p(agri_env_txt8),
#     br(),
#     p(strong(agri_env_txt9)),
#     p(agri_env_txt10),
#     p(agri_env_txt11),
#     p(agri_env_txt12),
#     p(agri_env_txt13),
#     br(),
#     p(strong(agri_env_txt14)),
#     p(agri_env_txt15),
#     p(agri_env_txt16),
#     p(agri_env_txt17),
#     br(),
#     p(strong(agri_env_txt18)),
#     p(agri_env_txt19),
#     p(agri_env_txt20),
#     p(agri_env_txt21),
#     p(agri_env_txt22),
#     p(agri_env_txt23),
#     p(agri_env_txt24),
#     p(agri_env_txt25),
#     p(agri_env_txt26),
#     p(agri_env_txt27)
#   )
# })
# }
# )



# commented out whilst in development
# observeEvent(input$btn_ghg_emissions, {
#   output$sub_section_content <- renderUI({
#     fluidPage(
#       tabsetPanel(
#         tabPanel("Summary", h4("Agriculture GHG Emissions Summary Content"),
#                  HTML("<div style='margin-top: 20px; font-weight: 
#                       bold;'>Content under development</a></div>"),
#                  HTML("<div style='margin-top: 20px; font-weight: bold;'> 
#                       Summarised data on Agriculture GHG Emissions can be found within 
#                       <a href='' 
#                       target='_blank'>Agriculture and the environment Publication(not yet published)</a></div>")),
#         tabPanel("Plot", h4("Agriculture GHG Emissions Interactive Plots"),
#                  HTML("<div style='margin-top: 20px; font-weight: 
#                       bold;'>Content under development</a></div>")),
#         tabPanel("Data", h4("Agriculture GHG Emissions Data Tables"),
#                  HTML("<div style='margin-top: 20px; font-weight: 
#                       bold;'>Content under development</a></div>"))
#       )
#     )
#   })
# })
# 
# observeEvent(input$btn_fertiliser_use, {
#   output$sub_section_content <- renderUI({
#     fluidPage(
#       tabsetPanel(
#         tabPanel("Summary", h4("Fertiliser Use Summary Content"),
#                  HTML("<div style='margin-top: 20px; font-weight: 
#                       bold;'>Content under development</a></div>"), 
#                  HTML("<div style='margin-top: 20px; font-weight: bold;'> 
#                       Statistical data on Fertilser Use can be found within the
#                       <a href='' 
#                       target='_blank'>Agriculture and the environment Publication(not yet published)</a></div>")
#         ),
#         tabPanel("Plot", h4("Fertilser Use Interactive Plots"),
#                  HTML("<div style='margin-top: 20px; font-weight: 
#                       bold;'>Content under development</a></div>")),
#         tabPanel("Data", h4("Fertilser Use Data"),
#                  HTML("<div style='margin-top: 20px; font-weight: 
#                       bold;'>Content under development</a></div>"))
#       )
#     )
#   })
# })
# 
# observeEvent(input$btn_farm_emissions, {
#   output$sub_section_content <- renderUI({
#     fluidPage(
#       tabsetPanel(
#         tabPanel("Summary", h4("Farm Emissions Summary Content"),
#                  HTML("<div style='margin-top: 20px; font-weight: 
#                       bold;'>Content under development</a></div>"),
#                  HTML("<div style='margin-top: 20px; font-weight: bold;'> 
#                       Statistical data on Farm Emisssions can be found within the 
#                       <a href='https://sfbs.shinyapps.io/deploy/' 
#                       target='_blank'>FBS Survey Shiny App</a></div>")
#         ),
#         tabPanel("Plot", h4("Farm Emisssions Interactive Plots"),
#                  HTML("<div style='margin-top: 20px; font-weight: 
#                       bold;'>Content under development</a></div>")),
#         tabPanel("Data", h4("Farm Emisssions Data"),
#                  HTML("<div style='margin-top: 20px; font-weight: 
#                       bold;'>Content under development</a></div>"))
#       )
#     )
#   })
# })
# 
# observeEvent(input$btn_legume_cover, {
#   output$sub_section_content <- renderUI({
#     fluidPage(
#       tabsetPanel(
#         tabPanel("Summary", h4("Farm Emissions Summary Content"),
#                  HTML("<div style='margin-top: 20px; font-weight: 
#                       bold;'>Content under development</a></div>"),
#                  HTML("<div style='margin-top: 20px; font-weight: bold;'>Content under development. 
#                       For statistical data on Legume Cover, click here: 
#                       <a href='' 
#                       target='_blank'>Agriculture and the environment Publication(not yet published)</a></div>")
#         ),
#         tabPanel("Plot", h4("Legume Cover Interactive Plots"),
#                  HTML("<div style='margin-top: 20px; font-weight: 
#                       bold;'>Content under development</a></div>")),
#         tabPanel("Data", h4("Legume Cover Data Tables"),
#                  HTML("<div style='margin-top: 20px; font-weight: 
#                       bold;'>Content under development</a></div>"))
#       )
#     )
#   })
# })

# Fodd and drink sub sections ---------------------------------------------


observeEvent(input$btn_employment, {
  output$sub_section_content <- renderUI({
    fluidPage(
      tabsetPanel(
        tabPanel("Summary", h4("Employment Summary Content"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                        bold;'>Content under development</a></div>"), 
                 HTML("<div style='margin-top: 20px; font-weight: bold;'> 
                        Employment statistical summary data can be found within the
                        <a href='https://www.gov.scot/publications/growth-sector-statistics/' 
                        target='_blank'>Growth Sector Employment Statistics Publication</a></div>")),
        tabPanel("Plot", h4("Employment Interactive Plots"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                        bold;'>Content under development</a></div>")),
        tabPanel("Data", h4("Employment Data tables"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                        bold;'>Content under development</a></div>"))
      )
    )
  })
})

observeEvent(input$btn_production, {
  output$sub_section_content <- renderUI({
    fluidPage(
      tabsetPanel(
        tabPanel("Summary", h4("Production Summary Content"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                        bold;'>Content under development</a></div>"), 
                 HTML("<div style='margin-top: 20px; font-weight: bold;'>
                        Production statistical summary data can be found within the 
                        <a href='https://www.gov.scot/publications/growth-sector-statistics/' 
                        target='_blank'>Growth Sector Production Statistics Publication</a></div>")
        ),
        tabPanel("Plot", h4("Production Interactive Plots"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                        bold;'>Content under development</a></div>")),
        tabPanel("Data", h4("Production Data Tables"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                        bold;'>Content under development</a></div>")),
      )
    )
  })
})

observeEvent(input$btn_gross_value, {
  output$sub_section_content <- renderUI({
    fluidPage(
      tabsetPanel(
        tabPanel("Summary", h4("Gross Value Summary Content"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                            bold;'>Content under development</a></div>"),  
                 
        ),
        tabPanel("Plot", h4("Gross Value Interactive Plots"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                            bold;'>Content under development</a></div>")),
        tabPanel("Data", h4("Gross Value Data"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                            bold;'>Content under development</a></div>"),
                 HTML("<div style='margin-top: 20px; font-weight: bold;'> 
                            Gross value statistical summary data tables can be found within the 
                            <a href='https://www.ons.gov.uk/economy/grossvalueaddedgva/datasets/nominalandrealregionalgrossvalueaddedbalancedbyindustry' 
                            target='_blank'>Regional Gross Added Value By Industry - Publication Tables</a></div>"))
      )
    )
  })
})

observeEvent(input$btn_labour_productivity, {
  output$sub_section_content <- renderUI({
    fluidPage(
      tabsetPanel(
        tabPanel("Summary", h4("Trade Summary Content"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                              bold;'>Content under development</a></div>")),  
        tabPanel("Plot", h4("Trade Interactive Plots"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                          bold;'>Content under development</a></div>")),
        tabPanel("Data", h4("Trade Value Data"),
                 HTML("<div style='margin-top: 20px; font-weight: 
                          bold;'>Content under development</a></div>"),
                 HTML("<div style='margin-top: 20px; font-weight: bold;'> 
                              Statistical data summary on trade can be found in the
                              <a href='https://www.uktradeinfo.com/trade-data/rts-custom-table/' 
                              target='_blank'>Trade Data Table</a></div>"))
      )
    )
  })
})


# Clear content and on the main panel when section action buttons on the landing page are clicked
output$main_panel_image <- renderUI({
  req(input$btn_agri_econ)  # Ensure a section button is clicked before rendering an image
  
  # Check which section button was clicked 
  if (input$btn_agri_econ > 0) {
    # Clear content function here:
    clear_sub_section_content
  } else if (input$btn_struct_agri > 0) {
    # Clear content function here:
    clear_sub_section_content
  } else if (input$btn_crops > 0) {
    # Clear content function here:
    clear_sub_section_content
  } else if (input$btn_livestock > 0) {
    # Clear content function here:
    clear_sub_section_content
  } else if (input$btn_agri_env > 0) {
    # Clear content function here:
    clear_sub_section_content
  } else if (input$btn_food_drink > 0) {
    # Clear content function here:
    clear_sub_section_content
  }
})


# Add observeEvents for other section images...

observeEvent(input$btn_agri_econ, {
  # Remove update_image() function here
})

observeEvent(input$btn_struct_agri, {
  # Remove update_image() function here
})

observeEvent(input$btn_crops, {
  # Remove update_image() function here
})

observeEvent(input$btn_livestock, {
  # Remove update_image() function here
})

observeEvent(input$btn_agri_env, {
  # Remove update_image() function here
})

observeEvent(input$btn_food_drink, {
  # Remove update_image() function here
})

# Add observeEvents for section images...

observeEvent(input$btn_agri_econ, {
  #update_image("agri_econ_main.png")
})

observeEvent(input$btn_struct_agri, {
  #update_image("Intro.png")
})

observeEvent(input$btn_crops, {
  #update_image("Intro.png")
})

observeEvent(input$btn_livestock, {
  #update_image("Intro.png")
})

observeEvent(input$btn_agri_env, {
  #update_image("farm_emissions_main.png")
})

observeEvent(input$btn_food_drink, {
  #update_image("Intro.png")
})

# Render the main content based on the main_content reactive value
output$main_panel <- renderUI({
  req(main_content())
  main_content()
})

# Render the sub-section content based on the sub_section_content reactive value
output$sub_section_panel <- renderUI({
  req(sub_section_content())
  sub_section_content()
})

# Initial landing page
output$page <- renderUI({
  landing_ui
})

}

# Run the application
shinyApp(ui, server)