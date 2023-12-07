#Want to create a population pyramid chart, using numbers rather than percentages (as was done in the JAC publication)

library(tidyverse)
# library(ggplot2)
library(highcharter)

#Read in and process the gender/population data
Gender_data <- read.csv("Data/Gender_table.csv") %>% 
  select(-starts_with("X")) %>% 
  gather(contains("ale"), key="Age group", value="Number") %>% 
  filter(Occupier.working.time=="Total working occupiers")
Gender_data$Gender[substr(Gender_data$`Age group`,1,1)=="M"]="Male"
Gender_data$Gender[substr(Gender_data$`Age group`,1,1)=="F"]="Female"
Gender_data$`Age group` = gsub("Female\\.\\.", "", Gender_data$`Age group`)
Gender_data$`Age group` = gsub("Female\\.", "", Gender_data$`Age group`)
Gender_data$`Age group` = gsub("Male\\.\\.", "", Gender_data$`Age group`)
Gender_data$`Age group`[Gender_data$`Age group`=="Under.25"] = "0.to.25"
Gender_data$`Age group` = gsub("\\.", " ", Gender_data$`Age group`)


#Plot (currently uses ggplot, but should be changed to highcharter for consistency)
#Also needs turned into a function, to be called from the main app code
#Modified from this:https://www.statology.org/population-pyramid-in-r/
# ggplot(filter(Gender_data, `Age group` != "Total"), mapping= aes(x= `Age group`, fill=Gender,
#                                  y=ifelse(test = Gender == "Male",
#                                           yes = -Number, no = Number))) +
#   geom_bar(stat="identity") + 
#   scale_y_continuous(labels = abs) + 
#   labs(title = "Population Pyramid", 
#        x = "Age group", 
#        y = "Number of working occupiers")+
#   coord_flip()

genderplotUI <- function(id) {
  ns <- NS(id)
  fluidRow( 
    tabsetPanel(id = "gender_plot_tab",
                tabPanel("Population pyramid",
                         highchartOutput(ns("gender_plot")))
    )
  )
}

genderServer <- function(id, plot_data) {
  
  moduleServer(id, function(input, output, session) {
    plot_gender <- reactive({gender_chart(plot_data())})
    
    output$gender_plot <-renderHighchart({
      ns <- session$ns
      plot_gender()})
  })
}

#Highcharter version
gender_chart <- function(plot_data) {
  hchart(filter(plot_data, `Age group` != "Total"), "bar", 
         hcaes(
           x = `Age group`, 
           y = ifelse(test = Gender == "Male",
                      yes = -Number, no = Number),
           color=Gender), 
         style = list(fontFamily = "Roboto"))%>%   
    hc_yAxis(
      labels = list(style = list(color =  "#000000", fontSize = "20px", fontFamily = "Roboto"),
                    format = "{value:,.0f}",
                    formatter = htmlwidgets::JS("function() {return Math.abs(this.value);}")),
      title = list(text = "Number of working occupiers",
                   style = list(color = "#000000", fontSize = "20px", fontFamily = "Roboto"))) %>%
    hc_xAxis(
      labels = list(style = list(color =  "#000000", fontSize = "20px", fontFamily = "Roboto")),
      title = list(text = "Age group", style = list(color = "#000000", fontSize = "20px", fontFamily = "Roboto")),
      type = "category",
      reversed = FALSE) %>%
    # hc_legend(
    #   align = "left",
    #           verticalAlign = "top",
    #           alignColums = FALSE,
    #           layout = "vertical") %>%
    hc_tooltip(
      formatter = htmlwidgets::JS("function() {return Math.abs(this.y);}")
    )
}

