#####################################
####
census_year <- 2023


# File: utils.R

createDownloadHandler <- function(input, file_map_name, file_timeseries_name, map_data, timeseries_data) {
  downloadHandler(
    filename = function() {
      if (input$table_data == "map") {
        file_map_name
      } else {
        file_timeseries_name
      }
    },
    content = function(file) {
      library(openxlsx)
      
      data_to_write <- if (input$table_data == "map") {
        map_data
      } else {
        timeseries_data
      }
      
      write.xlsx(data_to_write, file)
    }
  )
}

# File: utils.R

createNavObserver <- function(input, session, pages) {
  lapply(names(pages), function(page) {
    observeEvent(input[[paste0("nav_", page)]], {
      updateQueryString(paste0("?page=", pages[[page]]), mode = "push")
    })
  })
}

# Helper function to format numbers with commas and appropriate decimal places
format_number <- function(number) {
  # Check if the number is an integer
  if (number %% 1 == 0) {
    return(format(number, big.mark = ",", scientific = FALSE))
  }
  
  # Calculate the number of significant figures
  sig_digits <- nchar(gsub("0+$", "", gsub("\\.", "", as.character(number))))
  
  if (sig_digits > 3) {
    # Round to nearest integer if more than 3 significant figures
    rounded_number <- round(number)
    return(format(rounded_number, big.mark = ",", scientific = FALSE))
  } else {
    # Otherwise, round to 2 decimal places
    rounded_number <- round(number, 2)
    return(format(rounded_number, big.mark = ",", scientific = FALSE, nsmall = 2))
  }
}


# Function to generate the census data table footer with a light grey background
generateCensusTableFooter <- function() {
  div(
    style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-top: 20px;",
    "This data is sourced from the ",
    tags$a(href = "https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/",
           "Scottish Agricultural Census: June 2023"),
    " which was published on 31 October 2023.",
    tags$br(),
    "Where data is unavailable, findings have been suppressed to prevent disclosure of individual holdings.",
    tags$br(),
    "Full data tables and detailed analysis are available within the full report."
  )
}

generateEmissionsTableFooter <- function() {
  div(
    style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-top: 20px;",
    "This data is sourced from the ",
    tags$a(href = "https://www.gov.scot/publications/scottish-agriculture-greenhouse-gas-emissions-and-nitrogen-use-2022-23/",
           "Scottish agriculture greenhouse gas emissions and nitrogen use 2022-23"),
    " which was published on 27 June 2024, with its analysis based on the ",
    tags$a(href = "https://www.gov.scot/publications/scottish-greenhouse-gas-statistics-2022/", 
           "Scottish Greenhouse Gas Statistics 2022."),
    tags$br(),
    "Full data tables and detailed analysis are available within the full reports."
  )
}

generate2023ModuleTableFooter <- function() {
  div(
    style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin-top: 20px;",
    "This data is sourced from the ",
    tags$a(href = "https://www.gov.scot/publications/results-from-the-scottish-agricultural-census-module-june-2023/",
           "Scottish Agricultural Census: Module June 2023"),
    " which was published on 23 May 2024.",
    tags$br(),
    "This module provides insights into soil cover, tillage, irrigation, nutrient management, and fertiliser application and storage. The report highlights the significant role these practices play in reducing emissions and improving soil and nutrient management across Scottish agricultural holdings.",
    tags$br(),
    "Where data is unavailable, findings have been suppressed to prevent disclosure of individual holdings.",
    tags$br(),
    "Full data tables and detailed analysis are available within the full report."
  )
}




