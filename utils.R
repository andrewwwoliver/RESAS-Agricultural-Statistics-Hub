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



