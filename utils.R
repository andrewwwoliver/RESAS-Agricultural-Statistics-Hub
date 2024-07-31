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
