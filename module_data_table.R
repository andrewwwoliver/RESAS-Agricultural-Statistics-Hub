# module_data_table.R

library(DT)

# Function to render data tables using DT::renderDT
render_data_table <- function(id, chart_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$data_table <- DT::renderDT({
      data <- chart_data()
      data$Year <- as.character(data$Year)
      first_column_name <- names(data)[1]
      data <- pivot_wider(data, names_from = !!sym(first_column_name), values_from = Value)
      data <- data %>% mutate(across(where(is.numeric), ~ formatC(.x, format = "f", digits = 2)))
      datatable(as_tibble(data), options = list(paging = FALSE, scrollX = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), rownames = FALSE)
    })
  })
}

# Function to handle data download
handle_data_download <- function(id, chart_data, filename_prefix) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$download_data <- downloadHandler(
      filename = function() { paste0(filename_prefix, "_data.csv") },
      content = function(file) { 
        data <- chart_data()
        first_column_name <- names(data)[1]
        data <- pivot_wider(data, names_from = !!sym(first_column_name), values_from = Value)
        data <- data %>% mutate(across(where(is.numeric), ~ formatC(.x, format = "f", digits = 5)))
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
}
