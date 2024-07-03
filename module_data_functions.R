#module_data_functions.R



# Function to handle variable select and button events
handle_variable_select_and_buttons <- function(id_prefix, variables, input, output, session) {
  selected_variables <- reactive({ vars <- variables(); vars[vars != "Total"] })
  
  output[[paste0("variable_select_", id_prefix)]] <- renderUI({
    checkboxGroupInput(paste0("variables_", id_prefix), "Choose variables to add to chart", choices = variables(), selected = selected_variables())
  })
  
  observeEvent(input[[paste0("select_all_button_", id_prefix)]], {
    updateCheckboxGroupInput(session, paste0("variables_", id_prefix), selected = variables())
  })
  
  observeEvent(input[[paste0("deselect_all_button_", id_prefix)]], {
    updateCheckboxGroupInput(session, paste0("variables_", id_prefix), selected = character(0))
  })
}
