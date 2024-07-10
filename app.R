# app.R

# Source other elements
source("module_line_chart.R")
source("module_area_chart.R")
source("module_data_table.R")
source("options.R")
source("module_summary.R")
source("module_map.R")
source("module_occupiers_region.R")  # Added new module

# Source the UI and server components
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)
