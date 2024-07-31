# Source other elements
source("module_line_chart.R")
source("module_area_chart.R")
source("module_data_table.R")
source("options.R")
source("module_summary.R")
source("module_employees.R")
source("module_legal_responsibility.R")
source("module_manure_usage.R")
source("module_farm_types.R")
source("module_occupiers.R")
source("module_land_use_summary.R")
source("module_owned_land.R")
source("module_cattle.R")  # Added new module

# Source the UI and server components
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)

