# File: server.R

# Source the necessary modules for server logic
source("module_subsector_emissions.R")
source("module_total_emissions.R")
source("module_gas_emissions.R")
source("module_manure_usage.R")
source("module_map.R")
source("module_employees.R")
source("module_legal_responsibility.R")
source("module_land_use_summary.R")  # Added new module
source("module_farm_types.R")  # Added new module
source("module_occupiers.R")  # Added new module

server <- function(input, output, session) {
  subsectorEmissionsServer("subsector")
  totalEmissionsServer("total")
  gasEmissionsServer("gas")
  manureUsageServer("manure")
  employeesMapServer("employees")
  landUseSummaryServer("land_use")  # Added new module server
  legalResponsibilityServer("legal_responsibility")  # Added new module server
  farmTypesServer("farm_types")  # Added new module server
  occupiersServer("occupiers")  # Added new module server
}
