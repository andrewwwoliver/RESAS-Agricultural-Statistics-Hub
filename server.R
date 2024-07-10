# File: server.R

# Source the necessary modules for server logic
source("module_subsector_emissions.R")
source("module_total_emissions.R")
source("module_gas_emissions.R")
source("module_manure_usage.R")
source("module_map.R")
source("module_occupiers_region.R")
source("module_land_use_summary.R")  # Added new module

server <- function(input, output, session) {
  subsectorEmissionsServer("subsector")
  totalEmissionsServer("total")
  gasEmissionsServer("gas")
  manureUsageServer("manure")
  mapServer("occupiers")
  landUseSummaryServer("land_use")  # Added new module server
}
# server.R

# Source the necessary modules for server logic
source("module_subsector_emissions.R")
source("module_total_emissions.R")
source("module_gas_emissions.R")
source("module_manure_usage.R")
source("module_map.R")
source("module_occupiers_region.R")
source("module_land_use_summary.R")
source("module_legal_responsibility.R")  # Add new module server

server <- function(input, output, session) {
  subsectorEmissionsServer("subsector")
  totalEmissionsServer("total")
  gasEmissionsServer("gas")
  manureUsageServer("manure")
  mapServer("occupiers")
  landUseSummaryServer("land_use")
  legalResponsibilityServer("legal")  # Add new module server
}
