# File: server.R

# Source the necessary modules for server logic
source("module_subsector_emissions.R")
source("module_total_emissions.R")
source("module_gas_emissions.R")
source("module_manure_usage.R")

server <- function(input, output, session) {
  subsectorEmissionsServer("subsector")
  totalEmissionsServer("total")
  gasEmissionsServer("gas")
  manureUsageServer("manure")
}
