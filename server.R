# server.R

# Source the necessary modules for server logic

source("module_timelapse_bar_chart.R")
source("module_data_table.R")
source("module_summary.R")

server <- function(input, output, session) {
  totalEmissionsServer("total")
  subsectorEmissionsServer("subsector")
  gasEmissionsServer("gas")
}
