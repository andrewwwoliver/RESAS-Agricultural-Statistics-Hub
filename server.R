# Source the necessary modules for server logic
source("module_subsector_emissions.R")
source("module_total_emissions.R")
source("module_gas_emissions.R")
source("module_manure_usage.R")
source("module_map.R")
source("module_employees.R")
source("module_legal_responsibility.R")
source("module_land_use_summary.R")
source("module_farm_types.R")
source("module_occupiers.R")
source("module_owned_land.R")
source("module_cattle.R")
source("module_sheep.R")
source("module_pigs.R")
source("module_poultry.R")
source("module_other_animals.R")


server <- function(input, output, session) {
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$page)) {
      updateTabsetPanel(session, "navbar", selected = query$page)
    }
  })
  
  observeEvent(input$navbar, {
    updateQueryString(paste0("?page=", input$navbar), mode = "push")
  })
  
  subsectorEmissionsServer("subsector")
  totalEmissionsServer("total")
  gasEmissionsServer("gas")
  manureUsageServer("manure")
  employeesMapServer("employees")
  landUseSummaryServer("land_use")
  legalResponsibilityServer("legal_responsibility")
  farmTypesServer("farm_types")
  occupiersServer("occupiers")
  ownedLandServer("owned_land")
  cattleServer("cattle_module")
  sheepServer("sheep_module")
  pigsServer("pigs_module")
  poultryServer("poultry_module")
  otherAnimalsServer("other_animals_module")
  cerealsServer("cereals_module")
  oilseedServer("oilseed_module")
  potatoesServer("potatoes_module")
  beansServer("beans_module")
  stockfeedingServer("stockfeeding_module")
  humanVegetablesServer("human_vegetables_module")
  fruitServer("fruit_module")
  homeServer("home")
}
