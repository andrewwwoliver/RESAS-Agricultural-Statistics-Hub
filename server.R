# server.R
# Source the necessary modules for server logic
source("module_subsector_emissions.R")
source("module_total_emissions.R")
source("module_gas_emissions.R")
source("module_fertiliser_usage.R")
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
source("module_cereals.R")
source("module_oilseed.R")
source("module_potatoes.R")
source("module_beans.R")
source("module_stockfeeding.R")
source("module_human_vegetables.R")
source("module_fruit.R")
source("home.R")
source("module_soil_testing.R")
source("module_manure.R")

server <- function(input, output, session) {
  # Maintain selected tab state based on URL query parameters
  observe({
    query <- parseQueryString(session$clientData$url_search)
    page <- query$page
    if (!is.null(page)) {
      updateTabsetPanel(session, "navbar", selected = page)
    }
  })
  
  observeEvent(input$navbar, {
    query <- parseQueryString(session$clientData$url_search)
    page <- query$page
    if (is.null(page) || input$navbar != page) {
      if (input$navbar != "home") {
        updateQueryString(paste0("?page=", input$navbar), mode = "push")
      }
    }
  })
  
  
  
  subsectorEmissionsServer("subsector")
  totalEmissionsServer("total")
  gasEmissionsServer("gas")
  fertiliserUsageServer("fertiliser")
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
  animalsSummaryServer("animals_summary_module")
  soilTestingServer("soil")
  manureServer("manure")
}
