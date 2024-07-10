# File: ui.R

# Source UI modules
source("options.R")
source("module_line_chart.R")
source("module_area_chart.R")
source("module_bar_chart.R")
source("module_timelapse_bar_chart.R")
source("module_data_table.R")
source("module_total_emissions.R")
source("module_subsector_emissions.R")
source("module_gas_emissions.R")
source("module_summary.R")
source("module_information.R")
source("module_manure_usage.R")
source("module_map.R")
source("module_occupiers_region.R")
source("module_land_use_summary.R")  # Added new module
source("hc_theme.R")
library(shinyjs)

create_footer <- function() {
  div(
    class = "footer",
    span("Last Updated: ", format(Sys.Date(), "%d/%m/%Y")),
    img(src = "sg.png", alt = "SG Logo", style = "height: 30px; margin-left: 10px;")
  )
}

# Generate the UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  theme = shinytheme("flatly"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    includeHTML("google-analytics.html")
  ),
  div(class = "container-fluid full-height",
      div(class = "content",
          navbarPage(
            title = div(
              div("Home", style = "display: inline-block; margin-right: 20px;"),
              actionLink("toggleSidebar", icon("bars"), class = "nav-link", style = "display: inline-block; vertical-align: middle;"),
              tags$li(class = "nav-item", img(src = "RESAS Logo.png", class = "header-logo"))
            ),
            id = "navbar",
            navbarMenu("Emissions",
                       tabPanel("Agriculture Emissions", value = "subsector", subsectorEmissionsUI("subsector")),
                       tabPanel("Industry Emissions", value = "total", totalEmissionsUI("total")),
                       tabPanel("Gas Emissions", value = "gas", gasEmissionsUI("gas")),
                       tabPanel("Manure Usage", value = "manure", manureUsageUI("manure")),
                       tabPanel("Further Information", value = "info", informationUI("info"))
            ),
            navbarMenu("Structure",
                       tabPanel("Occupiers by Region", value = "occupiers", mapUI("occupiers")),
                       tabPanel("Land Use", value = "land_use", landUseSummaryUI("land_use"))  # Added new page
            )
          ),
          create_footer()
      )
  ))
