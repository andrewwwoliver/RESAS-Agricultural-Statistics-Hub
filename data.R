## data.R

library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)



# Load  data from the Excel file
file_path <- "ghg_data.xlsx"
agri_gas <- read_excel(file_path, sheet = "agri_gas")
national_total <- read_excel(file_path, sheet = "national_total")
subsector_total <- read_excel(file_path, sheet = "subsector_total")
subsector_source <- read_excel(file_path, sheet = "subsector_source")

agri_gas <- agri_gas %>% 
  rename(Gas = ...1)

# Reshape the data to long format
agri_gas <- agri_gas %>% pivot_longer(cols = -Gas, names_to = "Year", values_to = "Value")
national_total <- national_total %>% pivot_longer(cols = -Industry, names_to = "Year", values_to = "Value")
subsector_total <- subsector_total %>% pivot_longer(cols = -Subsector, names_to = "Year", values_to = "Value")

# Convert Year to numeric
agri_gas$Year <- as.numeric(agri_gas$Year)
national_total$Year <- as.numeric(national_total$Year)
subsector_total$Year <- as.numeric(subsector_total$Year)


save(subsector_total, agri_gas, national_total, subsector_source, file = "ghg_data.RData")

# load data
load("ghg_data.RData")



# File: load_tables.R

library(readxl)
library(dplyr)
library(stringr)

# Define file path
file_path <- "June+Agricultural+Census+2023+Tables.xlsx"

# Define the simplified table names and corresponding sheet names
table_names <- c(
  "agricultural_area_hectares" = "Table_1",
  "vegetables_bulbs_fruit_area" = "Table_2",
  "number_of_cattle" = "Table_3 ",
  "number_of_sheep" = "Table_4 ",
  "number_of_pigs" = "Table_5",
  "number_of_poultry" = "Table_6",
  "number_of_other_livestock" = "Table_7",
  "occupiers_employees" = "Table_8",
  "occupiers_age_gender" = "Table_9",
  "legal_responsibility" = "Table_10",
  "owned_rented_land" = "Table_11",
  "farm_type" = "Table_12",
  "agricultural_area_lfa" = "Table_13",
  "holdings_crops_grass_subregion" = "Table_14",
  "crops_grass_area_subregion" = "Table_15",
  "holdings_livestock_region_subregion" = "Table_16",
  "livestock_subregion" = "Table_17",
  "holdings_occupiers_employees_subregion" = "Table_18",
  "occupiers_employees_subregion" = "Table_19",
  "arable_rotation" = "Table_20",
  "manure_fertiliser" = "Table_21"
)

# Function to remove columns and rows that are all NAs
clean_data <- function(data) {
  data <- data[, colSums(is.na(data)) < nrow(data)]  # Remove columns with all NAs
  data <- data[rowSums(is.na(data)) < ncol(data), ]  # Remove rows with all NAs
  return(data)
}

# Function to clean header names by removing text within brackets, any '\r\n', extra spaces, and specific region names
clean_headers <- function(headers) {
  headers <- str_replace_all(headers, "\\s*\\([^\\)]+\\)", "")  # Remove text within brackets
  headers <- str_replace_all(headers, "\r\n", " ")  # Remove \r\n
  headers <- str_replace_all(headers, "\\s+", " ")  # Replace multiple spaces with a single space
  headers <- str_replace_all(headers, "\\b(North West|North East|South East|South West)\\b", "")  # Remove specific region names
  headers <- str_trim(headers)  # Trim again to remove any resulting leading or trailing spaces
  return(headers)
}

# Function to clean cell values by replacing multiple spaces with a single space
clean_cells <- function(data) {
  data <- data %>% mutate(across(where(is.character), ~str_replace_all(.x, "\\s+", " ")))
  return(data)
}

# Read each table, clean it, and assign to a variable
for (table in names(table_names)) {
  sheet_name <- table_names[table]
  data <- read_excel(file_path, sheet = sheet_name)
  
  # Clean headers
  headers <- names(data)
  cleaned_headers <- clean_headers(headers)
  
  # Remove columns with '5 year' in the header (case insensitive)
  columns_to_remove <- grep("5 year", cleaned_headers, ignore.case = TRUE)
  if (length(columns_to_remove) > 0) {
    data <- data[, -columns_to_remove]
    cleaned_headers <- cleaned_headers[-columns_to_remove]
  }
  
  # Assign cleaned headers to the data
  names(data) <- cleaned_headers
  
  # Convert all columns except the first to numeric, setting non-numeric values to NA
  data <- data %>% mutate(across(-1, ~ as.numeric(as.character(.))))
  
  # Remove '\r\n' from all character columns and clean multiple spaces in the first column
  data[[1]] <- str_replace_all(data[[1]], "\r\n", " ")
  data <- clean_cells(data)
  
  # Clean data
  cleaned_data <- clean_data(data)
  
  assign(table, cleaned_data)
}


# Save all tables to an RData file
save(list = names(table_names), file = "census_data.RData")

load("census_data.RData")


library(sf)
library(dplyr)
library(highcharter)
library(geojsonio)
library(rmapshaper)

# Load the shapefile
local_authorities <- st_read("pub_las.shp")

mapping_data <- data.frame(
  region = c("North West", "North West", "North West", "North West", 
             "North East", "North East", "North East", 
             "South East", "South East", "South East", "South East", "South East", 
             "South East", "South East", "South East", "South East",
             "South West", "South West", "South West", "South West", "South West", 
             "South West", "South West", "South West", "South West", "South West", 
             "South West", "South West", "South West", "South West", "South West", "South West"
             ),
  sub_region = c("Shetland", "Orkney", "Na h-Eileanan Siar", "Highland", 
                 "Grampian", "Grampian", "Grampian", 
                 "Tayside", "Tayside", "Tayside", "Fife", "Lothian", "Lothian", "Lothian", "Lothian", 
                 "Scottish Borders", "East Central", "East Central", "East Central", 
                 "Argyll & Bute", "Clyde Valley", "Clyde Valley", "Clyde Valley", "Clyde Valley", 
                 "Clyde Valley", "Clyde Valley", "Clyde Valley", "Clyde Valley", "Ayrshire", "Ayrshire", "Ayrshire", 
                 "Dumfries & Galloway"),
  local_authority = c("Shetland Islands", "Orkney Islands", "Na h-Eileanan an Iar", "Highland", 
                      "Aberdeen City", "Aberdeenshire", "Moray", 
                      "Angus", "Dundee City", "Perth and Kinross", "Fife", "East Lothian", "City of Edinburgh", 
                      "Midlothian", "West Lothian", "Scottish Borders", 
                      "Clackmannanshire", "Falkirk", "Stirling", 
                      "Argyll and Bute", "East Dunbartonshire", "East Renfrewshire", "Glasgow City", 
                      "Inverclyde", "North Lanarkshire", "Renfrewshire", "South Lanarkshire", 
                      "West Dunbartonshire", "East Ayrshire", "North Ayrshire", "South Ayrshire", 
                      "Dumfries and Galloway")
)

# Merge the shapefile with the mapping data
local_authorities <- local_authorities %>%
  left_join(mapping_data, by = c("local_auth" = "local_authority"))

# Ensure geometries are valid
local_authorities <- st_make_valid(local_authorities)

sub_regions <- local_authorities %>%
  group_by(sub_region) %>%
  summarise(geometry = st_union(geometry))

st_write(sub_regions, "sub_regions.geojson", driver = "GeoJSON")

# Load the GeoJSON file
geojson_data <- geojson_read("sub_regions.geojson", what = "sp")

geojson_data <- ms_simplify(geojson_data, keep = 0.001)

geojson_write(geojson_data, file = "subregions_simplified.geojson")



#crops / fruit/veg
# Print unique values 

unique(agricultural_area_hectares$`Crop/Land use`)

unique(vegetables_bulbs_fruit_area$`Vegetables and fruits for human consumption`)

unique(crops_grass_area_subregion$`Land use by category`)


names(agricultural_area_hectares) <- names(agricultural_area_hectares) %>%
  str_replace_all("Area", "") %>%
  str_trim

names(vegetables_bulbs_fruit_area) <- names(vegetables_bulbs_fruit_area) %>%
  str_replace_all("Area", "") %>%
  str_trim

#summary crops data
crops_summary_data <- agricultural_area_hectares %>%
  filter(`Crop/Land use` %in% c("Total combine harvested crops", "Total crops for stockfeeding",
                                "Vegetables for human consumption", "Soft fruit"))

land_use_data <- agricultural_area_hectares %>% 
  filter(`Crop/Land use` %in% c("Total crops, fallow, and set-aside", "Total grass", 
                                "Rough grazing", "Total sole right agricultural area", "Common grazings"))


land_use_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c("Total agricultural area", "Total grass and rough grazing", 
                                "Sole right grazing", "Total crops and fallow", "Common grazing", "Woodland"))


# Subset for cereals data
cereals_data <- agricultural_area_hectares %>%
  filter(`Crop/Land use` %in% c("Wheat", "Triticale", "Winter barley", "Spring barley", "Barley Total", 
                                "Winter oats", "Spring oats", "Oats Total", "Rye", "Mixed grain", 
                                "Total cereals"))

# Subset for oilseeds data
oilseed_data <- agricultural_area_hectares %>%
  filter(`Crop/Land use` %in% c("Winter oilseed rape", "Spring oilseed rape", "Linseed", "Total oilseeds"))

# Subset for potatoes data
potatoes_data <- agricultural_area_hectares %>%
  filter(`Crop/Land use` %in% c("Seed potatoes", "Ware potatoes", "Total potatoes"))

# Subset for beans data
beans_data <- agricultural_area_hectares %>%
  filter(`Crop/Land use` %in% c("Protein peas", "Field beans"))

# Subset for animal feed data
stockfeeding_data <- agricultural_area_hectares %>%
  filter(`Crop/Land use` %in% c("Turnips/swedes", "Kale/cabbage", "Maize", "Rape", "Fodder beet", 
                                "Lupins", "Other crops for stockfeeding", "Total crops for stockfeeding"))

# Subset for human vegetables data
human_vegetables_data <- vegetables_bulbs_fruit_area %>%
  filter(`Vegetables and fruits for human consumption` %in% c(
    "Peas for canning, freezing or drying",
    "Beans for canning, freezing or drying",
    "Turnips/swedes",
    "Calabrese",
    "Cauliflower",
    "Carrots",
    "Other vegetables",
    "Total vegetables"
  ))

# Subset for soft fruit data
fruit_data <- vegetables_bulbs_fruit_area %>%
  filter(`Vegetables and fruits for human consumption` %in% c(
    "Strawberries grown in the open",
    "Raspberries grown in the open",
    "Blueberries grown in the open",
    "Blackcurrants and other fruit grown in the open",
    "Total soft fruit grown in the open",
    "Tomatoes grown under cover",
    "Strawberries grown under cover",
    "Raspberries grown under cover",
    "Blueberries grown under cover",
    "Other fruit grown under cover",
    "Vegetables grown under cover",
    "Strawberries grown in open/under cover",
    "Raspberries grown in open/under cover",
    "Blackcurrants grown in open/under cover",
    "Blueberries grown in open/under cover",
    "Tomatoes grown in open/under cover",
    "Other fruit grown in open/under cover",
    "Total soft fruit"
  ))
# Subset for cereals_subregion
cereals_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Wheat",
    "Winter Barley",
    "Spring Barley",
    "Barley Total",
    "Oats, triticale and mixed grain"
  ))

# Subset for oilseed_subregion
oilseed_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Rape for oilseed and linseed"
  ))

# Subset for potato_subregion
potatoes_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Potatoes"
  ))

# Subset for beans_subregion
beans_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Peas and beans for combining"
  ))

# Subset for stockfeeding_subregion
stockfeeding_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Stockfeeding crops"
  ))

# Subset for human_veg_subregion
human_vegetables_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Vegetables for human consumption"
  ))

# Subset for fruit_subregion
fruit_subregion <- crops_grass_area_subregion %>%
  filter(`Land use by category` %in% c(
    "Orchard and soft fruit"
  ))

# Saving all the subsets to an RData file
save(
  crops_summary_data,
  land_use_data,
  land_use_subregion,
  cereals_data,
  oilseed_data,
  potatoes_data,
  beans_data,
  stockfeeding_data,
  human_vegetables_data,
  fruit_data,
  cereals_subregion,
  oilseed_subregion,
  potatoes_subregion,
  beans_subregion,
  stockfeeding_subregion,
  human_vegetables_subregion,
  fruit_subregion,
  file = "crops_data.RData"
)
