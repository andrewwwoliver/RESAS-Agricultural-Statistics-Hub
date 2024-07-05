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


# Could do with being more automated
# Manually removed first x rows of metadata from each table
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
  "businesses_land_area" = "Table_10",
  "owned_rented_land" = "Table_11",
  "holdings_main_farm_type" = "Table_12",
  "agricultural_area_lfa" = "Table_13",
  "holdings_crops_grass_subregion" = "Table_14",
  "crops_grass_area_subregion" = "Table_15",
  "holdings_livestock_region_subregion" = "Table_16",
  "livestock_subregion" = "Table_17",
  "holdings_occupiers_employees_subregion" = "Table_18",
  "occupiers_employees_subregion" = "Table_19",
  "holdings_arable_land_crop_rotation" = "Table_20",
  "manure_fertiliser" = "Table_21"
)

# Function to remove columns and rows that are all NAs
clean_data <- function(data) {
  data <- data[, colSums(is.na(data)) < nrow(data)]  # Remove columns with all NAs
  data <- data[rowSums(is.na(data)) < ncol(data), ]  # Remove rows with all NAs
  return(data)
}

# Function to clean header names by removing text within brackets
clean_headers <- function(headers) {
  str_replace_all(headers, "\\s*\\([^\\)]+\\)", "")
}

# Read each table, clean it, and assign to a variable
for (table in names(table_names)) {
  sheet_name <- table_names[table]
  data <- read_excel(file_path, sheet = sheet_name)
  
  # Clean headers
  headers <- names(data)
  cleaned_headers <- clean_headers(headers)
  
  # Remove columns with '5 year' in the header
  columns_to_remove <- grep("5 year", headers, ignore.case = TRUE)
  if (length(columns_to_remove) > 0) {
    data <- data[, -columns_to_remove]
    cleaned_headers <- cleaned_headers[-columns_to_remove]
  }
  
  names(data) <- cleaned_headers
  
  # Clean data
  cleaned_data <- clean_data(data)
  
  assign(table, cleaned_data)
}


# Save all tables to an RData file
save(list = names(table_names), file = "census_data.RData")

load("census_data.RData")


