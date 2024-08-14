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

#census data -----------------------------
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

# Function to apply rounding and drop only trailing zeros from decimals
round_values <- function(x) {
  sapply(x, function(value) {
    if (is.na(value)) return(NA)  # Handle NA values
    
    # Apply rounding based on the specified conditions
    if (value > 10000) {
      result <- round(value, 0)
    } else if (value > 1000) {
      result <- round(value, 1)
    } else {
      result <- value
    }
    
    # Convert to character and drop trailing zeros from decimal numbers only
    result <- as.character(result)
    
    if (grepl("\\.", result)) {  # Check if the result contains a decimal point
      result <- sub("\\.?0+$", "", result)  # Remove trailing zeros after decimal point
    }
    
    return(result)
  })
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
  
  # Apply rounding and drop trailing zeros to numeric columns (except the first)
  data <- data %>% mutate(across(-1, ~ round_values(.)))
  
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







# map data

library(sf)
library(dplyr)
library(highcharter)
library(geojsonio)
library(rmapshaper)

# Load the shapefile
local_authorities <- st_read("Local_Authority_Boundaries_-_Scotland/pub_las.shp")

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


regions <- local_authorities %>%
  group_by(region) %>%
  summarise(geometry = st_union(geometry))

st_write(regions, "regions.geojson", driver = "GeoJSON")

Sys.setenv(OGR_GEOJSON_MAX_OBJ_SIZE = "0")

regions_geojson <- geojson_read("regions.geojson", what = "sp")

regions_geojson <- ms_simplify(regions_geojson, keep = 0.001)

geojson_write(regions_geojson, file = "regions_simplified.geojson")

##############----------------------###################


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

# Preprocess and round the summary crops data
crops_summary_data <- agricultural_area_hectares %>%
  filter(`Crop/Land use` %in% c("Total combine harvested crops", "Total crops for stockfeeding",
                                "Vegetables for human consumption", "Soft fruit")) %>%
  pivot_longer(
    cols = -`Crop/Land use`,
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year = as.numeric(Year),  # Ensure Year is numeric
    Value = if_else(
      abs(as.numeric(Value)) >= 1000,  # Round up for numbers with more than 3 significant figures
      round(as.numeric(Value)),
      round(as.numeric(Value), digits = 2)
    )
  )

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




# Convert the wide format data into long format using pivot_longer
number_of_pigs_long <- number_of_pigs %>%
  pivot_longer(cols = -`Pigs by category`, names_to = "Year", values_to = "Total") %>%
  filter(`Pigs by category` == "Total pigs") %>%
  select(Year, `Total pigs` = Total)

number_of_poultry_long <- number_of_poultry %>%
  pivot_longer(cols = -`Poultry by category`, names_to = "Year", values_to = "Total") %>%
  filter(`Poultry by category` == "Total poultry") %>%
  select(Year, `Total poultry` = Total)

number_of_sheep_long <- number_of_sheep %>%
  pivot_longer(cols = -`Sheep by category`, names_to = "Year", values_to = "Total") %>%
  filter(`Sheep by category` == "Total sheep") %>%
  select(Year, `Total sheep` = Total)

number_of_cattle_long <- number_of_cattle %>%
  pivot_longer(cols = -`Cattle by category`, names_to = "Year", values_to = "Total") %>%
  filter(`Cattle by category` == "Total Cattle") %>%
  select(Year, `Total cattle` = Total)

# Merge the dataframes on the 'Year' column
total_animals <- number_of_pigs_long %>%
  inner_join(number_of_poultry_long, by = "Year") %>%
  inner_join(number_of_sheep_long, by = "Year") %>%
  inner_join(number_of_cattle_long, by = "Year")

# Convert Year column to numeric
total_animals$Year <- as.numeric(total_animals$Year)

# save total animals 
save(total_animals, file = "total_animals.RData")



# module 2023 data
# Load necessary libraries
library(readxl)
library(stringr)
library(dplyr)

# Define the file path
file_path <- "June+Agricultural+Census+2023+-+Module+Report+-+Production+methods+and+nutrient+application+-+Tables.xlsx"

# Define the sheet names to read
sheets_to_read <- c("Table_4", "Table_5", "Table_7", "Table_8", "Table_9", "Table_12")

# Define shortened names for the dataframes
short_names <- c("soil_nutrient_mgmt", "grass_crop_nutrient_mgmt", "nitrogen_250", 
                 "nitrogen_400", "manure_qty", "fertiliser_use")

# Function to clean header names by removing text within brackets, any '\r\n', extra spaces, and specific region names
clean_headers <- function(headers) {
  headers <- str_replace_all(headers, "\\s*\\([^\\)]+\\)", "")  # Remove text within brackets
  headers <- str_replace_all(headers, "\r\n", " ")  # Remove \r\n
  headers <- str_replace_all(headers, "\\s+", " ")  # Replace multiple spaces with a single space
  headers <- str_replace_all(headers, "\\b(North West|North East|South East|South West)\\b", "")  # Remove specific region names
  headers <- str_trim(headers)  # Trim again to remove any resulting leading or trailing spaces
  return(headers)
}

# Function to read and process each sheet
read_and_process_sheet <- function(sheet) {
  df <- read_excel(file_path, sheet = sheet, skip = 5)  # Skip the first 5 rows
  colnames(df) <- clean_headers(colnames(df))  # Clean the headers
  return(df)
}

# Function to clean and convert all columns except the first to numeric
clean_and_convert_numeric <- function(df) {
  df[] <- lapply(seq_along(df), function(i) {
    if (i == 1) {
      return(df[[i]])  # Keep the first column as is
    } else {
      # Remove commas, extra spaces, and convert to numeric for other columns
      return(as.numeric(gsub(",", "", df[[i]])))
    }
  })
  return(df)
}

# Function to determine the number of significant figures
signif_figures <- function(x) {
  if (x == 0) return(1)  # Edge case for 0
  return(floor(log10(abs(x))) + 1)
}

# Function to round numbers based on significant figures
custom_round <- function(x) {
  if (is.na(x)) return(NA)
  sf <- signif_figures(x)
  
  if (sf > 5) {
    return(round(x, 0))  # Round to 0 decimal places for 6+ significant figures
  } else if (sf == 5) {
    return(round(x, 0))  # Round to 0 decimal places for 5 significant figures
  } else if (sf == 4) {
    return(round(x, 1))  # Round to 1 decimal place for 4 significant figures
  } else if (sf == 3) {
    return(round(x, 2))  # Round to 2 decimal places for 3 significant figures
  } else {
    return(round(x, 2))  # Round to 2 decimal places for 2 or fewer significant figures
  }
}

# Updated function to apply custom rounding logic to all numeric columns in a dataframe
round_df <- function(df) {
  df <- clean_and_convert_numeric(df)  # Clean and convert to numeric first
  df[] <- lapply(df, function(x) if (is.numeric(x)) sapply(x, custom_round) else x)
  return(df)
}


# Read and process the specified sheets into a list of dataframes
data_frames <- lapply(sheets_to_read, function(sheet) {
  df <- read_and_process_sheet(sheet)
  df <- round_df(df)  # Apply rounding after conversion
  return(df)
})

# Name the list elements with shortened names
names(data_frames) <- short_names

# Remove the 'Area' column from 'grass_crop_nutrient_mgmt' if it exists
if("Area" %in% colnames(data_frames$grass_crop_nutrient_mgmt)) {
  data_frames$grass_crop_nutrient_mgmt <- select(data_frames$grass_crop_nutrient_mgmt, -Area)
}

# Ensure specific columns are numeric in both dataframes
convert_columns_to_numeric <- function(df, columns) {
  for (col in columns) {
    df[[col]] <- as.numeric(as.character(df[[col]]))
  }
  return(df)
}

numeric_columns <- c("Percentage of holdings", "Average holding area")

data_frames$soil_nutrient_mgmt <- convert_columns_to_numeric(data_frames$soil_nutrient_mgmt, numeric_columns)
data_frames$grass_crop_nutrient_mgmt <- convert_columns_to_numeric(data_frames$grass_crop_nutrient_mgmt, numeric_columns)

# Join 'soil_nutrient_mgmt' and 'grass_crop_nutrient_mgmt' data frames
combined_nutrient_mgmt <- bind_rows(data_frames$soil_nutrient_mgmt, data_frames$grass_crop_nutrient_mgmt)

# Remove specified rows from the 'Soil nutrient management' column in the combined data frame
remove_entries <- c("Holdings with grassland", "Cropland holdings", "Holdings with grass or crops", 
                    "Soil testing resulted in change of crop nutrient application (those that performed soil testing)", 
                    "Uses protected urea")

combined_nutrient_mgmt <- combined_nutrient_mgmt %>% 
  filter(!`Soil nutrient management` %in% remove_entries)

# Assign the combined data frame back to the list
data_frames$combined_nutrient_mgmt <- combined_nutrient_mgmt

# Remove the original separate data frames
data_frames$soil_nutrient_mgmt <- NULL
data_frames$grass_crop_nutrient_mgmt <- NULL

# Save the data frames as a .RData file
save(list = names(data_frames), file = "module_2023.RData", envir = list2env(data_frames))
