#File sorts and loads data for the app. 
#Data include dataframes and text that appear in the sections
library(readxl) 
library(tidyverse)
# categories --------------------------------------------------------------
# 
item <- c("Cereals",
          "Oilseeds",
          "Potatoes",
          "Vegetables for human consumption",
          #"Crops for stockfeeding",
          "Fruit"
          #"Rough grazing",
          #"Common grazing"
          )

total_item <- c(paste("Total", item[1:4]),
                "Total fruit"
                 # "Total crops and grass",
                 # "Total agricultural area")
)
# 
 total_item <- str_to_sentence(total_item)
# 

land_use <- c("Total crops and grass",
              "Other land (including woodland)",
              "Rough grazing",
              "Common grazings",
              "Total agricultural area"
              )




#dairy <- cattle$`Cattle by category`[grep("dairy", cattle$`Cattle by category`, ignore.case=TRUE)]

#beef <- c("beef", "Male", "Calves")
#beef <- cattle$`Cattle by category`[grep(paste(beef,collapse="|"), cattle$`Cattle by category`)]





#data prep----

#crops and structures of Scottish agriculture 
load("./Data/JAC_crops.RData")
#crops <- crops %>% select(-item, -total_item)
#farmtypes
load("./Data/JAC_farm_types.RData")


#livestock-
#cattle
load("./Data/JAC_cattle.RData")
cattle <- cattle %>%   pivot_longer(., cols = !`Cattle by category`,
                                    names_to = "Year",
                                    values_to = "Number of livestock"
)

beef <- read_xlsx("./Data/beef_sector.xlsx")
dairy <- read_xlsx("./Data/dairy_sector.xlsx")
sheep <- read_xlsx("./Data/sheep_sector.xlsx")
#
#crop data
#item <- c(item, total_item)
#read in farm type data
# farm_types <- read_xlsx("~/R/JAC_compendium/SG_Ag_compendium/farm_types.xlsx")
# save(farm_types, file = "~/R/JAC_compendium/SG_Ag_compendium/JAC_farm_types.RData")
# 
# #read in cattle data
# cattle <- read_xlsx("~/R/JAC_compendium/SG_Ag_compendium/cattle.xlsx")
# save(cattle, file = "~/R/JAC_compendium/SG_Ag_compendium/JAC_cattle.RData")
# 
#shape data

# #
# #shape data
# crops <- crops %>%   pivot_longer(., cols = !Crop,
#                                   names_to = "Year",
#                                   values_to = "Area"
# )
# crops$Crop <- as.factor(crops$Crop)
# levels(crops$Crop)
# 
# crops$Year <- as.integer(crops$Year)
# 
# crops <- crops %>% mutate(item = case_when(Crop == "Spring barley"|
#                                              Crop == "Spring oats" |
#                                              Crop == "Winter barley"|
#                                              Crop == "Winter oats"|
#                                              Crop == "Wheat"|
#                                              Crop == "Rye"|
#                                              Crop == "Mixed grain - not on the dataset"|
#                                              Crop == "Triticale" ~ "Cereals",
#                                            Crop == "Winter oilseed rape"|
#                                              Crop == "Spring oilseed rape"|
#                                              Crop == "Linseed" ~ "Oilseeds",
#                                            Crop == "Seed potatoes"|
#                                              Crop == "Ware potatoes" ~ "Potatoes",
#                                            str_detect(Crop, "Stockfeed") ~ "Crops for stockfeeding",
#                                            str_detect(Crop,"canning") |
#                                              Crop == "Turnips/swedes"|
#                                              Crop == "Calabrese" |
#                                              Crop == "Cauliflower" |
#                                              Crop == "Carrots" |
#                                              Crop == "Other vegetables" ~"Vegetables for human consumption",
#                                            str_detect(Crop, "fruit") ~ "Fruit",
#                                            Crop== "Rough grazing" ~ "Rough grazing",
#                                            Crop== "Common grazings" ~ "Common grazing",
#                                            TRUE~ NA_character_))
# #
# #
# crops <- crops %>% mutate(total_item = case_when(Crop == "Total cereals" ~ "Total cereals",
#                                                  Crop == "Total oilseeds" ~ "Total oilseeds",
#                                                  Crop == "Total potatoes" ~ "Total potatoes",
#                                                  Crop == "Vegetables for human consumption" ~ "Total vegetables for human consumption",
#                                                  Crop == "Total fruit" ~ "Total fruit",
#                                                  Crop == "Total agricultural area" ~ "Total agricultural area",
#                                                  Crop == "Total crops and grass" ~ "Total crops and grass",
#                                                  TRUE~ NA_character_))
# 
# #
# #
# #
# #
# #
# #
# 
# 
# 
# #export
# save(crops, file = "C:/Users/z620777/OneDrive - SCOTS Connect/R/JAC_compendium/SG_Ag_compendium/JAC_crops.RData")


#text----
#
#Structure of Scottish Agriculture section
struct_agri_txt1 <- "Use the links on the left to explore our data on:"
struct_agri_txt2 <- "Land use"
struct_agri_txt3 <- "Farm types"
struct_agri_txt4 <- "Labour"

struct_agri_txt5 <- "More data on land use, crop areas, livestock, farm structure and the number of people working on agricultural holdings are available in the "
struct_agri_txt6 <- tags$a(href="https://www.gov.scot/collections/june-scottish-agricultural-census/", target = "_blank", "Scottish Agricultural Census: results.")

struct_agri_txt7 <- "Economic data about Scottish agriculture, including the value of production, costs and support payments are available through "
struct_agri_txt8 <- tags$a(href="https://www.gov.scot/collections/total-income-from-farming/", target = "_blank", "Total income from farming")
struct_agri_txt9 <- ", the "
struct_agri_txt10 <- tags$a(href="https://www.gov.scot/collections/economic-report-on-scottish-agriculture/", target = "_blank", "Scottish agriculture: economic reports")
struct_agri_txt11 <- "and "
struct_agri_txt12 <- tags$a(href="https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/", target = "_blank", "Scottish farm business income estimates")

#Summary tab
land_use_summary_txt1 <- "This map shows the main farming types found in each area. Large areas of Scotland have hilly or rocky land suitable for livestock, but limited growing conditions. These areas are shown in light green on the map. The areas in black have better soil and can support crops usually grown for animal feed. Dark green areas can support vegetables, fruit and cereal farming for human consumption."
land_use_summary_txt2 <- "The total Scottish agricultural area in 2023 was 5.33 million hectares, 69 per cent of Scotland’s total land. However, it should be noted that large areas of agricultural land are only lightly farmed. For example, hilly or mountainous areas are mostly used for rough grazing. The total Scottish agricultural area excludes common grazing land."
land_use_summary_txt3 <- "More information about land use is available in the" 
land_use_summary_txt4 <- tags$a(href="https://www.gov.scot/collections/june-scottish-agricultural-census/", target = "_blank", "Scottish Agricultural Census.")
land_use_summary_txt5 <- "The twin climate change and environment crises mean considerations as to how we own, use and manage our land have never been as important as they are now. Scotland’s land and the natural capital it supports are some of our most valuable assets. It is vital to our environment, economy and wellbeing as individuals and communities. Information about "
land_use_summary_txt6 <- tags$a(href="https://www.gov.scot/policies/landscape-and-outdoor-access/land-use/",  target = "_blank", "land use policy")
land_use_summary_txt7 <- "is available on gov.scot."

#Plot tab
land_use_plot_txt1 <- "The interactive column charts display the different categories of land use across 2012 to 2023."
land_use_plot_txt2 <-"Please note the time series will have a gap for 2022 due to the survey being paused to make improvements to data collection, processing and statistical methodology used for the census data set.  Estimates for 2022 will be published later in 2023."
land_use_plot_txt3 <-"You can choose which time period to display data for by selecting the start and end year on the slider. Data for the plot is available in the \"Data\" tab."

#Data tab
land_use_data_txt1 <- "The data in the table below is displayed in the interactive chart. You can download a .csv file of this data by clicking on the \"Download the data\" link under the table."
land_use_data_txt2 <- "Detailed tables for land use, crop areas, livestock and the number of people working on agricultural holdings are available in the"
#link tot 2023 publication
land_use_data_txt3 <- tags$a(href="https://www.gov.scot/publications/results-scottish-agricultural-census-june-2023/documents/", target = "_blank", "Scottish Agricultural Census detailed tables")

#Farm types summary tab
farm_types_summary_txt1 <- "Farms are classified into farm types based on the relative contribution of their farming activities. A farm is allocated to a farm type where at least two thirds of their Standard Output is associated with that activity. Standard Output is the estimated worth of crops and livestock."
farm_types_summary_txt2 <- "The main farm types reported on in Scotland are:"
farm_types_summary_txt3 <- "cattle and sheep in "
farm_types_summary_txt4 <- tags$a(href="https://data.europa.eu/data/datasets/less-favoured-areas?locale=en", target = "_blank", "Less Favoured Areas (LFA)")
farm_types_summary_txt5 <- "cattle and sheep"
farm_types_summary_txt6 <- "cereals"
farm_types_summary_txt7 <- "dairy"
farm_types_summary_txt8 <- "general cropping; forage"
farm_types_summary_txt9 <- "horticulture and permanent crops"
farm_types_summary_txt10 <- "pigs"
farm_types_summary_txt11 <- "poultry"
farm_types_summary_txt12 <- "mixed"
farm_types_summary_txt13 <- "other"



#Agriculture and the environment
# agri_env_txt1 <- "Estimates for the emissions of greenhouse gases by the agriculutral industry are available" 
# agri_env_txt2 <- tags$a(href="https://www.gov.scot/publications/scottish-greenhouse-gas-statistics-2021/", target = "_blank", "through Scottish Greenhouse Gas Statistics")
# agri_env_txt3 <- "and the"
# agri_env_txt4 <- tags$a(href="https://www.gov.scot/publications/scottish-nitrogen-balance-sheet-2020/", target = "_blank", "Scottish Nitrogen Balance Sheet.")
agri_env_txt5 <- "The following list of resources are reports commissioned or published for the "
agri_env_txt6 <- tags$a(href="https://www.gov.scot/policies/agriculture-and-the-environment/farmer-led-climate-change-groups/", target = "_blank", "Farmer-led Groups:")
agri_env_txt7 <- "Agricultural Reform Programme"
agri_env_txt8 <-tags$a(href=" https://www.gov.scot/publications/evidence-support-development-new-rural-support-scheme-scotland-summary-written-outputs/", target = "_blank", "Evidence to support the development of a new rural support scheme for Scotland")
agri_env_txt9 <- "Climate change evidence reports"
agri_env_txt10 <-tags$a(href=" https://www.gov.scot/publications/resas-climate-change-evidence-dairy-farmer-led-group/", target = "_blank", "Dairy Farmer-led Group: climate change evidence")
agri_env_txt11 <-tags$a(href=" https://www.gov.scot/publications/resas-climate-change-evidence-arable-farmer-led-group/", target = "_blank", "Arable Farmer-led Group: climate change evidence")
agri_env_txt12 <-tags$a(href=" https://www.gov.scot/publications/resas-climate-change-evidence-huc-farmer-led-group/", target = "_blank", "Hill, Upland and Crofting Farmer-led Group: climate change evidence")
agri_env_txt13 <-tags$a(href=" https://www.gov.scot/publications/pig-sector-flg-climate-change-greenhouse-gas-evidence/", target = "_blank", "Pig Sector Farmer-Led Climate Change Group: climate change and greenhouse gas evidence")
agri_env_txt14 <- "Greenhouse Gas Inventory reports"
agri_env_txt15 <-tags$a(href="https://www.gov.scot/publications/estimated-dairy-emissions-mitigation-smart-inventory/", target = "_blank", "Greenhouse gas inventory: estimated dairy emissions and their mitigation")
agri_env_txt16 <-tags$a(href="https://www.gov.scot/publications/estimated-arable-emissions-mitigation-smart-inventory/", target = "_blank", "Greenhouse gas inventory: estimated arable emissions and their mitigation")
agri_env_txt17 <-tags$a(href="https://www.gov.scot/publications/disaggregating-headline-smart-inventory-figures/", target = "_blank", "Greenhouse gas emissions - agricultural: disaggregating headline figures")
agri_env_txt18 <-"Suckler Beef Climate Scheme research papers"
agri_env_txt19 <-tags$a(href="https://www.gov.scot/binaries/content/documents/govscot/publications/factsheet/2021/01/suckler-beef-climate-scheme-research-papers/documents/suckler-beef-climate-scheme---estimated-effects-of-sbcs-within-the-national-ghg-smart-inventory/suckler-beef-climate-scheme---estimated-effects-of-sbcs-within-the-national-ghg-smart-inventory/govscot%3Adocument/estimated-suckler-beef-climate-scheme-effects-within-national-ghg-smart-inventory.pdf", target = "_blank", "Estimated Suckler Beef Climate Scheme effects within the National GHG 'Smart' Inventory")
agri_env_txt20 <-tags$a(href="https://www.gov.scot/binaries/content/documents/govscot/publications/factsheet/2021/01/suckler-beef-climate-scheme-research-papers/documents/sruc-report-suckler-beef-climate-scheme-metrics/sruc-report-suckler-beef-climate-scheme-metrics/govscot%3Adocument/SRUC%2BReport%2B-%2BSuckler%2BBeef%2BClimate%2BScheme%2BMetrics%2BFINAL.pdf", target = "_blank", "Suckler Beef Climate Scheme: Draft metrics")
agri_env_txt21 <-tags$a(href="https://www.gov.scot/binaries/content/documents/govscot/publications/factsheet/2021/01/suckler-beef-climate-scheme-research-papers/documents/sruc-report-suckler-beef-climate-scheme---broader-issues/sruc-report-suckler-beef-climate-scheme---broader-issues/govscot%3Adocument/SRUC%2BReport%2B-%2BSuckler%2BBeef%2BClimate%2BScheme-%2BBroader%2Bissues%2BFINAL.pdf", target = "_blank", "Suckler Beef Climate Scheme: Broader issues")
agri_env_txt22 <-tags$a(href="https://www.gov.scot/binaries/content/documents/govscot/publications/factsheet/2021/01/suckler-beef-climate-scheme-research-papers/documents/sruc-report-structure-and-efficiency-of-the-scottish-beef-herd/sruc-report-structure-and-efficiency-of-the-scottish-beef-herd/govscot%3Adocument/SRUC%2BReport%2B-%2BStructure%2Band%2BEfficiency%2Bof%2Bthe%2BScottish%2BBeef%2BHerd%2B-%2BCTS%2B-%2BFINAL.pdf", target = "_blank", "Structure and Efficiency of the Scottish Beef Herd - Cattle Tracing System Insights")
agri_env_txt23 <-tags$a(href="https://www.gov.scot/binaries/content/documents/govscot/publications/factsheet/2021/01/suckler-beef-climate-scheme-research-papers/documents/sruc-report-implementation-issues-for-the-sbcgs/sruc-report-implementation-issues-for-the-sbcgs/govscot%3Adocument/SRUC%2BReport%2B-%2BImplementation%2Bissues%2Bfor%2Bthe%2BSBCGS%2BFINAL.pdf", target = "_blank", "Implementation issues for the Suckler Beef Climate Group Scheme")
agri_env_txt24 <-tags$a(href="https://www.gov.scot/binaries/content/documents/govscot/publications/factsheet/2021/01/suckler-beef-climate-scheme-research-papers/documents/sruc-report-estimated-suckler-beef-climate-scheme-implications-for-cattle-numbers/sruc-report-estimated-suckler-beef-climate-scheme-implications-for-cattle-numbers/govscot%3Adocument/SRUC%2BReport%2B-%2BEstimated%2BSuckler%2BBeef%2BClimate%2BScheme%2Bimplications%2Bfor%2Bcattle%2Bnumbers%2BFINAL.pdf", target = "_blank", "Estimated Suckler Beef Climate Scheme implications for cattle numbers")
agri_env_txt25 <-tags$a(href="https://www.gov.scot/binaries/content/documents/govscot/publications/factsheet/2021/01/suckler-beef-climate-scheme-research-papers/documents/sruc-report-environmental-conditionality/sruc-report-environmental-conditionality/govscot%3Adocument/SRUC%2BReport%2B-%2BEnvironmental%2BConditionality%2BFINAL.pdf", target = "_blank", "Environmental Conditionality on Direct Payment to Land Managers")
agri_env_txt26 <-tags$a(href="https://www.gov.scot/binaries/content/documents/govscot/publications/factsheet/2021/01/suckler-beef-climate-scheme-research-papers/documents/sruc-report---beef-structure-and-payments-report/sruc-report---beef-structure-and-payments-report/govscot%3Adocument/SRUC%2BReport%2B-%2BBeef%2BStructure%2Band%2BPayments%2BReport%2BFINAL.pdf", target = "_blank", "Structure and support of the Scottish Beef Sector 2019 - impact of CAP 2015 reforms")
agri_env_txt27 <-tags$a(href="https://www.gov.scot/binaries/content/documents/govscot/publications/factsheet/2021/01/suckler-beef-climate-scheme-research-papers/documents/sruc-report-advisory-support-and-accreditation-updated/sruc-report-advisory-support-and-accreditation-updated/govscot%3Adocument/SRUC%2BReport%2B-%2BAdvisory%2BSupport%2B%2526%2BAccreditation%2BUpdated%2BFINAL.pdf", target = "_blank", "Suckler Beef Climate Scheme: Advisory Support and Accreditation")

agri_env_txt100 <- "Scottish Greenhouse Gas Statistics"
agri_env_txt101 <- "Official estimates of greenhouse gas emissions are available in the "
agri_env_txt102 <-  tags$a(href="https://www.gov.scot/publications/scottish-greenhouse-gas-statistics-2021/", target = "_blank", "Scottish Greenhouse Gas Statistics 2021 publication")

agri_env_txt103 <- "Scottish Nitrogen Balance Sheet"
agri_env_txt104 <- "The "
agri_env_txt105 <- tags$a(href="https://www.gov.scot/publications/scottish-nitrogen-balance-sheet-2020/", target = "_blank", "Scottish Nitrogen Balance Sheet 2020")
agri_env_txt105 <- " brings together evidence on flows of nitrogen in Scotland to understand and keep track of the use of nitrogen." 

agri_env_txt106 <- "Scottish Government resources and commissioned reports"
# agri_env_txt107 <- "The following lists link to key evidence reports, including reports commissioned or published for the Farmer-led Groups:"

agri_env_txt108 <- "Agriculture and Rural Communities (Scotland) Bill: supporting evidence and analysis"
agri_env_txt109 <- tags$a(href="https://www.gov.scot/publications/agriculture-rural-communities-scotland-bill-supporting-evidence-analysis/pages/1/", target="_blank", "The supporting evidence and analysis") 
agri_env_txt110 <- " outlines the current position of the Scottish agricultural sector, with evidence aligned to the objectives in our Vision for Agriculture."





