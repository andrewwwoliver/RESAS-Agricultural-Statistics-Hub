library(highcharter)
library(tidyverse)

library(readxl)
library(scales)

# set options
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
hcoptslang$numericSymbols <- ""
options(highcharter.lang = hcoptslang)





thm <- hc_theme(
  colors = c("#2f6f55",
             "#43733e",
             "#627421",
             "#866f00",
             "#ae6300"
  ), 
  chart = list(
    backgroundColor = NULL
  ),
  title = list(
    style = list(
      color = "black",
      fontFamily = "Lato",
      fontSize= "24px"
    )
  ),
  subtitle = list(
    style = list(
      color = "black",
      fontFamily = "Lato"
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = "Lato",
      fontSize= "24px",
      color = "black"
    )
  ),
  itemHoverStyle = list(
    color = NULL
  )
)


crops <- read_xlsx("~/R/JAC_compendium/SG_Ag_compendium/crops.xlsx")

#shape data
crops <- crops %>% select(-`5_year_av`) %>%  pivot_longer(., cols = !Crop,
                      names_to = "Year",
                      values_to = "Area"
                      )
crops$Crop <- as.factor(crops$Crop)
levels(crops$Crop)

crops$Year <- as.integer(crops$Year)

#export
save(crops, file = "C:/Users/z620777/OneDrive - SCOTS Connect/R/JAC_compendium/SG_Ag_compendium/JAC_crops.RData")




#set as factor for graphs

#5 year av = NA
#

area <- crops%>% 
  hchart(
    'line', hcaes(x = Year,  y = Area, group = Crop),
    style = list(fontFamily = "Open sans"))%>%   
  hc_title(
    text ="Crops and Land Use ", 
    style = list(color = "#000000", fontSize = "26px", cursor = "default") 
  )%>% 
  hc_yAxis( 
    labels = list(style = list(color =  "#000000", fontSize = "24px", format = "{value:,f}")),
    title = list(text = "Hectares", style = list(color = "#000000", fontSize = "24px"))) %>% 
  hc_xAxis(
    labels = list(style = list(color =  "#000000", fontSize = "24px")),
    title = list(text = "", style = list(color = "#000000", fontSize = "24px")))

area <- hc_add_theme(area, thm)
area
