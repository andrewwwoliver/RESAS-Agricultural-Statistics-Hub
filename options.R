##options.R

library(shiny)
library(highcharter)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
library(rsconnect)
library(png)
library(htmltools)
library(DT)
library(shinyjs)
library(RColorBrewer)
library(shinyjs)


# Highchart options
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
hcoptslang$numericSymbols <- " "
options(highcharter.lang = hcoptslang)

# Load the theme
thm <- source("hc_theme.R")$value

# Load the .RData file containing the datasets
load("ghg_data.RData")
load("census_data.RData")


# Preset list of colors
preset_colors <- c("#002d54", "#2b9c93", "#6a2063", "#e5682a", "#0b4c0b", "#5d9f3c", "#592c20", "#ca72a2")

# Function to assign colors to variables
assign_colors <- function(data, colors) {
  first_col_name <- names(data)[1]
  variables <- unique(data[[first_col_name]])
  setNames(colors[1:length(variables)], variables)
}

safe_as_numeric <- function(x) {
  suppressWarnings({
    result <- as.numeric(x)
    if (is.na(result) && !is.na(x)) {
      return(NA_real_)
    }
    return(result)
  })
}