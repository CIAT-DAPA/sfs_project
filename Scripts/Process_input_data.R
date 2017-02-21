# Processing data sources for sustainable food systems project
# Implemented by: H. Achicanoy, P. Alvarez & L. Lamotte
# CIAT, 2017

# R options
options(warn = -1); options(scipen = 999)
OSys <- "windows" # It could be linux (for servers) or windows (for local machine)
if(OSys == "linux"){
  wk_dir <- "/mnt/Workspace_cluster_9/Sustainable_Food_System/Input_data"; setwd(wk_dir); rm(wk_dir)
} else {
  if(OSys == "windows"){
    wk_dir <- "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Input_data"; setwd(wk_dir); rm(wk_dir)
  }
}

# Load packages
suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(maptools)){install.packages('maptools'); library(maptools)} else {library(maptools)})
suppressMessages(if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} else {library(dplyr)})
suppressMessages(if(!require(tidyr)){install.packages('tidyr'); library(tidyr)} else {library(tidyr)})
suppressMessages(if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)} else {library(ggplot2)})
suppressMessages(if(!require(jsonlite)){install.packages('jsonlite'); library(jsonlite)} else {library(jsonlite)})
suppressMessages(library(compiler))

# Global shapefile
countries <- shapefile('./world_shape/all_countries.shp'); # countries <- shapefile('./all_countries.shp')

### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
### DIMENSION: HUMAN INTERVENTIONS                                                                            ###
### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###

# 1. Accessibility to cities (TO PROCESS IN LINUX SERVERS)
access <- raster('./world_cityAccess/acc_50k')

# Accessibility to cities per country

# # Way 1
# countries_access <- lapply(1:length(countries@data$COUNTRY), function(i){
#   
#   country <- countries[countries@data$COUNTRY==countries@data$COUNTRY[i],]
#   country_access <- raster::crop(access, extent(country))
#   country_access <- raster::mask(x = country_access, mask = country)
#   values <- country_access[!is.na(country_access[])]
#   country_access <- data.frame(Country = countries@data$COUNTRY[i], Access_median = median(values, na.rm = T)); rm(values, country)
#   cat(paste("Country: ", countries@data$COUNTRY[i], " done\n", sep = ""))
#   return(country_access)
#   
# })
# countries_access <- do.call(rbind, countries_access)

# Way 2
extractCMP <- compiler::cmpfun(f = raster::extract)
access <- extractCMP(access, countries, fun = median)
access <- data.frame(access)
saveRDS(object = access, file = '~/countries_access.RDS')

# 2. Global Human Footprint (1995-2004) (TO PROCESS IN LINUX SERVERS)
# From: http://sedac.ciesin.columbia.edu/data/set/wildareas-v2-human-footprint-geographic/data-download
hfootprint <- raster('./world_humanFootprint/hf_v2geo')
hfootprint <- extractCMP(hfootprint, countries, fun = median)
hfootprint <- data.frame(hfootprint)

### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
### DIMENSION: FOOD SECURITY                                                                                  ###
### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###

# 1. Global Hunger Index Data 2016
# From: https://github.com/IFPRI/Global-Hunger-Index/tree/master/data
ghi <- jsonlite::fromJSON("./world_globalHungerIndex2016/country-details.json")
ghi <- ghi$data$score

# 2. Global Subnational Prevalence of Child Malnutrition (1990 - 2002)
# From: http://sedac.ciesin.columbia.edu/data/set/povmap-global-subnational-prevalence-child-malnutrition/data-download

chmalnutrition <- read.csv("./world_childMalnutrition/hunger_data.csv")

# 3. FAO's suite of Food Security Indicators
fsecurity <- read.csv("./world_foodSecurity/FAOSTAT_data_2-20-2017.csv")
timeList <- unique(as.character(fsecurity$Year))

fsecurityList <- lapply(1:length(timeList), function(i){
  df <- fsecurity %>% dplyr::select(Country.Code, Country, Item, Year, Value) %>% dplyr::filter(Year == timeList[i]) %>% tidyr::spread(key = Item, value = Value)
  return(df)
}); rm(fsecurity)
# It seems properly to use index 26 (2014-2016) indicators
View(fsecurityList[[25]])
