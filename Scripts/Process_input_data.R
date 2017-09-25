# Processing data sources for sustainable food systems project
# Implemented by: H. Achicanoy, P. Alvarez & L. Lamotte
# CIAT, 2017

# R options
g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)
OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
wk_dir <- paste0(OSysPath, "/workspace_cluster_9/Sustainable_Food_System/Input_data/"); setwd(wk_dir)
rm(wk_dir, OSysPath, OSys)

# Load packages
suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(maptools)){install.packages('maptools'); library(maptools)} else {library(maptools)})
suppressMessages(if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} else {library(dplyr)})
suppressMessages(if(!require(tidyr)){install.packages('tidyr'); library(tidyr)} else {library(tidyr)})
suppressMessages(if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)} else {library(ggplot2)})
suppressMessages(if(!require(jsonlite)){install.packages('jsonlite'); library(jsonlite)} else {library(jsonlite)})
suppressMessages(if(!require(foreach)){install.packages('foreach'); library(foreach)} else {library(foreach)})
suppressMessages(if(!require(doMC)){install.packages('doMC'); library(doMC)} else {library(doMC)})
suppressMessages(if(!require(XML)){install.packages('XML'); library(XML)} else {library(XML)})
suppressMessages(if(!require(plspm)){install.packages('plspm'); library(plspm)} else {library(plspm)})
suppressMessages(if(!require(reshape)){install.packages('reshape'); library(reshape)} else {library(reshape)})
suppressMessages(if(!require(reshape)){install.packages('tidyverse'); library(tidyverse)} else {library(tidyverse)})
suppressMessages(library(compiler))

# Worldwide shapefile
countries <- rgdal::readOGR(dsn = "./world_shape", "all_countries")
countries$COUNTRY <- iconv(countries$COUNTRY, from = "UTF-8", to = "latin1")

# Country codes FAO
if(!file.exists('./world_foodSecurity/FAO_ISO3_codes.RDS')){
  iso3FAO <- readHTMLTable('http://www.fao.org/countryprofiles/iso3list/en/')
  iso3FAO <- iso3FAO$`NULL`
  saveRDS(object = iso3FAO, file = './world_foodSecurity/FAO_ISO3_codes.RDS')
} else {
  iso3FAO <- readRDS(file = './world_foodSecurity/FAO_ISO3_codes.RDS')
  iso3FAO$`Short name` <- as.character(iso3FAO$`Short name`); iso3FAO$`Short name`[which(iso3FAO$`Short name` == "Côte d'Ivoire")] <- "Ivory Coast"; iso3FAO$`Short name` <- as.factor(iso3FAO$`Short name`)
}

# Country codes World Bank
iso3WBK <- readHTMLTable('http://wits.worldbank.org/wits/wits/witshelp/content/codes/country_codes.htm')
iso3WBK <- iso3WBK$`NULL`
iso3WBK$V3 <- NULL
colnames(iso3WBK) <- c("Country", "ISO3")
iso3WBK <- iso3WBK[-1,]; rownames(iso3WBK) <- 1:nrow(iso3WBK)

dim(inner_join(x = iso3FAO, y = iso3WBK, by = "ISO3"))[1]

iso3FAO[which(is.na(match(iso3FAO$ISO3, iso3WBK$ISO3))),] %>% View

# ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
# ### DIMENSION: HUMAN INTERVENTIONS                                                                            ###
# ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
# 
# # 1. Accessibility per country
# if(!file.exists('./world_cityAccess/countries_access.txt')){
#   
#   # Accessibility to cities
#   tmpRaster <- paste(getwd(), "/tmpRaster", sep = "")
#   if(!dir.exists(tmpRaster)){dir.create(tmpRaster)}
#   rasterOptions(tmpdir = tmpRaster)
#   access <- raster('./world_cityAccess/acc_50k')
#   
#   # List of countries (excluding Antarctica)
#   countryList <- countries@data$COUNTRY
#   countryList <- countryList[which(countryList!="Antarctica")]
#   
#   # Function to calculate median per country
#   calc_median <- function(rObject, i){
#     
#     country <- countries[countries@data$COUNTRY==countryList[i],]
#     country_data <- raster::crop(rObject, extent(country))
#     country_data <- raster::mask(x = country_data, mask = country)
#     values <- country_data[!is.na(country_data[])]
#     country_data <- data.frame(ISO3 = countries@data$ISO3[which(countries@data$COUNTRY == countryList[i])], Country = countryList[i], Median = median(values, na.rm = T)); rm(values, country)
#     cat(paste("Country: ", countryList[i], " done\n", sep = ""))
#     return(country_data)
#     
#   }; calc_median <- cmpfun(calc_median)
#   
#   # Parellize the process using 8 cores
#   registerDoMC(8)
#   countries_access <- foreach(i = 1:length(countryList)) %dopar% {
#     tryCatch(expr={
#       calc_median(rObject = access, i = i)
#     },
#     error=function(e){
#       cat("Cutting process failed:", countryList[i], "\n")
#       return("Done\n")
#     })
#   }; removeTmpFiles(h = 0)
#   
#   countries_access <- do.call(rbind, countries_access)
#   saveRDS(object = countries_access, file = './world_cityAccess/countries_access.RDS')
#   removeTmpFiles(h = 0)
# } else {
#   countries_access <- read.table(file = './world_cityAccess/countries_access.txt', sep = ",", header = T)
#   names(countries_access)[ncol(countries_access)] <- "Access_median"
# }
# 
# # 2. Global Human Footprint (1995-2004) per country
# # From: http://sedac.ciesin.columbia.edu/data/set/wildareas-v2-human-footprint-geographic/data-download
# if(!file.exists('./world_humanFootprint/countries_foodprint.txt')){
#   
#   # Human footprint raster
#   tmpRaster <- paste(getwd(), "/tmpRaster", sep = "")
#   if(!dir.exists(tmpRaster)){dir.create(tmpRaster)}
#   rasterOptions(tmpdir = tmpRaster)
#   hfootprint <- raster('./world_humanFootprint/hf_v2geo')
#   
#   # List of countries (excluding Antarctica)
#   countryList <- countries@data$COUNTRY
#   countryList <- countryList[which(countryList!="Antarctica")]
#   
#   # Function to calculate median per country
#   calc_median <- function(rObject, i){
#     
#     country <- countries[countries@data$COUNTRY==countryList[i],]
#     country_data <- raster::crop(rObject, extent(country))
#     country_data <- raster::mask(x = country_data, mask = country)
#     values <- country_data[!is.na(country_data[])]
#     data.frame(ISO3 = countries@data$ISO3[which(countries@data$COUNTRY == countryList[i])], Country = countryList[i], Median = median(values, na.rm = T)); rm(values, country)
#     cat(paste("Country: ", countryList[i], " done\n", sep = ""))
#     return(country_data)
#     
#   }; calc_median <- cmpfun(calc_median)
#   
#   # Parellize the process using 8 cores
#   registerDoMC(8)
#   countries_footprint <- foreach(i = 1:length(countryList)) %dopar% {
#     tryCatch(expr={
#       calc_median(rObject = hfootprint, i = i)
#     },
#     error=function(e){
#       cat("Cutting process failed:", countryList[i], "\n")
#       return("Done\n")
#     })
#   }; removeTmpFiles(h = 0)
#   countries_footprint <- do.call(rbind, countries_footprint)
#   saveRDS(object = countries_footprint, file = './world_humanFootprint/countries_foodprint.RDS')
#   removeTmpFiles(h = 0)
# } else {
#   countries_footprint <- read.table(file = './world_humanFootprint/countries_foodprint.txt', sep = ",", header = T)
#   names(countries_footprint)[ncol(countries_footprint)] <- "Footprint_median"
# }
# 
# h_interventions <- dplyr::inner_join(x = countries_access, y = countries_footprint, by = "ISO3")
# h_interventions <- h_interventions %>% dplyr::select(ISO3, Access_median, Footprint_median); rm(countries_access, countries_footprint)
# 
# ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
# ### DIMENSION: FOOD SECURITY                                                                                  ###
# ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
# 
# # 1. Global Hunger Index Data 2016
# # From: https://github.com/IFPRI/Global-Hunger-Index/tree/master/data
# ghi <- jsonlite::fromJSON("./world_globalHungerIndex2016/country-details.json"); rows <- ghi$data$name_de; ISO3 <- ghi$data$id
# ghi <- ghi$data$score; ghi$ISO3 <- ISO3; rownames(ghi) <- rows; rm(rows, ISO3)
# for(i in 1:(ncol(ghi)-1)){
#   ghi[,i] <- gsub(pattern = "-", replacement = NA, x = ghi[,i]) # Omit empty data
#   ghi[,i] <- gsub(pattern = "<5", replacement = NA, x = ghi[,i]) # Omit values under detection limit
#   ghi[,i] <- as.numeric(ghi[,i])
# }; rm(i)
# ghi <- ghi[which(!is.na(ghi$ISO3)),]
# ghi <- ghi[,c("ISO3", "year2000")]; names(ghi)[2] <- "GHI_2000"
# 
# # 2. Global Subnational Prevalence of Child Malnutrition (1990 - 2002)
# # From: http://sedac.ciesin.columbia.edu/data/set/povmap-global-subnational-prevalence-child-malnutrition/data-download
# 
# chmalnutrition <- read.csv("./world_childMalnutrition/hunger_data.csv") # These data come from cities calculations
# 
# # Obtain countries according to ISO3 codes
# aux <- countries@data %>% dplyr::select(ISO3, COUNTRY)
# chmalnutrition <- chmalnutrition %>% dplyr::left_join(x = chmalnutrition, y = aux, by = c("ISO3V10" = "ISO3")); rm(aux)
# 
# # Define properly NAs
# chmalnutrition$UW[which(chmalnutrition$UW == -999)] <- NA
# chmalnutrition$PCTU5[which(chmalnutrition$PCTU5 == -999)] <- NA
# 
# # Calculate mean data per country
# chmalnutrition <- chmalnutrition %>% dplyr::select(ISO3V10, COUNTRY, UW, PCTU5) %>% dplyr::group_by(ISO3V10, COUNTRY) %>% dplyr::summarise_all(mean, na.rm = T)
# names(chmalnutrition)[which(names(chmalnutrition) == "ISO3V10")] <- "ISO3"
# chmalnutrition <- chmalnutrition[which(!is.na(chmalnutrition$ISO3)),]; rownames(chmalnutrition) <- 1:nrow(chmalnutrition)
# plot(chmalnutrition$PCTU5, chmalnutrition$UW, xlab = "% of population under age 5", ylab = "Percentage of children underweight", pch = 20)
# chmalnutrition <- chmalnutrition[,c("ISO3", "UW")]; names(chmalnutrition)[2] <- "ChldMalnutrition"
# chmalnutrition <- as.data.frame(chmalnutrition)
# 
# # 3. FAO's suite of Food Security Indicators
# fsecurity <- read.csv("./world_foodSecurity/FAOSTAT_data_2-20-2017.csv")
# timeList <- unique(as.character(fsecurity$Year)); fsecurity$Country <- as.character(fsecurity$Country)
# fsecurity$Country[which(fsecurity$Country == "Côte d'Ivoire")] <- "Ivory Coast"; fsecurity$Country <- factor(fsecurity$Country)
# 
# # Loading ISO3 codes for FAO information
# # From: http://www.fao.org/countryprofiles/iso3list/en/
# if(!file.exists('./world_foodSecurity/FAO_ISO3_codes.RDS')){
#   iso3FAO <- readHTMLTable('http://www.fao.org/countryprofiles/iso3list/en/')
#   iso3FAO <- iso3FAO$`NULL`
#   saveRDS(object = iso3FAO, file = './world_foodSecurity/FAO_ISO3_codes.RDS')
# } else {
#   iso3FAO <- readRDS(file = './world_foodSecurity/FAO_ISO3_codes.RDS')
#   iso3FAO$`Short name` <- as.character(iso3FAO$`Short name`); iso3FAO$`Short name`[which(iso3FAO$`Short name` == "C�te d'Ivoire")] <- "Ivory Coast"; iso3FAO$`Short name` <- as.factor(iso3FAO$`Short name`)
# }
# 
# fsecurity <- dplyr::left_join(x = fsecurity, y = iso3FAO, by = c("Country" = "Short name")); rm(iso3FAO)
# 
# fsecurityList <- lapply(1:length(timeList), function(i){
#   df <- fsecurity %>% dplyr::select(ISO3, Country, Item, Year, Value) %>% dplyr::filter(Year == timeList[i]) %>% tidyr::spread(key = Item, value = Value)
#   df <- df[which(!is.na(df$ISO3)),]; rownames(df) <- 1:nrow(df)
#   return(df)
# }); names(fsecurityList) <- timeList; rm(fsecurity, timeList)
# 
# # Year or periods to exclude
# ypList <- unlist(lapply(1:length(fsecurityList), function(i){
#   
#   x <- fsecurityList[[i]]
#   dim.mat <- nrow(x[,4:ncol(x)]) * ncol(x[,4:ncol(x)])
#   if(sum(is.na(x[,4:ncol(x)])) == dim.mat){
#     return(names(fsecurityList)[i])
#   } else {
#       return(cat(""))
#   }
#   
# }))
# fsecurityList <- fsecurityList[setdiff(x = names(fsecurityList), y = ypList)]; rm(ypList)
# 
# # for(i in 1:length(fsecurityList)){
# #   
# #   Sys.sleep(1)
# #   pairs(fsecurityList[[i]][,4:ncol(fsecurityList[[i]])])
# #   
# # }
# 
# fsec <- fsecurityList[[50]]; fsec <- fsec[,c(1, 4, 5, 8, 16)]
# names(fsec)[2:ncol(fsec)] <- c("sanitation", "water_sources", "GDP", "political_stability")
# 
# complete_data <- dplyr::full_join(x = ghi, y = chmalnutrition, by = c("ISO3" = "ISO3")); rm(ghi, chmalnutrition)
# complete_data <- dplyr::full_join(x = complete_data, y = h_interventions, by = c("ISO3" = "ISO3")); rm(h_interventions)
# complete_data <- dplyr::full_join(x = complete_data, y = fsec, by = c("ISO3" = "ISO3")); rm(fsec)
# 
# complete_data <- complete_data[-which(apply(X = complete_data, MARGIN = 1, FUN = function(x){sum(is.na(x))}) >= 7),]
# rownames(complete_data) <- 1:nrow(complete_data)
# 
# saveRDS(object = complete_data, file = "data_joined.RDS")

### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
### DIMENSION: ECONOMICS
### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###

employment <- read.csv(file = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Input_data_final/Economic/Employment_Indicators_E_All_Data.csv")
employment <- employment %>% filter(Indicator == "Agriculture value added per worker (constant 2005 US$)" |
                                      Indicator == "Wage employment distribution, agriculture" |
                                      Indicator == "Time related underemployment in agriculture")
employment <- employment %>% select(Country, Indicator, 8:ncol(employment))
employment <- employment %>% gather(Year, Value, -(Country:Indicator))
employment <- unique(employment)
employment <- employment[setdiff(1:nrow(employment), grep(pattern = "F$", x = employment$Year)),]
rownames(employment) <- 1:nrow(employment)
employment$Value <- employment$Value %>% as.character %>% as.numeric
employment <- employment %>% group_by(Country, Indicator, Year) %>% summarise(Value = mean(Value, na.rm = T))
employment$Year <- gsub(pattern = "Y", replacement = "", x = employment$Year) %>% as.character %>% as.numeric

employment %>% ggplot(aes(x = Year, y = Value, colour = Indicator)) +
  geom_point() + facet_wrap(~Indicator, scales = "free") + 
  theme_bw()

employment <- employment %>% spread(Indicator, Value)
names(employment)[3:5] <- c("Ag_value_added", "Time_underemployment", "Wage_employment")

cor(employment[,3:5], method = "spearman", use = "complete.obs")

### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
### DIMENSION: SOCIAL
### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###

# Labor force female
lf_female <- read_csv(file = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Input_data_final/Social/Labor_force_female.csv", col_names = T, skip = 4)
names(lf_female)[1:2] <- c("Country", "ISO3")
lf_female <- cbind(lf_female[,1:2], lf_female[,5:ncol(lf_female)])
lf_female <- lf_female %>% gather(Year, Value, -(Country:ISO3))
lf_female <- lf_female %>% select(ISO3, Country, Year, Value)
names(lf_female)[ncol(lf_female)] <- "Labor_force_female"
lf_female$Labor_force_female <- as.numeric(lf_female$Labor_force_female)

lf_female %>% filter(Year > 1989) %>% ggplot(aes(x = Year, y = Labor_force_female, colour = Country)) +
  geom_point() + theme(legend.position = 'none')

### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
### DIMENSION: FOOD AND NUTRITION
### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###

# Loading ISO3 codes for FAO information
# From: http://www.fao.org/countryprofiles/iso3list/en/
if(!file.exists('./world_foodSecurity/FAO_ISO3_codes.RDS')){
  iso3FAO <- readHTMLTable('http://www.fao.org/countryprofiles/iso3list/en/')
  iso3FAO <- iso3FAO$`NULL`
  saveRDS(object = iso3FAO, file = './world_foodSecurity/FAO_ISO3_codes.RDS')
} else {
  iso3FAO <- readRDS(file = './world_foodSecurity/FAO_ISO3_codes.RDS')
  iso3FAO$`Short name` <- as.character(iso3FAO$`Short name`); iso3FAO$`Short name`[which(iso3FAO$`Short name` == "Côte d'Ivoire")] <- "Ivory Coast"; iso3FAO$`Short name` <- as.factor(iso3FAO$`Short name`)
}

# Obesity
obesity <- read_csv(file = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Input_data_final/Food_Nutrition/Obesity.csv", col_names = T, skip = 2)
names(obesity)[1] <- "Country"
names(obesity)[2:ncol(obesity)] <- gsub(pattern = "_1", replacement = "", x = names(obesity)[2:ncol(obesity)])
obesity <- obesity[-1,]
names(obesity)[2:ncol(obesity)] <- paste0(names(obesity)[2:ncol(obesity)], "-", obesity[1,2:ncol(obesity)])
obesity <- obesity[-1,]

obesity <- obesity %>% gather(year_sex, Value, -Country) %>% separate(year_sex, c("Year", "Sex"))
obesity$Value <- gsub(pattern = " \\[*.*?\\]", replacement = "", x = obesity$Value) %>% as.character %>% as.numeric

aux <- obesity %>% group_by(Country, Year) %>% summarise(Value = mean(Value, na.rm = T))
aux$Sex <- "Average"

obesity <- rbind(obesity %>% as.data.frame, aux %>% as.data.frame); rm(aux)
obesity %>% filter(Sex == "Average") %>% ggplot(aes(x = Year, y = Value)) + geom_point()

obesity <- obesity %>% filter(Sex == "Average")
obesity$Country <- as.character(obesity$Country)
obesity$Country[which(obesity$Country == "Côte d'Ivoire")] <- "Ivory Coast"
obesity <- obesity[setdiff(1:nrow(obesity), grep(pattern = "^Sudan$", x = obesity$Country)),]; rownames(obesity) <- 1:nrow(obesity)
obesity$Country[which(obesity$Country == "Sudan (former)")] <- "Sudan"
obesity$Country[which(obesity$Country == "The former Yugoslav republic of Macedonia")] <- "The former Yugoslav Republic of Macedonia"
obesity$Country[which(obesity$Country == "United Kingdom of Great Britain and Northern Ireland")] <- "United Kingdom"
obesity <- dplyr::left_join(x = obesity, y = iso3FAO %>% select(`Short name`, ISO3), by = c("Country" = "Short name"))
obesity <- obesity %>% select(ISO3, Country, Year, Value)
names(obesity)[ncol(obesity)] <- "Obesity"

obesity %>%
  split(.$Year) %>%
  lapply(., dim)

# Stunting
stunting <- read_csv(file = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Input_data_final/Food_Nutrition/Stunting.csv", col_names = T, skip = 1)
names(stunting)[4] <- "Stunting"
stunting <- stunting %>% select(Country, Year, Stunting)
stunting$Country <- as.character(stunting$Country)
stunting$Country[which(stunting$Country == "Côte d'Ivoire")] <- "Ivory Coast"
stunting$Country[which(stunting$Country == "United Kingdom of Great Britain and Northern Ireland")] <- "United Kingdom"
stunting$Country[which(stunting$Country == "The former Yugoslav republic of Macedonia")] <- "The former Yugoslav Republic of Macedonia"
stunting <- dplyr::left_join(x = stunting, y = iso3FAO %>% select(`Short name`, ISO3), by = c("Country" = "Short name"))
stunting <- stunting %>% select(ISO3, Country, Year, Stunting)

stunting %>%
  split(.$Year) %>%
  lapply(., dim)

# GFSI indices 2016
gfsi <- readxl::read_excel(path = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Input_data_final/Food_Nutrition/GFSI_2016.xlsx", sheet = 2, skip = 5)
gfsi <- gfsi[,10:ncol(gfsi)]
gfsi <- gfsi[-c(1:4),]
gfsi <- gfsi[-c(7:8),]
gfsi <- gfsi[-10,]
gfsi <- gfsi[-18,]
gfsi <- gfsi[-19,]
gfsi <- gfsi[-22,]
gfsi <- gfsi[-26,]
gfsi <- gfsi[-29,]
gfsi <- gfsi[1:36, 1:115]
names(gfsi)[1:2] <- c("Variable", "Units")
gfsi$Units <- NULL

gfsi <- gfsi %>% gather(Country, Value, -Variable) %>% spread(Variable, Value)
nmList <- names(gfsi)
gfsi <- data.frame(Country = gfsi$Country, apply(X = gfsi[,-1], MARGIN = 2, FUN = as.numeric))
names(gfsi) <- nmList; rm(nmList)
gfsi$Country <- as.character(gfsi$Country)
gfsi$Country[which(gfsi$Country == "Bolivia")] <- "Bolivia (Plurinational State of)"
gfsi$Country[which(gfsi$Country == "Congo (Dem. Rep.)")] <- "Congo"
gfsi$Country[grep(pattern = "^Cote", x = gfsi$Country)] <- "Ivory Coast"
gfsi$Country[which(gfsi$Country == "Czech Republic")] <- "Czechia"
gfsi$Country[which(gfsi$Country == "Laos")] <- "Lao People's Democratic Republic"
gfsi$Country[which(gfsi$Country == "Russia")] <- "Russian Federation"
gfsi$Country[which(gfsi$Country == "South Korea")] <- "Republic of Korea"
gfsi$Country[which(gfsi$Country == "Syria")] <- "Syrian Arab Republic"
gfsi$Country[which(gfsi$Country == "Tanzania")] <- "United Republic of Tanzania"
gfsi$Country[which(gfsi$Country == "United States")] <- "United States of America"
gfsi$Country[which(gfsi$Country == "Venezuela")] <- "Venezuela (Bolivarian Republic of)"
gfsi$Country[which(gfsi$Country == "Vietnam")] <- "Viet Nam"
gfsi <- dplyr::left_join(x = gfsi, y = iso3FAO %>% select(`Short name`, ISO3), by = c("Country" = "Short name"))

colnames(gfsi)[grep(pattern = "consumption", x = colnames(gfsi))]

# Food supply variability
fsvar <- read_csv(file = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Input_data_final/Food_Nutrition/Food_supply_variability.csv", col_names = T)
fsvar <- fsvar %>% select(Country, Year, Value)
names(fsvar)[3] <- "Food_supply_variability"
fsvar$Country <- as.character(fsvar$Country)
fsvar$Country[which(fsvar$Country == "Côte d'Ivoire")] <- "Ivory Coast"
fsvar$Country[which(fsvar$Country == "Faroe Islands")] <- "Faroe Islands (Associate Member)"
fsvar <- dplyr::left_join(x = fsvar, y = iso3FAO %>% select(`Short name`, ISO3), by = c("Country" = "Short name"))
fsvar <- fsvar %>% select(ISO3, Country, Year, Food_supply_variability)

# Access to electricity
access_electricity <- readxl::read_xls(path = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Input_data_final/Food_Nutrition/Access_to_electricity.xls", sheet = 1, col_names = T, skip = 3)
names(access_electricity)[1:2] <- c("Country", "ISO3")
access_electricity <- access_electricity %>% select(Country, ISO3, 5:ncol(access_electricity))
access_electricity <- access_electricity %>% gather(Year, Value, -(Country:ISO3))
access_electricity <- access_electricity %>% select(ISO3, Country, Year, Value)
names(access_electricity)[4] <- "Access_electricity"

access_electricity %>% ggplot(aes(x = Year, y = Value)) + geom_point()

# Improved water sources
improved_water <- read_csv(file = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Input_data_final/Food_Nutrition/Access_improved_water.csv", col_names = T)
improved_water <- improved_water %>% select(Country, Year, Value)
names(improved_water)[3] <- "Improved_water"
improved_water$Country <- as.character(improved_water$Country)
improved_water$Country[which(improved_water$Country == "Côte d'Ivoire")] <- "Ivory Coast"
improved_water <- dplyr::left_join(x = improved_water, y = iso3FAO %>% select(`Short name`, ISO3), by = c("Country" = "Short name"))
improved_water <- improved_water %>% select(ISO3, Country, Year, Improved_water)

improved_water %>% ggplot(aes(x = Year, y = Improved_water)) + geom_point()

# Vitamin A deficiency
vitamin_A <- read_csv(file = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Input_data_final/Food_Nutrition/Vitamin_A_deficiency.csv", col_names = T)
vitamin_A <- vitamin_A %>% select(Country, Item, Year, Value)
vitamin_A <- vitamin_A %>% spread(Item, Value)
names(vitamin_A)[3:4] <- c("Iodine_deficiency", "Vitamin_A_deficiency")
vitamin_A$Country <- as.character(vitamin_A$Country)
vitamin_A$Country[which(vitamin_A$Country == "Côte d'Ivoire")] <- "Ivory Coast"
vitamin_A <- dplyr::left_join(x = vitamin_A, y = iso3FAO %>% select(`Short name`, ISO3), by = c("Country" = "Short name"))
vitamin_A <- vitamin_A %>% select(ISO3, Country, Year, Iodine_deficiency, Vitamin_A_deficiency)

vitamin_A %>% ggplot(aes(x = Year, y = Vitamin_A_deficiency)) + geom_point()

# Diet diversification
diet_div <- read_csv(file = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Input_data_final/Food_Nutrition/Diet_diversification.csv", col_names = T)
diet_div <- diet_div %>% select(Country, Year, Value)
names(diet_div)[3] <- "Diet_diversification"
diet_div$Country <- as.character(diet_div$Country)
diet_div$Country[which(diet_div$Country == "Côte d'Ivoire")] <- "Ivory Coast"
diet_div <- dplyr::left_join(x = diet_div, y = iso3FAO %>% select(`Short name`, ISO3), by = c("Country" = "Short name"))
diet_div <- diet_div %>% select(ISO3, Country, Year, Diet_diversification)


dplyr::left_join(x = diet_div, y = iso3FAO %>% select(`Short name`, ISO3), by = c("Country" = "Short name")) %>% select(Country, ISO3) %>% unique %>% View


