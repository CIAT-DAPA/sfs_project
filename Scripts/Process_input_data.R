# Processing and integrating data: SFS project
# Implemented by: H. Achicanoy & P. Alvarez
# CIAT, 2017

# R options
g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)

OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
wk_dir   <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9/Sustainable_Food_System/SFS_indicators", "Windows" = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators")
setwd(wk_dir); rm(wk_dir, OSysPath, OSys)

# Load packages
suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(maptools)){install.packages('maptools'); library(maptools)} else {library(maptools)})
suppressMessages(if(!require(jsonlite)){install.packages('jsonlite'); library(jsonlite)} else {library(jsonlite)})
suppressMessages(if(!require(foreach)){install.packages('foreach'); library(foreach)} else {library(foreach)})
suppressMessages(if(!require(doParallel)){install.packages('doParallel'); library(doParallel)} else {library(doParallel)})
suppressMessages(if(!require(XML)){install.packages('XML'); library(XML)} else {library(XML)})
suppressMessages(if(!require(plspm)){install.packages('plspm'); library(plspm)} else {library(plspm)})
suppressMessages(if(!require(reshape)){install.packages('reshape'); library(reshape)} else {library(reshape)})
suppressMessages(if(!require(tidyverse)){install.packages('tidyverse'); library(tidyverse)} else {library(tidyverse)})
suppressMessages(if(!require(countrycode)){install.packages('countrycode'); library(countrycode)} else {library(countrycode)})
suppressMessages(library(compiler))

## ========================================================================== ##
## Define countries to work with
## ========================================================================== ##

# Worldwide shapefile
countries <- rgdal::readOGR(dsn = "./Input_data/world_shape", "all_countries")
countries$COUNTRY <- iconv(countries$COUNTRY, from = "UTF-8", to = "latin1")

# Country code translation
country_codes <- countrycode_data %>% dplyr::select(country.name.en, iso3c, iso3n, iso2c, fao, wb)
country_codes$country.name.en <- country_codes$country.name.en %>% as.character
country_codes$country.name.en[which(country_codes$country.name.en == "Côte D'Ivoire")] <- "Ivory Coast"
country_codes$country.name.en[which(country_codes$country.name.en == "Virgin Islands, British")] <- "British Virgin Islands"
country_codes$country.name.en[which(country_codes$country.name.en == "Gambia (Islamic Republic of the)")] <- "Gambia"
country_codes$country.name.en[which(country_codes$country.name.en == "United Kingdom of Great Britain and Northern Ireland")] <- "United Kingdom"
country_codes$country.name.en[which(country_codes$country.name.en == "Virgin Islands, U.S.")] <- "United States Virgin Islands"
country_codes$country.name.en[which(country_codes$country.name.en == "Venezuela, Bolivarian Republic of")] <- "Venezuela"
country_codes$country.name.en[which(country_codes$country.name.en == "Palestine, State of")] <- "Palestine"
country_codes$country.name.en[which(country_codes$country.name.en == "Bolivia (Plurinational State of)")] <- "Bolivia"
country_codes$fao[which(country_codes == "Reunion")] <- 182

## ========================================================================== ##
## ENVIRONMENT
## ========================================================================== ##

# We need to include water quality indicators
if(!file.exists("environmental_dimension.csv")){
  
  ## 1. GHG Emissions (CO2eq)
  ## Measure: Air quality
  ## Original name: GHG emissions by sector
  ## Sectors: agriculture, energy, forest, industrial processes, land use, other sources, residential, transport, waste
  ## Units: gigagrams
  ## Years: 1990:2010 (Selected period: 2000:2010)
  ## Countries with data: 218, years: 2010
  
  emission <- read.csv("./Input_data_final/Environment/emission.csv")
  emission <- emission %>% dplyr::select(Country, Item, Year, Value)
  # For Patricia
  # emission2 <- emission %>% select(Country, Year, Emissions.agriculture.total)
  # emission2 <- emission2 %>% spread(Year, Emissions.agriculture.total)
  # write.csv(emission2, "./Temporal_data/GHG_emissions.csv", row.names = F)
  emission <- emission %>% filter(Year >= 2000)
  emission <- emission %>% spread(Item, Value)
  colnames(emission)[3:ncol(emission)] <- c("Emissions.agriculture.total",
                                            "Emissions.energy",
                                            "Emissions.forest",
                                            "Emissions.industrial",
                                            "Emissions.land.use",
                                            "Emissions.other",
                                            "Emissions.residential",
                                            "Emissions.transport",
                                            "Emissions.waste")
  # emission %>% filter(Country == "Colombia") %>% ggparcoord(data = ., columns = 3:ncol(.), groupColumn = 2, order = "anyClass")
  emission <- emission %>% gather(Source, Emission, Emissions.agriculture.total:Emissions.waste)
  # emission %>% ggplot(aes(x = Year, y = Emission, group = Country)) + geom_line(alpha = .2) +
  #   facet_wrap(~Source, scales = "free") +
  #   scale_x_continuous(breaks = 2000:2010, limits = c(2000, 2010)) +
  #   theme_bw()
  emission <- emission %>% spread(Source, Emission)
  emission$Country <- emission$Country %>% as.character
  emission$Country[which(emission$Country == "CÃ´te d'Ivoire")] <- "Ivory Coast"
  emission$Country[which(emission$Country == "Bolivia (Plurinational State of)")] <- "Bolivia"
  emission$Country[which(emission$Country == "RÃ©union")] <- "Reunion"
  emission$Country[which(emission$Country == "Sudan (former)")] <- "Sudan"
  emission$Country[which(emission$Country == "Czechia")] <- "Czech Republic"
  emission$Country[which(emission$Country == "French Southern and Antarctic Territories")] <- "French Southern Territories"
  emission$Country[which(emission$Country == "Guinea-Bissau")] <- "Guinea Bissau"
  emission$Country[which(emission$Country == "Netherlands Antilles (former)")] <- "Netherlands Antilles"
  emission$Country[which(emission$Country == "Pitcairn Islands")] <- "Pitcairn"
  emission$Country[which(emission$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
  emission$Country[which(emission$Country == "Wallis and Futuna Islands")] <- "Wallis and Futuna"
  
  yearsList <- emission$Year %>% unique %>% sort
  emissionList <- lapply(1:length(yearsList), function(i){
    df <- emission %>% select(Country, Year, Emissions.agriculture.total) %>% filter(Year == yearsList[i])
    df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
    return(df)
  })
  lapply(emissionList, function(y){apply(X = y, MARGIN = 2, FUN = function(x){sum(!is.na(x))})})
  
  recentEmission <- emissionList[[length(emissionList)]]
  recentEmission <- recentEmission %>% dplyr::select(country.name.en, iso3c, Emissions.agriculture.total)
  recentEmission <- recentEmission[complete.cases(recentEmission),]; rownames(recentEmission) <- 1:nrow(recentEmission)
  rm(emissionList, emission)
  
  
  ## 2. pH
  ## Measure: Water quality
  ## Original name: 
  ## Units: pH units
  ## Years: 
  ## Countries with data: 74
  pH1 <- readxl::read_excel("./Input_data_final/Environment/_water_pH.xlsx", sheet = 1)
  pH2 <- readxl::read_excel("./Input_data_final/Environment/_water_pH.xlsx", sheet = 2)
  pH3 <- readxl::read_excel("./Input_data_final/Environment/_water_pH.xlsx", sheet = 3)
  pH4 <- readxl::read_excel("./Input_data_final/Environment/_water_pH.xlsx", sheet = 4)
  pH <- rbind(pH1, pH2, pH3, pH4); rm(pH1, pH2, pH3, pH4)
  # dissOxygen %>% ggplot(aes(x = Value)) + geom_density() + facet_wrap(~ISO3)
  
  pH <- pH %>% select(ISO3, Value) %>% group_by(ISO3) %>% summarise(pH = median(Value, na.rm = T))
  pH <- dplyr::inner_join(x = country_codes, y = pH, by = c("iso3c" = "ISO3"))
  pH <- pH %>% dplyr::select(country.name.en, iso3c, pH)
  
  
  ## 3. Water withdrawal
  ## Measure: Water use
  ## Original name: Agricultural water withdrawal as % of total water withdrawal (%)
  ## Units: (%)
  ## Years: 2000:2016 (not all years have data)
  ## Countries with data: depends on the year
  ## Averaging by the period 2000-2016, we get 174 countries
  
  water <- readxl::read_excel(path = "./Input_data_final/Environment/water.xlsx", sheet = 1, col_names = T)
  water <- water[1:200,]
  names(water)[1] <- "Country"
  water$X__3 <- NULL
  
  water_aux <- lapply(1:nrow(water), function(i){
    df <- data.frame(Country = water$Country[i],
                     Year = c(water$year[i], water$year__1[i], water$year__2[i], water$year__3[i], water$year__4[i], water$year__5[i]),
                     Water.withdrawal = c(water$`1988-1992`[i], water$`1993-1997`[i], water$`1998-2002`[i], water$`2003-2007`[i], water$`2008-2012`[i], water$`2013-2017`[i]))
    return(df)
  })
  water <- do.call(rbind, water_aux); rm(water_aux)
  water <- water[which(apply(X = water, MARGIN = 1, FUN = function(x){sum(is.na(x))}) != 2),]
  rownames(water) <- 1:nrow(water)
  water <- water %>% filter(Year >= 2000)
  water$Country <- water$Country %>% as.character
  water$Country[which(water$Country == "Côte d'Ivoire")] <- "Ivory Coast"
  water$Country[which(water$Country == "Czechia")] <- "Czech Republic"
  water$Country[which(water$Country == "Guinea-Bissau")] <- "Guinea Bissau"
  water$Country[which(water$Country == "Occupied Palestinian Territory")] <- "Palestine"
  water$Country[which(water$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
  water$Country[which(water$Country == "Bolivia (Plurinational State of)")] <- "Bolivia"
  
  # water %>% ggplot(aes(x = Year, y = Water.withdrawal, group = Country)) +
  #   geom_line(alpha = .2) + 
  #   theme_bw()
  
  yearsList <- water$Year %>% unique %>% sort
  waterList <- lapply(1:length(yearsList), function(i){
    df <- water %>% filter(Year == yearsList[i])
    df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
    return(df)
  })
  lapply(waterList, dim)
  
  water <- water %>% tidyr::spread(key = Year, value = Water.withdrawal)
  water$Water.withdrawal <- rowMeans(water[,2:ncol(water)], na.rm = T)
  # water$Water.withdrawal <- rowMeans(water[,which(names(water)=="2012"):which(names(water)=="2016")], na.rm = T)
  water <- water %>% dplyr::select(Country, Water.withdrawal)
  
  water <- dplyr::inner_join(x = country_codes, y = water, by = c("country.name.en" = "Country"))
  water <- water %>% dplyr::select(country.name.en, iso3c, Water.withdrawal)
  
  
  ## 4. Soil carbon content
  ## Measure: Soil and land quality
  ## Original name: Average carbon content in the topsoil as a % in weight
  ## Units: (%)
  ## Years: 2008
  ## Countries with data: 201
  
  carbon_soil <- read.csv("./Input_data_final/Environment/soil_carbon.csv")
  carbon_soil <- carbon_soil %>% dplyr::select(Country.Code, Country, Value)
  colnames(carbon_soil)[3] <- "Soil.carbon.content"
  carbon_soil$Country <- as.character(carbon_soil$Country)
  carbon_soil$Country[which(carbon_soil$Country == "CÃ´te d'Ivoire")] <- "Ivory Coast"
  carbon_soil$Country[which(carbon_soil$Country == "RÃ©union")] <- "Reunion"
  carbon_soil$Country[which(carbon_soil$Country == "Sudan (former)")] <- "Sudan"
  carbon_soil$Country[which(carbon_soil$Country == "Czechia")] <- "Czech Republic"
  carbon_soil$Country[which(carbon_soil$Country == "Guinea-Bissau")] <- "Guinea Bissau"
  carbon_soil$Country[which(carbon_soil$Country == "Netherlands Antilles (former)")] <- "Netherlands Antilles"
  carbon_soil$Country[which(carbon_soil$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
  carbon_soil$Country.Code[which(carbon_soil$Country == "Sudan")] <- 276
  
  # carbon_soil %>% ggplot(aes(x = reorder(Country, Soil.carbon.content), y = Soil.carbon.content)) +
  #   geom_bar(stat = "identity") +
  #   xlab("Country") + ylab("Average carbon content in the topsoil (%)") +
  #   theme_bw() +
  #   theme(axis.text.x = element_text(angle = 90))
  
  carbon_soil <- dplyr::inner_join(x = country_codes, y = carbon_soil, by = c("country.name.en" = "Country"))
  carbon_soil <- carbon_soil %>% dplyr::select(country.name.en, iso3c, Soil.carbon.content)
  
  
  ## 5. Arable land
  ## Measure: Soil and land use
  ## Original name: Arable land
  ## Units: (%)
  ## Years: 2000:2014
  ## Countries with data: 216
  
  arable_land <- read.csv("./Input_data_final/Environment/arable_land.csv")
  arable_land <- arable_land %>% dplyr::select(Country, Year, Value)
  arable_land <- arable_land %>% filter(Year >= 2000)
  arable_land$Country <- as.character(arable_land$Country)
  arable_land$Country[which(arable_land$Country == "CÃ´te d'Ivoire")] <- "Ivory Coast"
  arable_land$Country[which(arable_land$Country == "RÃ©union")] <- "Reunion"
  arable_land$Country[which(arable_land$Country == "Wallis and Futuna Islands")] <- "Wallis and Futuna"
  arable_land$Country[which(arable_land$Country == "Guinea-Bissau")] <- "Guinea Bissau"
  arable_land$Country[which(arable_land$Country == "Czechia")] <- "Czech Republic"
  arable_land$Country[which(arable_land$Country == "Ethiopia PDR")] <- "Ethiopia"
  arable_land$Country[which(arable_land$Country == "Netherlands Antilles (former)")] <- "Netherlands Antilles"
  arable_land$Country[which(arable_land$Country == "Occupied Palestinian Territory")] <- "Palestine"
  arable_land$Country[which(arable_land$Country == "Sudan (former)")] <- "Sudan"
  arable_land$Country[which(arable_land$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
  
  colnames(arable_land)[3] <- "Arable.land"
  
  # arable_land %>% ggplot(aes(x = Year, y = Arable.land, group = Country)) +
  #   geom_line(alpha = .2) +
  #   scale_x_continuous(breaks = 2000:2014, limits = c(2000, 2014)) +
  #   theme_bw()
  
  yearsList <- arable_land$Year %>% unique %>% sort
  arable_landList <- lapply(1:length(yearsList), function(i){
    df <- arable_land %>% filter(Year == yearsList[i])
    df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
    return(df)
  })
  lapply(arable_landList, dim)
  lapply(arable_landList, function(y){apply(X = y, MARGIN = 2, FUN = function(x){sum(!is.na(x))})})
  
  recentArable_land <- arable_landList[[length(arable_landList)]]
  recentArable_land <- recentArable_land %>% dplyr::select(country.name.en, iso3c, Arable.land)
  
  
  ## 6. GEF biodiversity index
  ## Measure: Biodiversity wildlife (plants, animals)
  ## Original name: Total population with access to safe drinking-water (JMP)
  ## Units: (%)
  ## Years: 2008
  ## Countries with data: 147
  
  if(!file.exists("./Input_data_final/Environment/GEF_benefits_for_biodiversity.csv")){
    GBI <- XML::readHTMLTable("http://www.indexmundi.com/facts/indicators/ER.BDV.TOTL.XQ/rankings")
    GBI <- GBI$`NULL`
    write.csv(x = GBI, file = "./Input_data_final/Environment/GEF_benefits_for_biodiversity.csv", row.names = F)
  } else {
    GBI <- readr::read_csv(file = "./Input_data_final/Environment/GEF_benefits_for_biodiversity.csv", col_names = T)
    GBI$Rank <- GBI$Year <- NULL
  }
  
  names(GBI)[2] <- "GEF.benefits.biodiversity"
  
  # GBI %>% ggplot(aes(x = reorder(Country, GEF.benefits.biodiversity), y = GEF.benefits.biodiversity)) +
  #   geom_bar(stat = "identity") +
  #   xlab("Country") + ylab("GEF benefict for biodiversity index") +
  #   theme_bw() +
  #   theme(axis.text.x = element_text(angle = 90))
  
  GBI <- dplyr::inner_join(x = country_codes, y = GBI, by = c("country.name.en" = "Country"))
  GBI <- GBI %>% dplyr::select(country.name.en, iso3c, GEF.benefits.biodiversity)
  
  
  ## 7. Energy used in agriculture and forestry
  ## Measure: energy use
  ## Original name: Agriculture and forestry energy use as a % of total energy use
  ## Units: (%)
  ## Years: 1971:2009
  ## Countries with data: 122
  
  energy <- read.csv("./Input_data_final/Environment/energy.csv")
  energy <- energy %>% dplyr::select(Country, Year, Value)
  energy$Country <- as.character(energy$Country)
  energy$Country[which(energy$Country == "CÃ´te d'Ivoire")] <- "Ivory Coast"
  energy$Country[which(energy$Country == "Sudan (former)")] <- "Sudan"
  energy$Country[which(energy$Country == "Bolivia (Plurinational State of)")] <- "Bolivia"
  energy$Country[which(energy$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
  energy$Country[which(energy$Country == "Czechia")] <- "Czech Republic"
  colnames(energy)[3] <- "Energy.agriculture"
  energy <- energy %>% filter(Year >= 2000)
  
  # energy %>% ggplot(aes(x = Year, y = Energy.agriculture, group = Country)) +
  #   geom_line(alpha = .2) + 
  #   theme_bw()
  
  energy <- energy %>% group_by(Country) %>% summarise(Energy.agriculture = median(Energy.agriculture, na.rm = T))
  recentEnergy <- dplyr::inner_join(x = country_codes, y = energy, by = c("country.name.en" = "Country"))
  recentEnergy <- recentEnergy %>% dplyr::select(country.name.en, iso3c, Energy.agriculture)
  
  # yearsList <- energy$Year %>% unique %>% sort
  # energyList <- lapply(1:length(yearsList), function(i){
  #   df <- energy %>% filter(Year == yearsList[i])
  #   df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
  #   return(df)
  # })
  # lapply(energyList, dim)
  # lapply(energyList, function(y){apply(X = y, MARGIN = 2, FUN = function(x){sum(!is.na(x))})})
  # recentEnergy <- energyList[[length(energyList)]]
  # recentEnergy <- recentEnergy %>% dplyr::select(country.name.en, iso3c, Energy.agriculture)
  
  environmentDim <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = recentEmission, by = c("country.name.en", "iso3c"))
  environmentDim <- dplyr::left_join(x = environmentDim, y = pH, by = c("country.name.en", "iso3c"))
  environmentDim <- dplyr::left_join(x = environmentDim, y = water, by = c("country.name.en", "iso3c"))
  environmentDim <- dplyr::left_join(x = environmentDim, y = carbon_soil, by = c("country.name.en", "iso3c"))
  environmentDim <- dplyr::left_join(x = environmentDim, y = recentArable_land, by = c("country.name.en", "iso3c"))
  environmentDim <- dplyr::left_join(x = environmentDim, y = GBI, by = c("country.name.en", "iso3c"))
  environmentDim <- dplyr::left_join(x = environmentDim, y = recentEnergy, by = c("country.name.en", "iso3c"))
  
  environmentDim <- environmentDim[-which(apply(X = environmentDim[,3:ncol(environmentDim)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 7),]
  rownames(environmentDim) <- environmentDim$country.name.en
  environmentDim$country.name.en <- NULL
  
  write.csv(x = environmentDim, file = "environmental_dimension.csv", row.names = T)
  
  rm(recentEmission, recentSafe_water, water, carbon_soil, recentArable_land, GBI, recentEnergy)
  rm(emission, safe_water, arable_land, energy, dissOxygen)
  rm(arable_landList, emissionList, energyList, safe_waterList, waterList, yearsList)
} else {
  environmentDim <- read.csv("environmental_dimension.csv", row.names = 1)
}

suppressMessages(library(tabplot))
suppressMessages(library(GGally))
suppressMessages(library(corrplot))

# Distributions and missing values representation
tableplot(environmentDim[,-1], nBins = nrow(environmentDim))

# Correlation
M <- cor(environmentDim[,-1], use = "complete.obs", method = "spearman")
corrplot(M, method = "square")
plot(environmentDim$Emissions.agriculture.total, environmentDim$GBI, pch = 20)

# PCA
FactoMineR::PCA(X = environmentDim[complete.cases(environmentDim),-1])

## ========================================================================== ##
## ECONOMICS
## ========================================================================== ##

if(!file.exists("economic_dimension.csv")){
  
  ## 1. Agriculture value-added per worker
  ## Measure: Financial performance
  ## Original name: Agriculture value-added per worker (constant 2010 US$)
  ## Units: Constant 2010 US$
  ## Years: 2000:2016
  ## Countries with data: depends on the year
  ## Recent year selected with more information: 2010, 180 countries
  
  AgValueAdded <- read.csv("./Input_data_final/Economic/AgValue_added-WorldBank.csv")
  AgValueAdded$Indicator.Name <- AgValueAdded$Indicator.Code <- NULL
  names(AgValueAdded)[1:2] <- c("Country", "ISO3")
  names(AgValueAdded)[3:ncol(AgValueAdded)] <- gsub(pattern = "X", replacement = "", x = names(AgValueAdded)[3:ncol(AgValueAdded)])
  
  apply(X = AgValueAdded, MARGIN = 2, FUN = function(x){sum(!is.na(x))})
  which.max(apply(X = AgValueAdded, MARGIN = 2, FUN = function(x){sum(!is.na(x))})[-(1:2)])
  
  AgValueAdded <- AgValueAdded %>% gather(key = Year, value = AgValueAdded, 3:ncol(AgValueAdded))
  AgValueAdded$Year <- gsub(pattern = "X", replacement = "", x = AgValueAdded$Year) %>% as.character %>% as.numeric
  AgValueAdded <- AgValueAdded %>% filter(Year >= 2000)
  
  # AgValueAdded %>% ggplot(aes(x = Year, y = AgValueAdded, group = Country)) +
  #   geom_line(alpha = .5) + theme_bw()
  
  AgValueAdded$Country <- AgValueAdded$Country %>% as.character
  AgValueAdded$Country[which(AgValueAdded$Country == "Bahamas, The")] <- "Bahamas"
  AgValueAdded$Country[which(AgValueAdded$Country == "Cote d'Ivoire")] <- "Ivory Coast"
  AgValueAdded$Country[which(AgValueAdded$Country == "Congo, Dem. Rep.")] <- "Democratic Republic of the Congo"
  AgValueAdded$Country[which(AgValueAdded$Country == "Congo, Rep.")] <- "Congo"
  AgValueAdded$Country[which(AgValueAdded$Country == "Egypt, Arab Rep.")] <- "Egypt"
  AgValueAdded$Country[which(AgValueAdded$Country == "Micronesia, Fed. Sts.")] <- "Micronesia (Federated States of)"
  AgValueAdded$Country[which(AgValueAdded$Country == "Gambia, The")] <- "Gambia"
  AgValueAdded$Country[which(AgValueAdded$Country == "Guinea-Bissau")] <- "Guinea Bissau"
  AgValueAdded$Country[which(AgValueAdded$Country == "Hong Kong SAR, China")] <- "Hong Kong"
  AgValueAdded$Country[which(AgValueAdded$Country == "Iran, Islamic Rep.")] <- "Iran (Islamic Republic of)"
  AgValueAdded$Country[which(AgValueAdded$Country == "Kyrgyz Republic")] <- "Kyrgyzstan"
  AgValueAdded$Country[which(AgValueAdded$Country == "St. Kitts and Nevis")] <- "Saint Kitts and Nevis"
  AgValueAdded$Country[which(AgValueAdded$Country == "Korea, Rep.")] <- "Republic of Korea"
  AgValueAdded$Country[which(AgValueAdded$Country == "Lao PDR")] <- "Lao People's Democratic Republic"
  AgValueAdded$Country[which(AgValueAdded$Country == "St. Lucia")] <- "Saint Lucia"
  AgValueAdded$Country[which(AgValueAdded$Country == "Macao SAR, China")] <- "Macao"
  AgValueAdded$Country[which(AgValueAdded$Country == "St. Martin (French part)")] <- "Saint Martin (French part)"
  AgValueAdded$Country[which(AgValueAdded$Country == "Moldova")] <- "Republic of Moldova"
  AgValueAdded$Country[which(AgValueAdded$Country == "Macedonia, FYR")] <- "The former Yugoslav Republic of Macedonia"
  AgValueAdded$Country[grep(pattern = "Korea, Dem. People", x = AgValueAdded$Country)] <- "Democratic People's Republic of Korea"
  AgValueAdded$Country[which(AgValueAdded$Country == "West Bank and Gaza")] <- "Palestine"
  AgValueAdded$Country[which(AgValueAdded$Country == "Slovak Republic")] <- "Slovakia"
  AgValueAdded$Country[which(AgValueAdded$Country == "Tanzania")] <- "United Republic of Tanzania"
  AgValueAdded$Country[which(AgValueAdded$Country == "United States")] <- "United States of America"
  AgValueAdded$Country[which(AgValueAdded$Country == "St. Vincent and the Grenadines")] <- "Saint Vincent and the Grenadines"
  AgValueAdded$Country[which(AgValueAdded$Country == "Venezuela, RB")] <- "Venezuela"
  AgValueAdded$Country[which(AgValueAdded$Country == "Virgin Islands (U.S.)")] <- "United States Virgin Islands"
  AgValueAdded$Country[which(AgValueAdded$Country == "Vietnam")] <- "Viet Nam"
  AgValueAdded$Country[which(AgValueAdded$Country == "Yemen, Rep.")] <- "Yemen"
  AgValueAdded$ISO3 <- NULL
  
  yearsList <- AgValueAdded$Year %>% unique %>% sort
  AgValueAddedList <- lapply(1:length(yearsList), function(i){
    df <- AgValueAdded %>% filter(Year == yearsList[i])
    df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
    return(df)
  })
  lapply(AgValueAddedList, dim)
  
  recentAgValueAdded <- AgValueAddedList[[11]]
  recentAgValueAdded <- recentAgValueAdded %>% dplyr::select(country.name.en, iso3c, AgValueAdded)
  
  
  ## 2. Agriculture under-employment
  ## Measure: Employment rate
  ## Original name: Time related underemployment in agriculture
  ## Units: 
  ## Years: 2000:2014
  ## Countries with data: depends on the year
  ## Average produces more information: 2010-2014, 71 countries
  
  employment <- read.csv(file = "./Input_data_final/Economic/Employment_Indicators_E_All_Data.csv")
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
  
  # employment %>% ggplot(aes(x = Year, y = Value, colour = Indicator, group = Country)) +
  #   geom_line(alpha = .5) + facet_wrap(~Indicator, scales = "free") + 
  #   theme_bw()
  
  employment <- employment %>% spread(Indicator, Value)
  names(employment)[3:5] <- c("AgValue.added", "Time.underemployment", "Wage.employment")
  employment <- employment %>% filter(Year >= 2000 & Country != "Serbia and Montenegro")
  employment$Country <- as.character(employment$Country)
  employment$Country[which(employment$Country == "Bolivia (Plurinational State of)")] <- "Bolivia"
  employment$Country[which(employment$Country == "China, Macao SAR")] <- "Macao"
  employment$Country[which(employment$Country == "China, mainland")] <- "China"
  employment$Country[which(employment$Country == "Côte d'Ivoire")] <- "Ivory Coast"
  employment$Country[which(employment$Country == "Ethiopia PDR")] <- "Ethiopia"
  employment$Country[which(employment$Country == "Occupied Palestinian Territory")] <- "Palestine"
  employment$Country[which(employment$Country == "Réunion")] <- "Reunion"
  employment$Country[which(employment$Country == "Sudan (former)")] <- "Sudan"
  employment$Country[which(employment$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
  employment <- employment[which(employment %>% select(Country, Year) %>% duplicated() == FALSE),]
  
  TimeUnderemployment <- employment %>% select(Country, Year, Time.underemployment) %>% spread(., key = Year, value = Time.underemployment)
  apply(X = TimeUnderemployment, MARGIN = 2, FUN = function(x){sum(!is.na(x))})
  TimeUnderemployment$Time.underemployment <- rowMeans(x = TimeUnderemployment[,12:ncol(TimeUnderemployment)], na.rm = T)
  TimeUnderemployment <- TimeUnderemployment %>% select(Country, Time.underemployment)
  
  TimeUnderemployment <- dplyr::inner_join(x = country_codes, y = TimeUnderemployment, by = c("country.name.en" = "Country"))
  TimeUnderemployment <- TimeUnderemployment %>% dplyr::select(country.name.en, iso3c, Time.underemployment)
  
  
  ## 3. Wage employment distribution in agriculture
  ## Measure: Economic distribution
  ## Original name: Wage employment distribution in agriculture
  ## Units: Constant 2010 US$
  ## Years: 2000:2014
  ## Countries with data: depends on the year
  ## Average produces more information: 2009-2013, 117 countries
  
  WageEmployment <- employment %>% select(Country, Year, Wage.employment) %>% spread(., key = Year, value = Wage.employment)
  apply(X = WageEmployment, MARGIN = 2, FUN = function(x){sum(!is.na(x))})
  WageEmployment$Wage.employment <- rowMeans(x = WageEmployment[,11:15], na.rm = T)
  WageEmployment <- WageEmployment %>% select(Country, Wage.employment)
  
  WageEmployment <- dplyr::inner_join(x = country_codes, y = WageEmployment, by = c("country.name.en" = "Country"))
  WageEmployment <- WageEmployment %>% dplyr::select(country.name.en, iso3c, Wage.employment)
  
  
  economicDim <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = recentAgValueAdded, by = c("country.name.en", "iso3c"))
  economicDim <- dplyr::left_join(x = economicDim, y = TimeUnderemployment, by = c("country.name.en", "iso3c"))
  economicDim <- dplyr::left_join(x = economicDim, y = WageEmployment, by = c("country.name.en", "iso3c"))
  
  economicDim <- economicDim[-which(apply(X = economicDim[,3:ncol(economicDim)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 3),]
  rownames(economicDim) <- economicDim$country.name.en
  economicDim$country.name.en <- NULL
  
  write.csv(x = economicDim, file = "economic_dimension.csv", row.names = T)
  
  rm(AgValueAdded, AgValueAddedList, employment, recentAgValueAdded, WageEmployment, TimeUnderemployment, yearsList)
  
} else {
  economicDim <- read.csv("economic_dimension.csv")
}

# Distributions and missing values representation
tableplot(economicDim[,-1], nBins = nrow(economicDim))

# Correlation
M <- cor(economicDim[,-1], use = "complete.obs", method = "spearman")
corrplot(M, method = "square")

# PCA
FactoMineR::PCA(X = economicDim[complete.cases(economicDim),-1])


## ========================================================================== ##
## SOCIAL
## ========================================================================== ##

if(!file.exists("social_dimension.csv")){
  
  ## 1. Employment in agriculture female (% of female employment)
  ## Measure: Gender/Equity
  ## Original name: Employment in agriculture female (% of female employment)
  ## Units: (%)
  ## Years: 1990:2010 (Selected period: 2000:2016)
  ## Recent year selected with more information: 2016, 184 countries
  
  FemaleLaborForce <- read_csv(file = "./Input_data_final/Social/Labor_force_female.csv", col_names = T, skip = 4)
  FemaleLaborForce$`Country Code` <- NULL
  FemaleLaborForce$`Indicator Name` <- NULL
  FemaleLaborForce$`Indicator Code` <- NULL
  names(FemaleLaborForce)[1] <- "Country"
  
  apply(X = FemaleLaborForce, MARGIN = 2, FUN = function(x){sum(!is.na(x))})
  
  FemaleLaborForce <- FemaleLaborForce %>% gather(Year, Female.labor.force, 2:ncol(FemaleLaborForce))
  FemaleLaborForce$Country <- FemaleLaborForce$Country %>% as.character
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Bahamas, The")] <- "Bahamas"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Cote d'Ivoire")] <- "Ivory Coast"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Congo, Dem. Rep.")] <- "Democratic Republic of the Congo"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Congo, Rep.")] <- "Congo"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Egypt, Arab Rep.")] <- "Egypt"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Micronesia, Fed. Sts.")] <- "Micronesia (Federated States of)"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Gambia, The")] <- "Gambia"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Guinea-Bissau")] <- "Guinea Bissau"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Hong Kong SAR, China")] <- "Hong Kong"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Iran, Islamic Rep.")] <- "Iran (Islamic Republic of)"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Kyrgyz Republic")] <- "Kyrgyzstan"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "St. Kitts and Nevis")] <- "Saint Kitts and Nevis"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Korea, Rep.")] <- "Republic of Korea"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Lao PDR")] <- "Lao People's Democratic Republic"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "St. Lucia")] <- "Saint Lucia"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Macao SAR, China")] <- "Macao"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "St. Martin (French part)")] <- "Saint Martin (French part)"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Moldova")] <- "Republic of Moldova"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Macedonia, FYR")] <- "The former Yugoslav Republic of Macedonia"
  FemaleLaborForce$Country[grep(pattern = "Korea, Dem. People", x = FemaleLaborForce$Country)] <- "Democratic People's Republic of Korea"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "West Bank and Gaza")] <- "Palestine"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Slovak Republic")] <- "Slovakia"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Tanzania")] <- "United Republic of Tanzania"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "United States")] <- "United States of America"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "St. Vincent and the Grenadines")] <- "Saint Vincent and the Grenadines"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Venezuela, RB")] <- "Venezuela"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Virgin Islands (U.S.)")] <- "United States Virgin Islands"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Vietnam")] <- "Viet Nam"
  FemaleLaborForce$Country[which(FemaleLaborForce$Country == "Yemen, Rep.")] <- "Yemen"
  
  # FemaleLaborForce %>% ggplot(aes(x = Year, y = Female.labor.force, group = Country)) +
  #   geom_line(alpha = .5) + theme_bw()
  
  yearsList <- FemaleLaborForce$Year %>% unique %>% sort
  FemaleLaborForceList <- lapply(1:length(yearsList), function(i){
    df <- FemaleLaborForce %>% filter(Year == yearsList[i])
    df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
    return(df)
  })
  lapply(FemaleLaborForceList, dim)
  
  recentFemaleLaborForce <- FemaleLaborForceList[[length(FemaleLaborForceList)]]
  recentFemaleLaborForce <- recentFemaleLaborForce %>% dplyr::select(country.name.en, iso3c, Female.labor.force)
  
  
  ## 2. Predominant fair trade organizations and producers
  ## Measure: Inclusion
  ## Original name: Fairtrade around the world
  ## Units: Categorical variable: (1: Fairtrade Organization; 2: Fairtrade Producer Network; 3: Fairtrade Organization & Producer Network)
  ## Years: 2016?
  ## Countries with data: 160
  
  FairTrade <- read_csv("./Input_data_final/Social/Fairtrade_data-pOSAp.csv", col_names = T)
  FairTrade <- FairTrade %>% dplyr::select(Country, data)
  names(FairTrade)[2] <- "Fairtrade.org"
  FairTrade$Country <- as.character(FairTrade$Country)
  FairTrade$Country[which(FairTrade$Country == "Cape Verde")] <- "Cabo Verde"
  FairTrade$Country[which(FairTrade$Country == "Federated States of Micronesia")] <- "Micronesia (Federated States of)"
  FairTrade$Country[which(FairTrade$Country == "Iran")] <- "Iran (Islamic Republic of)"
  FairTrade$Country[which(FairTrade$Country == "Laos")] <- "Lao People's Democratic Republic"
  FairTrade$Country[which(FairTrade$Country == "Republic of the Congo")] <- "Congo"
  FairTrade$Country[which(FairTrade$Country == "São Tomé and Principe")] <- "Sao Tome and Principe"
  FairTrade$Country[which(FairTrade$Country == "South Korea")] <- "Republic of Korea"
  FairTrade$Country[which(FairTrade$Country == "Taiwan")] <- "Taiwan, Province of China"
  FairTrade$Country[which(FairTrade$Country == "Vietnam")] <- "Viet Nam"
  
  FairTrade <- dplyr::inner_join(x = country_codes, y = FairTrade, by = c("country.name.en" = "Country"))
  FairTrade <- FairTrade %>% dplyr::select(country.name.en, iso3c, Fairtrade.org)
  
  
  ## 3. Employment in agriculture
  ## Measure: Inclusion
  ## Original name: Employment in agriculture (% of total employment)
  ## Units: (%)
  ## Years: 2008:2016
  ## Countries with data: depends on the year
  ## Average produces more information: 2011-2015, 120 countries
  
  AgEmployment <- readxl::read_excel(path = "./Input_data_final/Social/employment_agriculture.xlsx", sheet = 1)
  AgEmployment <- AgEmployment[1:217,]
  AgEmployment$`Series Name` <- NULL
  AgEmployment$`Series Code` <- NULL
  AgEmployment$`Country Code` <- NULL
  AgEmployment$`1990` <- NULL
  names(AgEmployment)[1] <- "Country"
  
  apply(X = AgEmployment, MARGIN = 2, FUN = function(x){sum(!is.na(x))})
  which.max(apply(X = AgEmployment, MARGIN = 2, FUN = function(x){sum(!is.na(x))})[-(1:2)])
  
  AgEmployment$Agr.employment <- rowMeans(x = AgEmployment[,6:10], na.rm = T)
  AgEmployment <- AgEmployment %>% select(Country, Agr.employment)
  AgEmployment$Country <- AgEmployment$Country %>% as.character
  AgEmployment$Country[which(AgEmployment$Country == "Bahamas, The")] <- "Bahamas"
  AgEmployment$Country[which(AgEmployment$Country == "Cote d'Ivoire")] <- "Ivory Coast"
  AgEmployment$Country[which(AgEmployment$Country == "Congo, Dem. Rep.")] <- "Democratic Republic of the Congo"
  AgEmployment$Country[which(AgEmployment$Country == "Congo, Rep.")] <- "Congo"
  AgEmployment$Country[which(AgEmployment$Country == "Egypt, Arab Rep.")] <- "Egypt"
  AgEmployment$Country[which(AgEmployment$Country == "Micronesia, Fed. Sts.")] <- "Micronesia (Federated States of)"
  AgEmployment$Country[which(AgEmployment$Country == "Gambia, The")] <- "Gambia"
  AgEmployment$Country[which(AgEmployment$Country == "Guinea-Bissau")] <- "Guinea Bissau"
  AgEmployment$Country[which(AgEmployment$Country == "Hong Kong SAR, China")] <- "Hong Kong"
  AgEmployment$Country[which(AgEmployment$Country == "Iran, Islamic Rep.")] <- "Iran (Islamic Republic of)"
  AgEmployment$Country[which(AgEmployment$Country == "Kyrgyz Republic")] <- "Kyrgyzstan"
  AgEmployment$Country[which(AgEmployment$Country == "St. Kitts and Nevis")] <- "Saint Kitts and Nevis"
  AgEmployment$Country[which(AgEmployment$Country == "Korea, Rep.")] <- "Republic of Korea"
  AgEmployment$Country[which(AgEmployment$Country == "Lao PDR")] <- "Lao People's Democratic Republic"
  AgEmployment$Country[which(AgEmployment$Country == "St. Lucia")] <- "Saint Lucia"
  AgEmployment$Country[which(AgEmployment$Country == "Macao SAR, China")] <- "Macao"
  AgEmployment$Country[which(AgEmployment$Country == "St. Martin (French part)")] <- "Saint Martin (French part)"
  AgEmployment$Country[which(AgEmployment$Country == "Moldova")] <- "Republic of Moldova"
  AgEmployment$Country[which(AgEmployment$Country == "Macedonia, FYR")] <- "The former Yugoslav Republic of Macedonia"
  AgEmployment$Country[which(AgEmployment$Country == "Korea, Dem. People's Rep.")] <- "Democratic People's Republic of Korea"
  AgEmployment$Country[which(AgEmployment$Country == "West Bank and Gaza")] <- "Palestine"
  AgEmployment$Country[which(AgEmployment$Country == "Slovak Republic")] <- "Slovakia"
  AgEmployment$Country[which(AgEmployment$Country == "Tanzania")] <- "United Republic of Tanzania"
  AgEmployment$Country[which(AgEmployment$Country == "United States")] <- "United States of America"
  AgEmployment$Country[which(AgEmployment$Country == "St. Vincent and the Grenadines")] <- "Saint Vincent and the Grenadines"
  AgEmployment$Country[which(AgEmployment$Country == "Venezuela, RB")] <- "Venezuela"
  AgEmployment$Country[which(AgEmployment$Country == "Virgin Islands (U.S.)")] <- "United States Virgin Islands"
  AgEmployment$Country[which(AgEmployment$Country == "Vietnam")] <- "Viet Nam"
  AgEmployment$Country[which(AgEmployment$Country == "Yemen, Rep.")] <- "Yemen"
  
  AgEmployment <- dplyr::inner_join(x = country_codes, y = AgEmployment, by = c("country.name.en" = "Country"))
  AgEmployment <- AgEmployment %>% dplyr::select(country.name.en, iso3c, Agr.employment)
  
  
  socialDim <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = recentFemaleLaborForce, by = c("country.name.en", "iso3c"))
  socialDim <- dplyr::left_join(x = socialDim, y = FairTrade, by = c("country.name.en", "iso3c"))
  socialDim <- dplyr::left_join(x = socialDim, y = AgEmployment, by = c("country.name.en", "iso3c"))
  
  socialDim <- socialDim[-which(apply(X = socialDim[,3:ncol(socialDim)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 3),]
  rownames(socialDim) <- socialDim$country.name.en
  socialDim$country.name.en <- NULL
  
  write.csv(x = socialDim, file = "social_dimension.csv", row.names = T)
  
  rm(AgEmployment, FemaleLaborForce, FemaleLaborForceList, recentFemaleLaborForce, FairTrade, yearsList)
  
} else {
  socialDim <- read.csv("social_dimension.csv", row.names = 1)
}

# Distributions and missing values representation
tableplot(socialDim[,-1], nBins = nrow(socialDim))

# Correlation
M <- cor(socialDim[,-1], use = "complete.obs", method = "spearman")
corrplot(M, method = "square"); rm(M)

# PCA
FactoMineR::PCA(X = socialDim[complete.cases(socialDim),-1])


## ========================================================================== ##
## FOOD AND NUTRITION
## ========================================================================== ##

if(!file.exists("food_nutrition_dimension.csv")){
  
  ## 1. Food available for human consumption
  ## Measure: Food security availability
  ## Original name: Per capita food available for human consumption
  ## Units: ???
  ## Years: 2016
  ## Countries with data: 113
  
  # GFSI indices 2016
  gfsi <- readxl::read_excel(path = "./Input_data_final/Food_Nutrition/GFSI_2016.xlsx", sheet = 2, skip = 5)
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
  gfsi$Country[which(gfsi$Country == "Bolivia (Plurinational State of)")] <- "Bolivia"
  gfsi$Country[which(gfsi$Country == "Congo (Dem. Rep.)")] <- "Congo"
  gfsi$Country[grep(pattern = "^Cote", x = gfsi$Country)] <- "Ivory Coast"
  gfsi$Country[which(gfsi$Country == "Czechia")] <- "Czech Republic"
  gfsi$Country[which(gfsi$Country == "Laos")] <- "Lao People's Democratic Republic"
  gfsi$Country[which(gfsi$Country == "Russia")] <- "Russian Federation"
  gfsi$Country[which(gfsi$Country == "South Korea")] <- "Republic of Korea"
  gfsi$Country[which(gfsi$Country == "Syria")] <- "Syrian Arab Republic"
  gfsi$Country[which(gfsi$Country == "Tanzania")] <- "United Republic of Tanzania"
  gfsi$Country[which(gfsi$Country == "United States")] <- "United States of America"
  gfsi$Country[which(gfsi$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
  gfsi$Country[which(gfsi$Country == "Vietnam")] <- "Viet Nam"
  
  AverageFoodSupply <- gfsi[,c(1, 8)]
  names(AverageFoodSupply)[2] <- "Food.available"
  AverageFoodSupply <- dplyr::inner_join(x = country_codes, y = AverageFoodSupply, by = c("country.name.en" = "Country"))
  AverageFoodSupply <- AverageFoodSupply %>% dplyr::select(country.name.en, iso3c, Food.available)
  
  
  ## 2. Food consumption
  ## Measure: Food security access
  ## Original name: Food consumption as share of total income
  ## Units: ???
  ## Years: 2016
  ## Countries with data: 113
  
  FoodConsumption <- gfsi[,c(1, 2)]
  names(FoodConsumption)[2] <- "Food.consumption"
  FoodConsumption <- dplyr::inner_join(x = country_codes, y = FoodConsumption, by = c("country.name.en" = "Country"))
  FoodConsumption <- FoodConsumption %>% dplyr::select(country.name.en, iso3c, Food.consumption)
  rm(gfsi)
  
  
  ## 3. City access
  ## Measure: Food security access
  ## Original name: Estimated travel time to the nearest city of 50,000 or more people in year 2000
  ## Units: minutes of travel time
  ## Years: 2000
  ## Countries with data: 247
  
  city_access <- read.table("./Input_data_final/Food_Nutrition/countries_access.txt", header = T, sep = ",")
  names(city_access)[ncol(city_access)] <- "City.access"
  
  city_access <- dplyr::inner_join(x = country_codes, y = city_access, by = c("iso3c" = "ISO3"))
  city_access <- city_access %>% dplyr::select(country.name.en, iso3c, City.access)
  
  
  ## 4. Access to improved water resource
  ## Measure: Food security utilization
  ## Original name: Access to improved water resource
  ## Units: (%)
  ## Years: 1995:2014 (Selected period: 2000:2014)
  ## Countries with data: depends on the year
  ## Average produces more information: 2010-2014, 195 countries
  
  improved_water <- read_csv(file = "./Input_data_final/Food_Nutrition/Access_improved_water.csv", col_names = T)
  improved_water <- improved_water %>% select(Country, Year, Value)
  names(improved_water)[3] <- "Access.improved.water"
  
  improved_water <- improved_water %>% tidyr::spread(key = Year, value = Access.improved.water)
  apply(X = improved_water, MARGIN = 2, FUN = function(x){sum(!is.na(x))})
  
  improved_water$Access.improved.water <- rowMeans(improved_water[,which(names(improved_water)=="2010"):which(names(improved_water)=="2014")], na.rm = T)
  improved_water$Country <- as.character(improved_water$Country)
  improved_water$Country[which(improved_water$Country == "Bolivia (Plurinational State of)")] <- "Bolivia"
  improved_water$Country[which(improved_water$Country == "Côte d'Ivoire")] <- "Ivory Coast"
  improved_water$Country[which(improved_water$Country == "Czechia")] <- "Czech Republic"
  improved_water$Country[which(improved_water$Country == "Guinea-Bissau")] <- "Guinea Bissau"
  improved_water$Country[which(improved_water$Country == "Holy See")] <- "Holy See (Vatican City State)"
  improved_water$Country[which(improved_water$Country == "Netherlands Antilles (former)")] <- "Netherlands Antilles"
  improved_water$Country[which(improved_water$Country == "Réunion")] <- "Reunion"
  improved_water$Country[which(improved_water$Country == "Sudan (former)")] <- "Sudan"
  improved_water$Country[which(improved_water$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
  improved_water$Country[which(improved_water$Country == "Wallis and Futuna Islands")] <- "Wallis and Futuna"
  improved_water$Country[which(improved_water$Country == "West Bank and Gaza Strip")] <- "Palestine"
  improved_water <- improved_water %>% filter(Country != "Serbia and Montenegro")
  
  improved_water <- dplyr::inner_join(x = country_codes, y = improved_water, by = c("country.name.en" = "Country"))
  improved_water <- improved_water %>% dplyr::select(country.name.en, iso3c, Access.improved.water)
  
  
  ## 5. Access to electricity
  ## Measure: Food security utilization
  ## Original name: Access to electricity
  ## Units: (%)
  ## Years: 1990:2015 (Selected period: 2000:2014)
  ## Countries with data: 212
  
  access_electricity <- readxl::read_xls(path = "./Input_data_final/Food_Nutrition/Access_to_electricity.xls", sheet = 1, col_names = T, skip = 3)
  names(access_electricity)[1:2] <- c("Country", "ISO3")
  access_electricity <- access_electricity %>% select(Country, ISO3, which(names(access_electricity)=="2000"):which(names(access_electricity)=="2014"))
  
  apply(X = access_electricity, MARGIN = 2, FUN = function(x){sum(!is.na(x))})
  
  access_electricity <- access_electricity %>% gather(Year, Value, -(Country:ISO3))
  access_electricity <- access_electricity %>% select(ISO3, Country, Year, Value)
  names(access_electricity)[4] <- "Access.electricity"
  
  access_electricity$Country <- as.character(access_electricity$Country)
  access_electricity$Country[which(access_electricity$Country == "Bahamas, The")] <- "Bahamas"
  access_electricity$Country[which(access_electricity$Country == "Cote d'Ivoire")] <- "Ivory Coast"
  access_electricity$Country[which(access_electricity$Country == "Congo, Dem. Rep.")] <- "Democratic Republic of the Congo"
  access_electricity$Country[which(access_electricity$Country == "Congo, Rep.")] <- "Congo"
  access_electricity$Country[which(access_electricity$Country == "Egypt, Arab Rep.")] <- "Egypt"
  access_electricity$Country[which(access_electricity$Country == "Micronesia, Fed. Sts.")] <- "Micronesia (Federated States of)"
  access_electricity$Country[which(access_electricity$Country == "Gambia, The")] <- "Gambia"
  access_electricity$Country[which(access_electricity$Country == "Guinea-Bissau")] <- "Guinea Bissau"
  access_electricity$Country[which(access_electricity$Country == "Hong Kong SAR, China")] <- "Hong Kong"
  access_electricity$Country[which(access_electricity$Country == "Iran, Islamic Rep.")] <- "Iran (Islamic Republic of)"
  access_electricity$Country[which(access_electricity$Country == "Kyrgyz Republic")] <- "Kyrgyzstan"
  access_electricity$Country[which(access_electricity$Country == "St. Kitts and Nevis")] <- "Saint Kitts and Nevis"
  access_electricity$Country[which(access_electricity$Country == "Korea, Rep.")] <- "Republic of Korea"
  access_electricity$Country[which(access_electricity$Country == "Lao PDR")] <- "Lao People's Democratic Republic"
  access_electricity$Country[which(access_electricity$Country == "St. Lucia")] <- "Saint Lucia"
  access_electricity$Country[which(access_electricity$Country == "Macao SAR, China")] <- "Macao"
  access_electricity$Country[which(access_electricity$Country == "St. Martin (French part)")] <- "Saint Martin (French part)"
  access_electricity$Country[which(access_electricity$Country == "Moldova")] <- "Republic of Moldova"
  access_electricity$Country[which(access_electricity$Country == "Macedonia, FYR")] <- "The former Yugoslav Republic of Macedonia"
  access_electricity$Country[grep(pattern = "Korea, Dem. People", x = access_electricity$Country)] <- "Democratic People's Republic of Korea"
  access_electricity$Country[which(access_electricity$Country == "West Bank and Gaza")] <- "Palestine"
  access_electricity$Country[which(access_electricity$Country == "Slovak Republic")] <- "Slovakia"
  access_electricity$Country[which(access_electricity$Country == "Tanzania")] <- "United Republic of Tanzania"
  access_electricity$Country[which(access_electricity$Country == "United States")] <- "United States of America"
  access_electricity$Country[which(access_electricity$Country == "St. Vincent and the Grenadines")] <- "Saint Vincent and the Grenadines"
  access_electricity$Country[which(access_electricity$Country == "Venezuela, RB")] <- "Venezuela"
  access_electricity$Country[which(access_electricity$Country == "Virgin Islands (U.S.)")] <- "United States Virgin Islands"
  access_electricity$Country[which(access_electricity$Country == "Vietnam")] <- "Viet Nam"
  access_electricity$Country[which(access_electricity$Country == "Yemen, Rep.")] <- "Yemen"
  
  # access_electricity %>% ggplot(aes(x = Year, y = Access.electricity, group = Country)) +
  #   geom_line(alpha = .2) + 
  #   theme_bw()
  
  yearsList <- access_electricity$Year %>% unique %>% sort
  yearsList <- yearsList[-((length(yearsList)-1):length(yearsList))]
  access_electricityList <- lapply(1:length(yearsList), function(i){
    df <- access_electricity %>% filter(Year == yearsList[i])
    df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
    return(df)
  })
  lapply(access_electricityList, dim)
  
  recentAccessElectricity <- access_electricityList[[length(access_electricityList)]]
  recentAccessElectricity <- recentAccessElectricity %>% dplyr::select(country.name.en, iso3c, Access.electricity)
  rm(access_electricity, access_electricityList)
  
  
  ## 6. Price volatility index
  ## Measure: Food security stability
  ## Original name: Price volatility index
  ## Units: (CV %)
  ## Years: 2011:2017
  ## Countries with data: depends on the year
  ## Average produces more information: 2013-2017, 195 countries
  
  price_volatility <- read_csv(file = "./Input_data_final/Food_Nutrition/Price_volatility_index.csv", col_names = T)
  price_volatility <- price_volatility %>% select(Country, Year, 4)
  names(price_volatility)[3] <- "Price.volatility.index"
  
  price_volatility <- price_volatility %>% tidyr::spread(key = Year, value = Price.volatility.index)
  apply(X = price_volatility, MARGIN = 2, FUN = function(x){sum(!is.na(x))})
  price_volatility$Price.volatility.index <- rowMeans(price_volatility[,which(names(price_volatility)=="2013"):which(names(price_volatility)=="2017")], na.rm = T)
  
  price_volatility$Country <- as.character(price_volatility$Country)
  price_volatility$Country[which(price_volatility$Country == "Sint Maarten (Dutch Part)")] <- "Sint Maarten (Dutch part)"
  price_volatility$Country[which(price_volatility$Country == "Netherlands Antilles (former)")] <- "Netherlands Antilles"
  price_volatility$Country[grep(pattern = "Cura", x = price_volatility$Country)] <- "Curacao"
  price_volatility$Country[which(price_volatility$Country == "China, mainland")] <- "China"
  price_volatility$Country[which(price_volatility$Country == "China, Hong Kong SAR")] <- "Hong Kong"
  price_volatility$Country[which(price_volatility$Country == "China, Macao SAR")] <- "Macao"
  price_volatility$Country[which(price_volatility$Country == "Czechia")] <- "Czech Republic"
  price_volatility$Country[grep(pattern = "land Islands", x = price_volatility$Country)] <- "Aland Islands"
  price_volatility$Country[which(price_volatility$Country == "Bolivia (Plurinational State of)")] <- "Bolivia"
  price_volatility$Country[which(price_volatility$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
  price_volatility$Country[grep(pattern = "union", x = price_volatility$Country)] <- "Reunion"
  price_volatility$Country[grep(pattern = "d'Ivoire", x = price_volatility$Country)] <- "Ivory Coast"
  price_volatility$Country[which(price_volatility$Country == "Guinea-Bissau")] <- "Guinea Bissau"
  price_volatility$Country[which(price_volatility$Country == "Occupied Palestinian Territory")] <- "Palestine"
  
  price_volatility <- dplyr::inner_join(x = country_codes, y = price_volatility, by = c("country.name.en" = "Country"))
  price_volatility <- price_volatility %>% dplyr::select(country.name.en, iso3c, Price.volatility.index)
  
  
  ## 7. Food supply variability
  ## Measure: Food security stability
  ## Original name: Per capita food supply variability
  ## Units: ??
  ## Years: (Selected period: 2000:2011)
  ## Countries with data: 162
  
  fsvar <- read_csv(file = "./Input_data_final/Food_Nutrition/Food_supply_variability.csv", col_names = T)
  fsvar <- fsvar %>% select(Country, Year, Value)
  names(fsvar)[3] <- "Food.supply.variability"
  fsvar$Country <- as.character(fsvar$Country)
  fsvar$Country[which(fsvar$Country == "Bolivia (Plurinational State of)")] <- "Bolivia"
  fsvar$Country[which(fsvar$Country == "Côte d'Ivoire")] <- "Ivory Coast"
  fsvar$Country[which(fsvar$Country == "Czechia")] <- "Czech Republic"
  fsvar$Country[which(fsvar$Country == "Guinea-Bissau")] <- "Guinea Bissau"
  fsvar$Country[which(fsvar$Country == "Holy See")] <- "Holy See (Vatican City State)"
  fsvar$Country[which(fsvar$Country == "Netherlands Antilles (former)")] <- "Netherlands Antilles"
  fsvar$Country[which(fsvar$Country == "Réunion")] <- "Reunion"
  fsvar$Country[which(fsvar$Country == "Sudan (former)")] <- "Sudan"
  fsvar$Country[which(fsvar$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
  fsvar$Country[which(fsvar$Country == "Wallis and Futuna Islands")] <- "Wallis and Futuna"
  fsvar$Country[which(fsvar$Country == "West Bank and Gaza Strip")] <- "Palestine"
  fsvar <- fsvar %>% filter(Country != "Serbia and Montenegro" & Year >= 2000)
  fsvar <- fsvar[!(fsvar$Country == "Sudan" & is.na(fsvar$Food.supply.variability)),]
  fsvar <- fsvar %>% filter(Year <= 2011)
  
  # fsvar %>% ggplot(aes(x = Year, y = Food.supply.variability, group = Country)) +
  #   geom_line(alpha = .2) + 
  #   theme_bw()
  
  yearsList <- fsvar$Year %>% unique %>% sort
  fsvarList <- lapply(1:length(yearsList), function(i){
    df <- fsvar %>% filter(Year == yearsList[i])
    df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
    return(df)
  })
  lapply(fsvarList, dim)
  
  recentFoodSupplyVar <- fsvarList[[length(fsvarList)]]
  recentFoodSupplyVar <- recentFoodSupplyVar %>% dplyr::select(country.name.en, iso3c, Food.supply.variability)
  rownames(recentFoodSupplyVar) <- 1:nrow(recentFoodSupplyVar)
  
  rm(fsvar, fsvarList)
  
  
  ## 8. Burden of food borne illness
  ## Measure: Food safety
  ## Original name: Burden of food borne illness
  ## Units: ??
  ## Years: 2010
  ## Countries with data: 194
  
  foodBorne_illness <- read_csv(file = "./Input_data_final/Food_Nutrition/Foodborne_illness.csv", col_names = T)
  foodBorne_illness$Region <- NULL
  foodBorne_illness$Country <- as.character(foodBorne_illness$Country)
  foodBorne_illness$Country[which(foodBorne_illness$Country == "Guinea-Bissau")] <- "Guinea Bissau"
  foodBorne_illness$Country[which(foodBorne_illness$Country == "The Former Yugoslav Republic of Macedonia")] <- "The former Yugoslav Republic of Macedonia"
  
  foodBorne_illness <- dplyr::inner_join(x = country_codes, y = foodBorne_illness, by = c("country.name.en" = "Country"))
  foodBorne_illness <- foodBorne_illness %>% dplyr::select(country.name.en, iso3c, Foodborne.illness)
  
  
  ## 9. Food loss
  ## Measure: Food waste and use
  ## Original name: Total waste/total domestic supply quantity (tonnes)
  ## Units: (%)
  ## Years: 2016
  ## Countries with data: 113
  
  food_loss <- read_csv(file = "./Input_data_final/Food_Nutrition/Food_loss.csv", col_names = T)
  food_loss$Country <- as.character(food_loss$Country)
  food_loss$Country[which(food_loss$Country == "Congo (Dem. Rep.)")] <- "Democratic Republic of the Congo"
  food_loss$Country[grep(pattern = "Ivoire", x = food_loss$Country)] <- "Ivory Coast"
  food_loss$Country[which(food_loss$Country == "Laos")] <- "Lao People's Democratic Republic"
  food_loss$Country[which(food_loss$Country == "Russia")] <- "Russian Federation"
  food_loss$Country[which(food_loss$Country == "South Korea")] <- "Republic of Korea"
  food_loss$Country[which(food_loss$Country == "Syria")] <- "Syrian Arab Republic"
  food_loss$Country[which(food_loss$Country == "Tanzania")] <- "United Republic of Tanzania"
  food_loss$Country[which(food_loss$Country == "United States")] <- "United States of America"
  food_loss$Country[which(food_loss$Country == "Vietnam")] <- "Viet Nam"
  
  food_loss <- dplyr::inner_join(x = country_codes, y = food_loss, by = c("country.name.en" = "Country"))
  food_loss <- food_loss %>% dplyr::select(country.name.en, iso3c, Food.loss)
  
  
  ## 10. Diet diversification
  ## Measure: Nutrition diet
  ## Original name: Diet diversification
  ## Units: (%)
  ## Years: 2001:2010
  ## Countries with data: 165
  
  diet_div <- read_csv(file = "./Input_data_final/Food_Nutrition/Diet_diversification.csv", col_names = T)
  diet_div <- diet_div %>% select(Country, Year, Value)
  names(diet_div)[3] <- "Diet.diversification"
  diet_div$Country <- as.character(diet_div$Country)
  diet_div$Country[which(diet_div$Country == "Côte d'Ivoire")] <- "Ivory Coast"
  diet_div$Country[which(diet_div$Country == "Bolivia (Plurinational State of)")] <- "Bolivia"
  diet_div$Country[which(diet_div$Country == "Czechia")] <- "Czech Republic"
  diet_div$Country[which(diet_div$Country == "Guinea-Bissau")] <- "Guinea Bissau"
  diet_div$Country[which(diet_div$Country == "Holy See")] <- "Holy See (Vatican City State)"
  diet_div$Country[which(diet_div$Country == "Netherlands Antilles (former)")] <- "Netherlands Antilles"
  diet_div$Country[which(diet_div$Country == "Réunion")] <- "Reunion"
  diet_div$Country[which(diet_div$Country == "Sudan (former)")] <- "Sudan"
  diet_div$Country[which(diet_div$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
  diet_div$Country[which(diet_div$Country == "Wallis and Futuna Islands")] <- "Wallis and Futuna"
  diet_div$Country[which(diet_div$Country == "West Bank and Gaza Strip")] <- "Palestine"
  diet_div <- diet_div %>% filter(Country != "Serbia and Montenegro" & Year >= 2000)
  diet_div$Year <- as.character(diet_div$Year)
  diet_div$Year[which(diet_div$Year == "2000-2002")] <- "2001"
  diet_div$Year[which(diet_div$Year == "2001-2003")] <- "2002"
  diet_div$Year[which(diet_div$Year == "2002-2004")] <- "2003"
  diet_div$Year[which(diet_div$Year == "2003-2005")] <- "2004"
  diet_div$Year[which(diet_div$Year == "2004-2006")] <- "2005"
  diet_div$Year[which(diet_div$Year == "2005-2007")] <- "2006"
  diet_div$Year[which(diet_div$Year == "2006-2008")] <- "2007"
  diet_div$Year[which(diet_div$Year == "2007-2009")] <- "2008"
  diet_div$Year[which(diet_div$Year == "2008-2010")] <- "2009"
  diet_div$Year[which(diet_div$Year == "2009-2011")] <- "2010"
  diet_div$Year[which(diet_div$Year == "2010-2012")] <- "2011"
  diet_div$Year[which(diet_div$Year == "2011-2013")] <- "2012"
  diet_div$Year[which(diet_div$Year == "2012-2014")] <- "2013"
  diet_div$Year[which(diet_div$Year == "2013-2015")] <- "2014"
  diet_div$Year[which(diet_div$Year == "2014-2016")] <- "2015"
  diet_div$Year <- as.numeric(diet_div$Year)
  diet_div <- diet_div %>% filter(Year <= 2010)
  diet_div <- diet_div[!(diet_div$Country == "Sudan" & is.na(diet_div$Diet.diversification)),]
  
  # diet_div %>% ggplot(aes(x = Year, y = Diet.diversification, group = Country)) +
  #   geom_line(alpha = .2) + 
  #   theme_bw()
  
  yearsList <- diet_div$Year %>% unique %>% sort
  diet_divList <- lapply(1:length(yearsList), function(i){
    df <- diet_div %>% filter(Year == yearsList[i])
    df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
    return(df)
  })
  lapply(diet_divList, dim)
  
  recentDietDiv <- diet_divList[[length(diet_divList)]]
  recentDietDiv <- recentDietDiv %>% dplyr::select(country.name.en, iso3c, Diet.diversification)
  rownames(recentDietDiv) <- 1:nrow(recentDietDiv)
  
  rm(diet_div, diet_divList)
  
  
  ## 11. Crop diversity
  ## Measure: Nutrition and diet
  ## Original name: Crop diversity
  ## Units: Shannon diversity index
  ## Years: 2009:2011
  ## Countries with data: 177
  
  crop_diversity <- read_csv(file = "./Input_data_final/Food_Nutrition/Diversity_indexes_colin_study.csv", col_names = T)
  
  # crop_diversity %>% ggplot(aes(x = Year, y = Diversity, group = Country)) +
  #   facet_grid(Index~Measurement, scales = "free") + geom_line(alpha = .2) + theme_bw()
  
  crop_diversity <- crop_diversity %>% filter(Index == "Shannon" & Measurement == "Calories")
  crop_diversity <- crop_diversity %>% select(Country, Year, Diversity)
  names(crop_diversity)[ncol(crop_diversity)] <- "Crop.diversity"
  crop_diversity <- crop_diversity %>% spread(key = Year, value = Crop.diversity)
  crop_diversity$Crop.diversity <- rowMeans(x = crop_diversity[,2:ncol(crop_diversity)], na.rm = T)
  crop_diversity <- crop_diversity %>% select(Country, Crop.diversity)
  crop_diversity$Country <- crop_diversity$Country %>% as.character
  crop_diversity$Country[which(crop_diversity$Country == "Bolivia (Plurinational State of)")] <- "Bolivia"
  crop_diversity$Country[which(crop_diversity$Country == "China, Hong Kong SAR")] <- "Hong Kong"
  crop_diversity$Country[which(crop_diversity$Country == "China, Macao SAR")] <- "Macao"
  crop_diversity$Country[which(crop_diversity$Country == "China, mainland")] <- "China"
  crop_diversity$Country[which(crop_diversity$Country == "China, Taiwan Province of")] <- "Taiwan, Province of China"
  crop_diversity$Country[which(crop_diversity$Country == "Guinea-Bissau")] <- "Guinea Bissau"
  crop_diversity$Country[which(crop_diversity$Country == "Occupied Palestinian Territory")] <- "Palestine"
  crop_diversity$Country[which(crop_diversity$Country == "Sudan (former)")] <- "Sudan"
  crop_diversity$Country[which(crop_diversity$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
  
  crop_diversity <- dplyr::inner_join(x = country_codes, y = crop_diversity, by = c("country.name.en" = "Country"))
  crop_diversity <- crop_diversity %>% dplyr::select(country.name.en, iso3c, Crop.diversity)
  
  
  ## 12. Stunting
  ## Measure: Undernutrition
  ## Original name: Children aged <5 years stunted
  ## Units: (%)
  ## Years: 2000:2014
  ## Countries with data: depends on the year
  ## Average produces more information: 2010-2014, 98 countries
  
  stunting <- read_csv(file = "./Input_data_final/Food_Nutrition/Stunting.csv", col_names = T, skip = 1)
  names(stunting)[4] <- "Stunting"
  stunting <- stunting %>% select(Country, Year, Stunting)
  stunting$Country <- as.character(stunting$Country)
  stunting$Country[which(stunting$Country == "Bolivia (Plurinational State of)")] <- "Bolivia"
  stunting$Country[which(stunting$Country == "Côte d'Ivoire")] <- "Ivory Coast"
  stunting$Country[which(stunting$Country == "Czechia")] <- "Czech Republic"
  stunting$Country[which(stunting$Country == "Guinea-Bissau")] <- "Guinea Bissau"
  stunting$Country[which(stunting$Country == "The former Yugoslav republic of Macedonia")] <- "The former Yugoslav Republic of Macedonia"
  stunting$Country[which(stunting$Country == "United Kingdom of Great Britain and Northern Ireland")] <- "United Kingdom"
  stunting$Country[which(stunting$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
  stunting <- stunting %>% filter(Year >= 2000)
  stunting$Year <- as.numeric(as.character(stunting$Year))
  
  stunting <- stunting %>% spread(key = Year, value = Stunting)
  stunting$Stunting <- rowMeans(stunting[,which(names(stunting)=="2010"):which(names(stunting)=="2014")], na.rm = T)
  # stunting$Stunting <- rowMeans(stunting[,2:ncol(stunting)], na.rm = T) # Calculate average 2000-2014
  stunting <- stunting %>% dplyr::select(Country, Stunting)
  
  stunting <- dplyr::inner_join(x = country_codes, y = stunting, by = c("country.name.en" = "Country"))
  stunting <- stunting %>% dplyr::select(country.name.en, iso3c, Stunting)
  
  
  ## 13. Obesity
  ## Measure: Overnutrition
  ## Original name: Prevalence of Obesity, percentage of the population, over 18 years of age
  ## Units: (%)
  ## Years: 2000:2014
  ## Countries with data: 191
  
  obesity <- read_csv(file = "./Input_data_final/Food_Nutrition/Obesity.csv", col_names = T, skip = 2)
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
  
  obesity <- obesity %>% filter(Sex == "Average")
  obesity$Country <- as.character(obesity$Country)
  obesity$Year <- as.numeric(as.character(obesity$Year))
  obesity$Sex <- NULL
  colnames(obesity)[3] <- "Obesity"
  obesity$Country[which(obesity$Country == "Bolivia (Plurinational State of)")] <- "Bolivia"
  obesity$Country[which(obesity$Country == "Côte d'Ivoire")] <- "Ivory Coast"
  obesity$Country[which(obesity$Country == "Czechia")] <- "Czech Republic"
  obesity$Country[which(obesity$Country == "Guinea-Bissau")] <- "Guinea Bissau"
  obesity$Country[which(obesity$Country == "Sudan (former)")] <- "Sudan"
  obesity$Country[which(obesity$Country == "The former Yugoslav republic of Macedonia")] <- "The former Yugoslav Republic of Macedonia"
  obesity$Country[which(obesity$Country == "United Kingdom of Great Britain and Northern Ireland")] <- "United Kingdom"
  obesity$Country[which(obesity$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
  obesity <- obesity %>% filter(Year >= 2000)
  obesity <- obesity[!(obesity$Country == "Sudan" & is.na(obesity$Obesity)),]
  
  # obesity %>% ggplot(aes(x = Year, y = Obesity, group = Country)) + geom_line(alpha = .2) + theme_bw()
  
  yearsList <- obesity$Year %>% unique %>% sort
  obesityList <- lapply(1:length(yearsList), function(i){
    df <- obesity %>% filter(Year == yearsList[i])
    df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
    return(df)
  })
  lapply(obesityList, dim)
  
  recentObesity <- obesityList[[length(obesityList)]]
  recentObesity <- recentObesity %>% dplyr::select(country.name.en, iso3c, Obesity)
  rownames(recentObesity) <- 1:nrow(recentObesity)
  rm(obesity, obesityList)
  
  
  ## 14. Nutrient deficiency in vitamin A
  ## Measure: Overnutrition
  ## Original name: Country data on median urinary iodine concentrations and urinary iodine concentrations in school age children <100 μg/l 1993–2006
  ## Units: (%)
  ## Years: 2000:2014
  ## Countries with data: 195
  
  # iodine <- read.csv("./Input_data_final/Food_Nutrition/VitaminA_deficiency/iodine_concentrations.csv")
  # iodine <- dplyr::inner_join(x = country_codes, y = iodine, by = c("country.name.en" = "Country"))
  # iodine <- iodine %>% dplyr::select(country.name.en, iso3c, Urinary.iodine)
  # 
  # nigth_blindness <- read.csv("./Input_data_final/Food_Nutrition/VitaminA_deficiency/proportion_population_night_blindness.csv")
  # nigth_blindness <- dplyr::inner_join(x = country_codes, y = nigth_blindness, by = c("country.name.en" = "Country"))
  # nigth_blindness <- nigth_blindness %>% dplyr::select(country.name.en, iso3c, Proportion.pop.night.blindness)
   
  serum_retinol <- read.csv("./Input_data_final/Food_Nutrition/VitaminA_deficiency/serum_retinol_deficiency.csv")
  serum_retinol <- dplyr::inner_join(x = country_codes, y = serum_retinol, by = c("country.name.en" = "Country"))
  serum_retinol <- serum_retinol %>% dplyr::select(country.name.en, iso3c, Serum.retinol.deficiency)
  apply(X = serum_retinol, MARGIN = 2, FUN = function(x){sum(!is.na(x))})
  
  
  foodNutDim <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = AverageFoodSupply, by = c("country.name.en", "iso3c"))
  foodNutDim <- dplyr::left_join(x = foodNutDim, y = FoodConsumption, by = c("country.name.en", "iso3c"))
  foodNutDim <- dplyr::left_join(x = foodNutDim, y = city_access, by = c("country.name.en", "iso3c"))
  foodNutDim <- dplyr::left_join(x = foodNutDim, y = improved_water, by = c("country.name.en", "iso3c"))
  foodNutDim <- dplyr::left_join(x = foodNutDim, y = recentAccessElectricity, by = c("country.name.en", "iso3c"))
  foodNutDim <- dplyr::left_join(x = foodNutDim, y = price_volatility, by = c("country.name.en", "iso3c"))
  foodNutDim <- dplyr::left_join(x = foodNutDim, y = recentFoodSupplyVar, by = c("country.name.en", "iso3c"))
  foodNutDim <- dplyr::left_join(x = foodNutDim, y = foodBorne_illness, by = c("country.name.en", "iso3c"))
  foodNutDim <- dplyr::left_join(x = foodNutDim, y = food_loss, by = c("country.name.en", "iso3c"))
  foodNutDim <- dplyr::left_join(x = foodNutDim, y = recentDietDiv, by = c("country.name.en", "iso3c"))
  foodNutDim <- dplyr::left_join(x = foodNutDim, y = crop_diversity, by = c("country.name.en", "iso3c"))
  foodNutDim <- dplyr::left_join(x = foodNutDim, y = stunting, by = c("country.name.en", "iso3c"))
  foodNutDim <- dplyr::left_join(x = foodNutDim, y = recentObesity, by = c("country.name.en", "iso3c"))
  foodNutDim <- dplyr::left_join(x = foodNutDim, y = serum_retinol, by = c("country.name.en", "iso3c"))
  foodNutDim$country.name.en <- foodNutDim$country.name.en %>% as.character
  foodNutDim <- foodNutDim[-which(duplicated(foodNutDim$country.name.en)==T),]
  
  foodNutDim <- foodNutDim[-which(apply(X = foodNutDim[,3:ncol(foodNutDim)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 14),]
  rownames(foodNutDim) <- foodNutDim$country.name.en
  foodNutDim$country.name.en <- NULL
  write.csv(foodNutDim, "food_nutrition_dimension.csv", row.names = T)
  
  rm(AverageFoodSupply, FoodConsumption, city_access, improved_water, recentAccessElectricity, price_volatility,
     recentFoodSupplyVar, food_loss, recentDietDiv, crop_diversity, stunting, recentObesity, yearsList, foodBorne_illness, serum_retinol)
  
} else {
  food_nutritionDim <- read.csv("food_nutrition_dimension.csv", row.names = 1)
}

suppressMessages(library(tabplot))
suppressMessages(library(GGally))
suppressMessages(library(corrplot))

# Distributions and missing values representation
tableplot(food_nutritionDim[,-1], nBins = nrow(food_nutritionDim))

# Correlation
M <- cor(food_nutritionDim[,-1], use = "complete.obs", method = "spearman")
corrplot(M, method = "square"); rm(M)

# PCA
FactoMineR::PCA(X = food_nutritionDim[complete.cases(food_nutritionDim),-1])
