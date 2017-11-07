# Processing and integrating data: SFS project
# Implemented by: H. Achicanoy & P. Alvarez
# CIAT, 2017

# R options
g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)

OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
wk_dir   <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9/Sustainable_Food_System", "Windows" = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System")
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

## 1. GHG Emissions (CO2eq)
## Measure: Air quality
## Original name: GHG emissions by sector
## Sectors: agriculture, energy, forest, industrial processes, land use, other sources, residential, transport, waste
## Units: gigagrams
## Years: 1990:2010 (Selected period: 2000:2010)
## Countries with data: 231

emission <- read.csv("./Input_data_final/Environment/emission.csv")
emission <- emission %>% dplyr::select(Country, Item, Year, Value)
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
emission %>% ggplot(aes(x = Year, y = Emission, group = Country)) + geom_line(alpha = .2) +
  facet_wrap(~Source, scales = "free") +
  scale_x_continuous(breaks = 2000:2010, limits = c(2000, 2010)) +
  theme_bw()
emission <- emission %>% spread(Source, Emission)
emission$Country <- emission$Country %>% as.character
emission$Country[which(emission$Country == "CÃ´te d'Ivoire")] <- "Ivory Coast"
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
lapply(emissionList, dim)

recentEmission <- emissionList[[length(emissionList)]]
recentEmission <- recentEmission %>% dplyr::select(country.name.en, iso3c, Emissions.agriculture.total)


## 2. Total population with access to safe drinking-water
## Measure: Water quality
## Original name: Total population with access to safe drinking-water (JMP)
## Units: (%)
## Years: 1992:2015 (not all years have data)
## Countries with data: 195 (not all countries have data)

safe_water <- read.csv("./Input_data_final/Environment/aquastat_access_safe_water.csv")
safe_water <- safe_water %>% dplyr::select(Area, Year, Value)
safe_water <- safe_water %>% filter(Year >= 2000)
colnames(safe_water)[c(1, 3)] <- c("Country", "Access.safe.water")
safe_water$Country <- safe_water$Country %>% as.character
safe_water$Country[which(safe_water$Country == "Côte d'Ivoire")] <- "Ivory Coast"
safe_water$Country[which(safe_water$Country == "Czechia")] <- "Czech Republic"
safe_water$Country[which(safe_water$Country == "Guinea-Bissau")] <- "Guinea Bissau"
safe_water$Country[which(safe_water$Country == "Occupied Palestinian Territory")] <- "Palestine"
safe_water$Country[which(safe_water$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"

safe_water %>% ggplot(aes(x = Year, y = Access.safe.water, group = Country)) +
  geom_line(alpha = .2) + 
  theme_bw()

yearsList <- safe_water$Year %>% unique %>% sort
safe_waterList <- lapply(1:length(yearsList), function(i){
  df <- safe_water %>% filter(Year == yearsList[i])
  df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
  return(df)
})
lapply(safe_waterList, dim)

recentSafe_water <- safe_waterList[[length(safe_waterList)]]
recentSafe_water <- recentSafe_water %>% dplyr::select(country.name.en, iso3c, Access.safe.water)


## 3. Water withdrawal
## Measure: Water use
## Original name: Agricultural water withdrawal as % of total water withdrawal (%)
## Units: (%)
## Years: 1988:2016 (not all years have data)
## Countries with data: 178 (not all countries have data)

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

water %>% ggplot(aes(x = Year, y = Water.withdrawal, group = Country)) +
  geom_line(alpha = .2) + 
  theme_bw()

yearsList <- water$Year %>% unique %>% sort
waterList <- lapply(1:length(yearsList), function(i){
  df <- water %>% filter(Year == yearsList[i])
  df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
  return(df)
})
lapply(waterList, dim)

water <- water %>% tidyr::spread(key = Year, value = Water.withdrawal)
water$Water.withdrawal <- rowMeans(water[,2:ncol(water)], na.rm = T)
water <- water %>% dplyr::select(Country, Water.withdrawal)

water <- dplyr::inner_join(x = country_codes, y = water, by = c("country.name.en" = "Country"))
water <- water %>% dplyr::select(country.name.en, iso3c, Water.withdrawal)


## 4. Soil carbon content
## Measure: Soil and land quality
## Original name: Average carbon content in the topsoil as a % in weight
## Units: (%)
## Years: 2008
## Countries with data: 202

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

carbon_soil %>% ggplot(aes(x = reorder(Country, Soil.carbon.content), y = Soil.carbon.content)) +
  geom_bar(stat = "identity") +
  xlab("Country") + ylab("Average carbon content in the topsoil (%)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

carbon_soil <- dplyr::inner_join(x = country_codes, y = carbon_soil, by = c("country.name.en" = "Country"))
carbon_soil <- carbon_soil %>% dplyr::select(country.name.en, iso3c, Soil.carbon.content)


## 5. Arable land
## Measure: Soil and land use
## Original name: Arable land
## Units: (%)
## Years: 1961:2014
## Countries with data: 227

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

arable_land %>% ggplot(aes(x = Year, y = Arable.land, group = Country)) +
  geom_line(alpha = .2) +
  scale_x_continuous(breaks = 2000:2014, limits = c(2000, 2014)) +
  theme_bw()

yearsList <- arable_land$Year %>% unique %>% sort
arable_landList <- lapply(1:length(yearsList), function(i){
  df <- arable_land %>% filter(Year == yearsList[i])
  df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
  return(df)
})
lapply(arable_landList, dim)

recentArable_land <- arable_landList[[length(arable_landList)]]
recentArable_land <- recentArable_land %>% dplyr::select(country.name.en, iso3c, Arable.land)


## 6. GEF biodiversity index
## Measure: Biodiversity wildlife (plants, animals)
## Original name: Total population with access to safe drinking-water (JMP)
## Units: (%)
## Years: 2008
## Countries with data: 195 (not all countries have data)

GBI <- read.csv("./Input_data_final/Environment/GEF_Biodiversity.csv")
GBI$Country <- as.character(GBI$Country)
GBI$Country[which(GBI$Country == "Côte d'ivoire")] <- "Ivory Coast"
GBI$Country[which(GBI$Country == "Cape Verde")] <- "Cabo Verde"
GBI$Country[which(GBI$Country == "Congo DR")] <- "Democratic Republic of the Congo"
GBI$Country[which(GBI$Country == "Guinea-Bissau")] <- "Guinea Bissau"
GBI$Country[which(GBI$Country == "Iran")] <- "Iran (Islamic Republic of)"
GBI$Country[which(GBI$Country == "Korea DPR")] <- "Democratic People's Republic of Korea"
GBI$Country[which(GBI$Country == "Kyrgyz Republic")] <- "Kyrgyzstan"
GBI$Country[which(GBI$Country == "Laos")] <- "Lao People's Democratic Republic"
GBI$Country[which(GBI$Country == "Macedonia")] <- "The former Yugoslav Republic of Macedonia"
GBI$Country[which(GBI$Country == "Micronesia")] <- "Micronesia (Federated States of)"
GBI$Country[which(GBI$Country == "Moldova")] <- "Republic of Moldova"
GBI$Country[which(GBI$Country == "Russia")] <- "Russian Federation"
GBI$Country[which(GBI$Country == "Slovak Republic")] <- "Slovakia"
GBI$Country[which(GBI$Country == "St. Kitts and Nevis")] <- "Saint Kitts and Nevis"
GBI$Country[which(GBI$Country == "St. Lucia")] <- "Saint Lucia"
GBI$Country[which(GBI$Country == "St. Vincent and the Grenadines")] <- "Saint Vincent and the Grenadines"
GBI$Country[which(GBI$Country == "Syria")] <- "Syrian Arab Republic"
GBI$Country[which(GBI$Country == "Tanzania")] <- "United Republic of Tanzania"
GBI$Country[which(GBI$Country == "Timor Leste")] <- "Timor-Leste"
GBI$Country[which(GBI$Country == "Vietnam")] <- "Viet Nam"

GBI %>% ggplot(aes(x = reorder(Country, GBI), y = GBI)) +
  geom_bar(stat = "identity") +
  xlab("Country") + ylab("GBI biodiversity index") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

GBI <- dplyr::inner_join(x = country_codes, y = GBI, by = c("country.name.en" = "Country"))
GBI <- GBI %>% dplyr::select(country.name.en, iso3c, GBI)


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

energy %>% ggplot(aes(x = Year, y = Energy.agriculture, group = Country)) +
  geom_line(alpha = .2) + 
  theme_bw()

yearsList <- energy$Year %>% unique %>% sort
energyList <- lapply(1:length(yearsList), function(i){
  df <- energy %>% filter(Year == yearsList[i])
  df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
  return(df)
})
lapply(energyList, dim)

recentEnergy <- energyList[[length(energyList)]]
recentEnergy <- recentEnergy %>% dplyr::select(country.name.en, iso3c, Energy.agriculture)

environmentDim <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = recentEmission, by = c("country.name.en", "iso3c"))
environmentDim <- dplyr::left_join(x = environmentDim, y = recentSafe_water, by = c("country.name.en", "iso3c"))
environmentDim <- dplyr::left_join(x = environmentDim, y = water, by = c("country.name.en", "iso3c"))
environmentDim <- dplyr::left_join(x = environmentDim, y = carbon_soil, by = c("country.name.en", "iso3c"))
environmentDim <- dplyr::left_join(x = environmentDim, y = recentArable_land, by = c("country.name.en", "iso3c"))
environmentDim <- dplyr::left_join(x = environmentDim, y = GBI, by = c("country.name.en", "iso3c"))
environmentDim <- dplyr::left_join(x = environmentDim, y = recentEnergy, by = c("country.name.en", "iso3c"))

environmentDim <- environmentDim[-which(apply(X = environmentDim[,3:ncol(environmentDim)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 7),]
rownames(environmentDim) <- environmentDim$country.name.en
environmentDim$country.name.en <- NULL

rm(recentEmission, recentSafe_water, water, carbon_soil, recentArable_land, GBI, recentEnergy)
rm(emission, safe_water, arable_land, energy)
rm(arable_landList, emissionList, energyList, safe_waterList, waterList, yearsList)

suppressMessages(library(tabplot))
suppressMessages(library(GGally))
suppressMessages(library(corrplot))

tableplot(environmentDim[,-1], nBins = nrow(environmentDim)) # Distributions and missing values representation

# Correlation
ggpairs(environmentDim[,-1])
M <- cor(environmentDim[,-1], use = "complete.obs")
corrplot(M, method = "square")
plot(environmentDim$Emissions.agriculture.total, environmentDim$GBI, pch = 20)

# PCA
FactoMineR::PCA(X = environmentDim[complete.cases(environmentDim),-1])

## ========================================================================== ##
## ECONOMICS
## ========================================================================== ##

## 1. Agriculture value-added per worker
## Measure: Financial performance
## Original name: Agriculture value-added per worker (constant 2010 US$)
## Units: Constant 2010 US$
## Years: 2000:2016
## Countries with data: 137

AgValueAdded <- read.csv("./Input_data_final/Economic/AgValue_added-WorldBank.csv")
AgValueAdded$Indicator.Name <- AgValueAdded$Indicator.Code <- NULL
names(AgValueAdded)[1:2] <- c("Country", "ISO3")
AgValueAdded <- AgValueAdded %>% gather(key = Year, value = AgValueAdded, 3:ncol(AgValueAdded))
AgValueAdded$Year <- gsub(pattern = "X", replacement = "", x = AgValueAdded$Year) %>% as.character %>% as.numeric
AgValueAdded <- AgValueAdded %>% filter(Year >= 2000)

AgValueAdded %>% ggplot(aes(x = Year, y = AgValueAdded, group = Country)) +
  geom_line(alpha = .5) + theme_bw()

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

recentAgValueAdded <- AgValueAddedList[[length(AgValueAddedList)]]
recentAgValueAdded <- recentAgValueAdded %>% dplyr::select(country.name.en, iso3c, AgValueAdded)


## 2. Agriculture under-employment
## Measure: Employment rate

## 3. Wage employment distribution in agriculture
## Measure: Economic distribution
## Original name: Wage employment distribution in agriculture
## Units: Constant 2010 US$
## Years: 2000:2016
## Countries with data: 137

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

employment %>% ggplot(aes(x = Year, y = Value, colour = Indicator, group = Country)) +
  geom_line(alpha = .5) + facet_wrap(~Indicator, scales = "free") + 
  theme_bw()

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

# AgValueAdded <- employment %>% select(Country, Year, AgValue.added) %>% spread(., key = Year, value = AgValue.added)
# TimeUnderemployment <- employment %>% select(Country, Year, Time.underemployment) %>% spread(., key = Year, value = Time.underemployment)
WageEmployment <- employment %>% select(Country, Year, Wage.employment) %>% spread(., key = Year, value = Wage.employment)

# AgValueAdded$AgValue.added <- rowMeans(x = AgValueAdded[,2:ncol(AgValueAdded)], na.rm = T)
# sum(!is.na(AgValueAdded$AgValue.added))
# TimeUnderemployment$Time.underemployment <- rowMeans(x = TimeUnderemployment[,2:ncol(TimeUnderemployment)], na.rm = T)
# sum(!is.na(TimeUnderemployment$Time.underemployment))
WageEmployment$Wage.employment <- rowMeans(x = WageEmployment[,2:ncol(WageEmployment)], na.rm = T)
WageEmployment <- WageEmployment %>% select(Country, Wage.employment)
# sum(!is.na(WageEmployment$Wage.employment))

WageEmployment <- dplyr::inner_join(x = country_codes, y = WageEmployment, by = c("country.name.en" = "Country"))
WageEmployment <- WageEmployment %>% dplyr::select(country.name.en, iso3c, Wage.employment)


## ========================================================================== ##
## SOCIAL
## ========================================================================== ##

## 1. Employment in agriculture female (% of female employment)
## Measure: Gender/Equity
## Original name: Employment in agriculture female (% of female employment)
## Units: (%)
## Years: 1990:2010 (Selected period: 2000:2010)
## Countries with data: 231

lf_female <- read_csv(file = "./Input_data_final/Social/Labor_force_female.csv", col_names = T, skip = 4)
names(lf_female)[1:2] <- c("Country", "ISO3")
lf_female <- cbind(lf_female[,1:2], lf_female[,5:ncol(lf_female)])
lf_female <- lf_female %>% gather(Year, Value, -(Country:ISO3))
lf_female <- lf_female %>% select(ISO3, Country, Year, Value)
names(lf_female)[ncol(lf_female)] <- "Labor_force_female"
lf_female$Labor_force_female <- as.numeric(lf_female$Labor_force_female)

lf_female %>% filter(Year > 1989) %>% ggplot(aes(x = Year, y = Labor_force_female, colour = Country)) +
  geom_point() + theme(legend.position = 'none')


## 2. Number of fair trade producer organizations
## Measure: Inclusion


## 3. Employment in agriculture
## Measure: Inclusion
## Original name: Employment in agriculture (% of total employment)
## Units: (%)
## Years: 
## Countries with data: 

AgEmployment <- readxl::read_excel(path = "./Input_data_final/Social/employment_agriculture.xlsx", sheet = 1)
AgEmployment <- AgEmployment[1:217,]
AgEmployment$`Series Name` <- NULL
AgEmployment$`Series Code` <- NULL
AgEmployment$`Country Code` <- NULL
AgEmployment$`1990` <- NULL
names(AgEmployment)[1] <- "Country"

AgEmployment$Agr.employment <- rowMeans(x = AgEmployment[,2:ncol(AgEmployment)], na.rm = T)
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
AgEmployment$Country[which(AgEmployment$Country == "Macedonia, FYR")] <- "Democratic People's Republic of Korea"
AgEmployment$Country[which(AgEmployment$Country == "West Bank and Gaza")] <- "Palestine"
AgEmployment$Country[which(AgEmployment$Country == "Slovak Republic")] <- "Slovakia"
AgEmployment$Country[which(AgEmployment$Country == "Tanzania")] <- "United Republic of Tanzania"
AgEmployment$Country[which(AgEmployment$Country == "United States")] <- "United States of America"
AgEmployment$Country[which(AgEmployment$Country == "St. Vincent and the Grenadines")] <- "Saint Vincent and the Grenadines"
AgEmployment$Country[which(AgEmployment$Country == "Venezuela, RB")] <- "Venezuela"
AgEmployment$Country[which(AgEmployment$Country == "Virgin Islands (U.S.)")] <- "United States Virgin Islands"
AgEmployment$Country[which(AgEmployment$Country == "Vietnam")] <- "Viet Nam"
AgEmployment$Country[which(AgEmployment$Country == "Yemen, Rep.")] <- "Yemen"



AgEmployment2 <- AgEmployment %>% select(Country) %>% unique
AgEmployment2$Country[which(is.na(match(AgEmployment2$Country, country_codes$country.name.en)))]

AgEmployment <- dplyr::inner_join(x = country_codes, y = AgEmployment, by = c("country.name.en" = "Country"))
AgEmployment <- AgEmployment %>% dplyr::select(country.name.en, iso3c, Agr.employment)

## ========================================================================== ##
## FOOD AND NUTRITION
## ========================================================================== ##

## 1. Food available for human consumption
## Measure: Food security availability
## Original name: Per capita food available for human consumption
## Units: ???
## Years: 1990:2010 (Selected period: 2000:2010)
## Countries with data: 231

## 2. Food consumption
## Measure: Food security access
## Original name: Food consumption as share of total income
## Units: ???
## Years: 1990:2010 (Selected period: 2000:2010)
## Countries with data: 231

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
## Years: 1995:2015 (Selected period: 2000:2015)
## Countries with data: 226

improved_water <- read_csv(file = "./Input_data_final/Food_Nutrition/Access_improved_water.csv", col_names = T)
improved_water <- improved_water %>% select(Country, Year, Value)
names(improved_water)[3] <- "Access.improved.water"
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
improved_water <- improved_water %>% filter(Country != "Serbia and Montenegro" & Year >= 2000)

improved_water %>% ggplot(aes(x = Year, y = Access.improved.water, group = Country)) +
  geom_line(alpha = .2) + 
  theme_bw()

yearsList <- improved_water$Year %>% unique %>% sort
improved_waterList <- lapply(1:length(yearsList), function(i){
  df <- improved_water %>% filter(Year == yearsList[i])
  df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
  return(df)
})
lapply(improved_waterList, dim)

recentImprovedWater <- improved_waterList[[length(improved_waterList)-1]]
recentImprovedWater <- recentImprovedWater %>% dplyr::select(country.name.en, iso3c, Access.improved.water)


## 5. Access to electricity
## Measure: Food security utilization
## Original name: Access to electricity
## Units: (%)
## Years: 1995:2015 (Selected period: 2000:2014)
## Countries with data: 215

access_electricity <- readxl::read_xls(path = "./Input_data_final/Food_Nutrition/Access_to_electricity.xls", sheet = 1, col_names = T, skip = 3)
names(access_electricity)[1:2] <- c("Country", "ISO3")
access_electricity <- access_electricity %>% select(Country, ISO3, which(names(access_electricity)=="2000"):ncol(access_electricity))
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

access_electricity %>% ggplot(aes(x = Year, y = Access.electricity, group = Country)) +
  geom_line(alpha = .2) + 
  theme_bw()

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

## 6. Price volatility index
## Measure: Food security stability
## Original name: Price volatility index
## Units: (CV %)
## Years: 1995:2015 (Selected period: 2000:2014)
## Countries with data: 215

price_volatility <- read_csv(file = "./Input_data_final/Food_Nutrition/Price_volatility_index.csv", col_names = T)
price_volatility <- price_volatility %>% select(Country, Year, 4)
names(price_volatility)[3] <- "Price.volatility.index"

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

price_volatility %>% ggplot(aes(x = Year, y = Price.volatility.index, group = Country)) +
  geom_line(alpha = .2) + 
  theme_bw()

yearsList <- price_volatility$Year %>% unique %>% sort
price_volatilityList <- lapply(1:length(yearsList), function(i){
  df <- price_volatility %>% filter(Year == yearsList[i])
  df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
  return(df)
})
lapply(price_volatilityList, dim)

recentPriceVolatility <- price_volatilityList[[length(price_volatilityList)-2]]
recentPriceVolatility <- recentPriceVolatility %>% dplyr::select(country.name.en, iso3c, Price.volatility.index)

## 7. Food supply variability
## Measure: Food security stability
## Original name: Per capita food supply variability
## Units: 
## Years: (Selected period: 2000:2011)
## Countries with data: 226

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
fsvar <- fsvar %>% filter(Year <= 2011)

fsvar %>% ggplot(aes(x = Year, y = Food.supply.variability, group = Country)) +
  geom_line(alpha = .2) + 
  theme_bw()

yearsList <- fsvar$Year %>% unique %>% sort
fsvarList <- lapply(1:length(yearsList), function(i){
  df <- fsvar %>% filter(Year == yearsList[i])
  df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
  return(df)
})
lapply(fsvarList, dim)

recentFoodSupplyVar <- fsvarList[[length(fsvarList)]]
recentFoodSupplyVar <- recentFoodSupplyVar %>% dplyr::select(country.name.en, iso3c, Food.supply.variability)

## 8. Burden of food borne illness
## Measure: Food safety
## Original name: Burden of food borne illness
## Units: 
## Years: 
## Countries with data: 


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

## 9. Diet diversification
## Measure: Nutrition and diet
## Original name: Diet diversification
## Units: (%)
## Years: 2016
## Countries with data: 113

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

diet_div %>% ggplot(aes(x = Year, y = Diet.diversification, group = Country)) +
  geom_line(alpha = .2) + 
  theme_bw()

yearsList <- diet_div$Year %>% unique %>% sort
diet_divList <- lapply(1:length(yearsList), function(i){
  df <- diet_div %>% filter(Year == yearsList[i])
  df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
  return(df)
})
lapply(diet_divList, dim)

recentDietDiv <- diet_divList[[length(diet_divList)]]
recentDietDiv <- recentDietDiv %>% dplyr::select(country.name.en, iso3c, Diet.diversification)

## 10. Crop diversity
## Measure: Nutrition and diet
## Original name: Crop diversity
## Units: (%)
## Years: 2016
## Countries with data: 
### FALTA HACER ESTA

crop_diversity <- read_csv(file = "./Input_data_final/Food_Nutrition/Diversity_indexes_colin_study.csv", col_names = T)
crop_diversity <- crop_diversity %>% select(Country, Year, Value)
names(crop_diversity)[3] <- "Diet.diversification"
crop_diversity$Country <- as.character(crop_diversity$Country)


## 11. Stunting
## Measure: Undernutrition
## Original name: Children aged <5 years stunted
## Units: (%)
## Years: 2000-2014 (An average of this period has been calculated in order to have more data)
## Countries with data: 143

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

# Calculate average 2000 - 2014
stunting <- stunting %>% spread(key = Year, value = Stunting)
stunting$Stunting <- rowMeans(stunting[,2:ncol(stunting)], na.rm = T)
stunting <- stunting %>% dplyr::select(Country, Stunting)

stunting <- dplyr::inner_join(x = country_codes, y = stunting, by = c("country.name.en" = "Country"))
stunting <- stunting %>% dplyr::select(country.name.en, iso3c, Stunting)

## 11. Obesity
## Measure: Overnutrition
## Original name: Prevalence of Obesity, percentage of the population, over 18 years of age
## Units: (%)
## Years: 2000:2014
## Countries with data: 195

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

obesity %>% ggplot(aes(x = Year, y = Obesity, group = Country)) + geom_line(alpha = .2) + theme_bw()

yearsList <- obesity$Year %>% unique %>% sort
obesityList <- lapply(1:length(yearsList), function(i){
  df <- obesity %>% filter(Year == yearsList[i])
  df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
  return(df)
})
lapply(obesityList, dim)

recentObesity <- obesityList[[length(obesityList)]]
recentObesity <- recentObesity %>% dplyr::select(country.name.en, iso3c, Obesity)



foodNutDim <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = city_access, by = c("country.name.en", "iso3c"))
foodNutDim <- dplyr::left_join(x = foodNutDim, y = recentImprovedWater, by = c("country.name.en", "iso3c"))
foodNutDim <- dplyr::left_join(x = foodNutDim, y = recentAccessElectricity, by = c("country.name.en", "iso3c"))
foodNutDim <- dplyr::left_join(x = foodNutDim, y = recentPriceVolatility, by = c("country.name.en", "iso3c"))
foodNutDim <- dplyr::left_join(x = foodNutDim, y = recentFoodSupplyVar, by = c("country.name.en", "iso3c"))
foodNutDim <- dplyr::left_join(x = foodNutDim, y = food_loss, by = c("country.name.en", "iso3c"))
foodNutDim <- dplyr::left_join(x = foodNutDim, y = recentDietDiv, by = c("country.name.en", "iso3c"))
foodNutDim <- dplyr::left_join(x = foodNutDim, y = stunting, by = c("country.name.en", "iso3c"))
foodNutDim <- dplyr::left_join(x = foodNutDim, y = recentObesity, by = c("country.name.en", "iso3c"))

foodNutDim <- foodNutDim[-which(apply(X = foodNutDim[,3:ncol(foodNutDim)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 9),]
rownames(foodNutDim) <- foodNutDim$foodNutDim
foodNutDim$country.name.en <- NULL

rm(city_access, recentImprovedWater, recentAccessElectricity, recentPriceVolatility, recentFoodSupplyVar, food_loss, recentDietDiv, stunting, recentObesity)
rm(access_electricity, crop_diversity, diet_div, fsvar, improved_water, obesity, price_volatility)
rm(access_electricityList, diet_divList, fsvarList, improved_waterList, obesityList, price_volatilityList)
rm(yearsList)

suppressMessages(library(tabplot))
suppressMessages(library(GGally))
suppressMessages(library(corrplot))

tableplot(foodNutDim[,-1], nBins = nrow(foodNutDim)) # Distributions and missing values representation




## 12. Nutrient deficiency
## Measure: Hidden hunger
## Original name: Nutrient deficiency (Iodine/vitamin A)
## Units: (%)
## Years: 2000:2014
## Countries with data: 195

nutrientDeficiency <- read_csv(file = "./Input_data_final/Food_Nutrition/Vitamin_A_deficiency.csv", col_names = T)
nutrientDeficiency <- nutrientDeficiency %>% select(Country, Item, Year, Value)
nutrientDeficiency <- nutrientDeficiency %>% spread(Item, Value)
names(nutrientDeficiency)[3:4] <- c("Iodine.deficiency", "VitaminA.deficiency")
nutrientDeficiency$Country <- as.character(nutrientDeficiency$Country)
nutrientDeficiency <- nutrientDeficiency %>% filter(Year >= 2000)

nutrientDeficiency$Country[which(nutrientDeficiency$Country == "Côte d'Ivoire")] <- "Ivory Coast"
nutrientDeficiency <- dplyr::left_join(x = vitamin_A, y = iso3FAO %>% select(`Short name`, ISO3), by = c("Country" = "Short name"))
vitamin_A <- vitamin_A %>% select(ISO3, Country, Year, Iodine_deficiency, Vitamin_A_deficiency)

vitamin_A %>% ggplot(aes(x = Year, y = Vitamin_A_deficiency)) + geom_point()






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
