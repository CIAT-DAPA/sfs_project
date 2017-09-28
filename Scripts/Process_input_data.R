# Processing and integrating data: SFS project
# Implemented by: H. Achicanoy & P. Alvarez
# CIAT, 2017

# R options
g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)
OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
wk_dir <- paste0(OSysPath, "/workspace_cluster_9/Sustainable_Food_System/"); setwd(wk_dir)
rm(wk_dir, OSysPath, OSys)

# Load packages
suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(maptools)){install.packages('maptools'); library(maptools)} else {library(maptools)})
suppressMessages(if(!require(jsonlite)){install.packages('jsonlite'); library(jsonlite)} else {library(jsonlite)})
suppressMessages(if(!require(foreach)){install.packages('foreach'); library(foreach)} else {library(foreach)})
suppressMessages(if(!require(doMC)){install.packages('doMC'); library(doMC)} else {library(doMC)})
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
country_codes$fao[which(country_codes == "Reunion")] <- 182

# # Country codes FAO
# if(!file.exists('./Results/_fixed_data/FAO_ISO3_codes.RDS')){
#   iso3FAO <- readHTMLTable('http://www.fao.org/countryprofiles/iso3list/en/')
#   iso3FAO <- iso3FAO$`NULL`
#   saveRDS(object = iso3FAO, file = './Results/_fixed_data/FAO_ISO3_codes.RDS')
# } else {
#   iso3FAO <- readRDS(file = './Results/_fixed_data/FAO_ISO3_codes.RDS')
#   iso3FAO$`Short name` <- as.character(iso3FAO$`Short name`); iso3FAO$`Short name`[which(iso3FAO$`Short name` == "Côte d'Ivoire")] <- "Ivory Coast"; iso3FAO$`Short name` <- as.factor(iso3FAO$`Short name`)
# }

# # Country codes World Bank
# if(!file.exists('./Results/_fixed_data/WBK_ISO3_codes.RDS')){
#   iso3WBK <- readHTMLTable(getURL('https://wits.worldbank.org/wits/wits/witshelp/content/codes/country_codes.htm', .opts = list(ssl.verifypeer = FALSE)))
#   iso3WBK <- iso3WBK$`NULL`
#   iso3WBK$V3 <- NULL
#   colnames(iso3WBK) <- c("Country", "ISO3")
#   iso3WBK <- iso3WBK[-1,]; rownames(iso3WBK) <- 1:nrow(iso3WBK)
#   saveRDS(object = iso3WBK, file = './Results/_fixed_data/WBK_ISO3_codes.RDS')
# } else {
#   iso3WBK <- readRDS(file = './Results/_fixed_data/WBK_ISO3_codes.RDS')
#   iso3WBK$Country <- as.character(iso3WBK$Country); iso3WBK$Country[which(iso3WBK$Country == "Côte d'Ivoire")] <- "Ivory Coast"; iso3WBK$Country <- as.factor(iso3WBK$Country)
# }

## ========================================================================== ##
## ENVIRONMENT
## ========================================================================== ##

## 1. Soil carbon content
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


## 2. Emissions (CO2eq)
## Original name: Emissions by sector
## Sectors: agriculture, energy, forest, industrial processes, land use, other sources, residential, transport, waste
## Units: gigagrams
## Years: 1990:2010 (Selected period: 2000:2010)
## Countries with data: 231

emission <- read.csv("./Input_data_final/Environment/emission.csv")
# yearsList <- unique(as.character(emission$Year))
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
  facet_wrap(~Source, scales = "free") + theme_bw()
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
  df <- emission %>% filter(Year == yearsList[i])
  df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
  return(df)
})
lapply(emissionList, dim)

# emission2 <- emission %>% select(Country) %>% unique
# dplyr::inner_join(x = country_codes, y = emission %>% select(Country) %>% unique, by = c("country.name.en" = "Country")) %>% dim
# emission2$Country[which(is.na(match(emission2$Country, country_codes$country.name.en)))]


## 3. Arable land
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
  theme_bw()

yearsList <- arable_land$Year %>% unique %>% sort
arable_landList <- lapply(1:length(yearsList), function(i){
  df <- arable_land %>% filter(Year == yearsList[i])
  return(df)
})
lapply(arable_landList, dim)

arable_land2 <- arable_land %>% select(Country) %>% unique
dplyr::inner_join(x = country_codes, y = arable_land %>% select(Country) %>% unique, by = c("country.name.en" = "Country")) %>% dim
arable_land2$Country[which(is.na(match(arable_land2$Country, country_codes$country.name.en)))]

arable_land[which(arable_land$Country == "Belgium" | arable_land$Country == "Luxembourg" | arable_land$Country == "Belgium-Luxembourg"),]
arable_land[which(arable_land$Country == "Serbia" | arable_land$Country == "Montenegro" | arable_land$Country == "Serbia and Montenegro"),]
arable_land[which(arable_land$Country == "Sudan" | arable_land$Country == "Sudan (former)"),]
arable_land[which(arable_land$Country == "USSR"),]

## 4. Energy used in agriculture and forestry
## Original name: Agriculture and forestry energy use as a % of total Energy use
## Units: (%)
## Years: 1971:2009
## Countries with data: 122

energy <- read.csv("./Input_data_final/Environment/energy.csv")
energy <- energy %>% dplyr::select(Country, Year, Value)
energy$Country <- as.character(energy$Country)
energy$Country[which(energy$Country == "CÃ´te d'Ivoire")] <- "Ivory Coast"
energy$Country[which(energy$Country == "Sudan (former)")] <- "Sudan"
colnames(energy)[3] <- "Energy.agriculture"

energy %>% ggplot(aes(x = Year, y = Energy.agriculture, group = Country)) +
  geom_line(alpha = .2) + 
  theme_bw()

# Please doing merge

## 5. Water withdrawal
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

water %>% ggplot(aes(x = Year, y = Water.withdrawal, group = Country)) +
  geom_line(alpha = .2) + 
  theme_bw()

## 6. Total population with access to safe drinking-water
## Original name: Total population with access to safe drinking-water (JMP)
## Units: (%)
## Years: 1992:2015 (not all years have data)
## Countries with data: 195 (not all countries have data)

safe_water <- read.csv("./Input_data_final/Environment/aquastat_access_safe_water.csv")
safe_water <- safe_water %>% dplyr::select(Area, Year, Value)
colnames(safe_water)[c(1, 3)] <- c("Country", "Access.safe.water")

safe_water %>% ggplot(aes(x = Year, y = Access.safe.water, group = Country)) +
  geom_line(alpha = .2) + 
  theme_bw()

# 7. GEF biodiversity index
## Original name: Total population with access to safe drinking-water (JMP)
## Units: (%)
## Years: 2008
## Countries with data: 195 (not all countries have data)

GBI <- read.csv("./Input_data_final/Environment/GEF_Biodiversity.csv")
GBI$Country <- as.character(GBI$Country)
GBI$Country[which(GBI$Country == "Côte d'ivoire")] <- "Ivory Coast"

GBI %>% ggplot(aes(x = reorder(Country, GBI), y = GBI)) +
  geom_bar(stat = "identity") +
  xlab("Country") + ylab("GBI biodiversity index") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

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


