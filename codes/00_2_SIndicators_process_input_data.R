# Processing and integrating data: SFS project - sustainability indicators
# Implemented by: H. Achicanoy & P. Alvarez
# CIAT, 2019

# R options
g <- gc(reset = T); rm(list = ls()); options(scipen = 999, warn = -1)

OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
wk_dir   <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9/Sustainable_Food_System/SFS_indicators", "Windows" = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators")
setwd(wk_dir); rm(wk_dir, OSysPath, OSys)

# Load packages
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(raster, rgdal, maptools, jsonlite, foreach, doParallel, XML, plspm, reshape, tidyverse, countrycode, caret,
                                missMDA, missForest, treemap, viridisLite, highcharter, corrplot, cluster, factoextra, FactoMineR, gghighlight,
                                EnvStats, compiler, caretEnsemble))

data_path <- "D:/sfs_repo/data"

generate_tables <- function(data_path = data_path){
  
  cat(">>> This function generates or loads created input tables <<<\n")
  cat("    1. Verify if intermediate target files exists: ...\n")
  cat("    Environment, Economic, Social, and Food and nutrition\n")
  
  listed_fls <- list.files(paste0(data_path, "/inputs_compiled"), full.names = T)
  
  if(length(listed_fls) < 4){
    
    cat("    1.1. Some intermediate target files does not exist: creating them ...\n")
    
    # Load country codes list
    country_codes <- read.csv(paste0(data_path, "/country_codes/country_codes_28_09_18.csv"))
    
    # Environment
    if(!file.exists(paste0(data_path, "/inputs_compiled/environmental_dimension.csv"))){
      
      ## 1. GHG emissions in total agriculture
      ## Measure: Air quality
      ## Units: gigagrams
      ## Years: 1990-2010 (Selected period: 2000-2010)
      ## Countries with data: 222, Selected year: 2010
      
      emission <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_environmental_raw_indicators.xlsx"), sheet = "emission")
      emission <- emission %>% dplyr::select(Country, Item, Year, Value)
      emission <- emission %>% dplyr::filter(Year >= 2000)
      emission <- emission %>% tidyr::spread(Item, Value)
      colnames(emission)[3] <- "Emissions.agriculture.total"
      emission$Country <- emission$Country %>% as.character
      yearsList <- emission$Year %>% unique %>% sort
      emissionList <- lapply(1:length(yearsList), function(i){
        df <- emission %>% dplyr::select(Country, Year, Emissions.agriculture.total) %>% filter(Year == yearsList[i])
        df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
        return(df)
      })
      recentEmission <- emissionList[[length(emissionList)]]
      recentEmission <- recentEmission %>% dplyr::select(country.name.en, iso3c, Emissions.agriculture.total)
      recentEmission <- recentEmission[complete.cases(recentEmission),]; rownames(recentEmission) <- 1:nrow(recentEmission)
      recentEmission <- recentEmission[recentEmission[,1:2] %>% unique %>% rownames %>% as.numeric,]
      rm(emissionList, emission)
      
      ## 2. Water pH
      ## Measure: Water quality
      ## Units: pH units
      ## Years: 1965-2016 (Selected period: 1965-2016)
      ## Countries with data: 74, Selected year: median of 1965-2016
      
      pH <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_environmental_raw_indicators.xlsx"), sheet = "water_pH")
      pH <- pH %>% dplyr::select(ISO3, Value) %>% dplyr::group_by(ISO3) %>% dplyr::summarise(pH = median(Value, na.rm = T))
      pH <- dplyr::inner_join(x = country_codes, y = pH, by = c("iso3c" = "ISO3"))
      pH <- pH %>% dplyr::select(country.name.en, iso3c, pH)
      pH$pH <- abs(pH$pH - 7)
      
      ## 3. Water withdrawal
      ## Measure: Water use
      ## Units: %
      ## Years: 1988-2016 (Selected period: 2000-2016)
      ## Countries with data: 174, Selected year: mean of 2000-2016
      
      water <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_environmental_raw_indicators.xlsx"), sheet = "water_withdrawal")
      water$Item <- NULL
      water_aux <- lapply(1:nrow(water), function(i){
        df <- data.frame(Country = water$Country[i],
                         Year = c(water$Year1[i], water$Year2[i], water$Year3[i], water$Year4[i], water$Year5[i], water$Year6[i]),
                         Water.withdrawal = c(water$`1988-1992`[i], water$`1993-1997`[i], water$`1998-2002`[i], water$`2003-2007`[i], water$`2008-2012`[i], water$`2013-2017`[i]))
        return(df)
      })
      water <- do.call(rbind, water_aux); rm(water_aux)
      water <- water %>% tidyr::drop_na()
      rownames(water) <- 1:nrow(water)
      water <- water %>% filter(Year >= 2000)
      water$Country <- water$Country %>% as.character
      water <- water %>% tidyr::spread(key = Year, value = Water.withdrawal)
      water$Water.withdrawal <- rowMeans(water[,2:ncol(water)], na.rm = T)
      water <- water %>% dplyr::select(Country, Water.withdrawal)
      water <- dplyr::inner_join(x = country_codes, y = water, by = c("country.name.en" = "Country"))
      water <- water %>% dplyr::select(country.name.en, iso3c, Water.withdrawal)
      
      ## 4. Soil carbon content
      ## Measure: Soil and land quality
      ## Units: %
      ## Years: 2008 (Selected period: 2008)
      ## Countries with data: 202, Selected year: 2008
      
      carbon_soil <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_environmental_raw_indicators.xlsx"), sheet = "soil_carbon")
      carbon_soil <- carbon_soil %>% dplyr::select(Country, Value)
      colnames(carbon_soil)[2] <- "Soil.carbon.content"
      carbon_soil$Country <- as.character(carbon_soil$Country)
      carbon_soil <- dplyr::inner_join(x = country_codes, y = carbon_soil, by = c("country.name.en" = "Country"))
      carbon_soil <- carbon_soil %>% dplyr::select(country.name.en, iso3c, Soil.carbon.content)
      
      ## 5. Arable land
      ## Measure: Soil and land use
      ## Units: %
      ## Years: 1961-2014 (Selected period: 2000-2014)
      ## Countries with data: 217, Selected year: 2014
      
      arable_land <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_environmental_raw_indicators.xlsx"), sheet = "arable_land")
      arable_land <- arable_land %>% dplyr::select(Country, Year, Value)
      arable_land <- arable_land %>% dplyr::filter(Year >= 2000)
      arable_land$Country <- as.character(arable_land$Country)
      colnames(arable_land)[3] <- "Arable.land"
      yearsList <- arable_land$Year %>% unique %>% sort
      arable_landList <- lapply(1:length(yearsList), function(i){
        df <- arable_land %>% filter(Year == yearsList[i])
        df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
        return(df)
      })
      recentArable_land <- arable_landList[[length(arable_landList)]]
      recentArable_land <- recentArable_land %>% dplyr::select(country.name.en, iso3c, Arable.land)
      rm(arable_land, arable_landList)
      
      ## 6. GEF benefits for biodiversity index
      ## Measure: Biodiversity wildlife (plants, animals)
      ## Units: %
      ## Years: 2008 (Selected period: 2008)
      ## Countries with data: 192, Selected year: 2008
      
      GBI <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_environmental_raw_indicators.xlsx"), sheet = "GEF_benefits_for_biodiversity")
      GBI$Rank <- GBI$Year <- NULL
      names(GBI)[2] <- "GEF.benefits.biodiversity"
      GBI$Country <- as.character(GBI$Country)
      GBI <- dplyr::inner_join(x = country_codes, y = GBI, by = c("country.name.en" = "Country"))
      GBI <- GBI %>% dplyr::select(country.name.en, iso3c, GEF.benefits.biodiversity)
      
      ## 7. Energy used in agriculture and forestry
      ## Measure: energy use
      ## Units: %
      ## Years: 1971-2009 (Selected period: 2000-2009)
      ## Countries with data: 113, Selected year: median of 2000-2009
      
      energy <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_environmental_raw_indicators.xlsx"), sheet = "energy")
      energy <- energy %>% dplyr::select(Country, Year, Value)
      energy$Country <- as.character(energy$Country)
      energy$Country[which(energy$Country == "Viet Nam")] <- "Vietnam"
      colnames(energy)[3] <- "Energy.agriculture"
      energy <- energy %>% dplyr::filter(Year >= 2000)
      energy <- energy %>% dplyr::group_by(Country) %>% dplyr::summarise(Energy.agriculture = median(Energy.agriculture, na.rm = T))
      recentEnergy <- dplyr::inner_join(x = country_codes, y = energy, by = c("country.name.en" = "Country"))
      recentEnergy <- recentEnergy %>% dplyr::select(country.name.en, iso3c, Energy.agriculture)
      rm(energy)
      
      # Consolidate Environmental indicators
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
      
      write.csv(x = environmentDim, file = paste0(data_path, "/inputs_compiled/environmental_dimension.csv"), row.names = T)
      
      rm(recentEmission, recentSafe_water, water, carbon_soil, recentArable_land, GBI, recentEnergy)
      rm(emission, safe_water, arable_land, energy, dissOxygen)
      rm(arable_landList, emissionList, energyList, safe_waterList, waterList, yearsList, pH)
    } else {
      environmentDim <- read.csv(paste0(data_path, "/inputs_compiled/environmental_dimension.csv"), row.names = 1)
    }
    
    # Economic
    if(!file.exists(paste0(data_path, "/inputs_compiled/economic_dimension.csv"))){
      
      ## 1. Agriculture value-added per worker
      ## Measure: Financial performance
      ## Units: Constant 2010 US$
      ## Years: 1980-2016 (Selected period: 2006-2016)
      ## Countries with data: 181, Selected year: median of 2006-2016
      
      AgValueAdded <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_economic_raw_indicators.xlsx"), sheet = "agvalue_added")
      AgValueAdded$`Indicator Name` <- AgValueAdded$`Indicator Code` <- NULL
      names(AgValueAdded)[1:2] <- c("Country", "ISO3")
      AgValueAdded <- AgValueAdded %>% dplyr::gather(key = Year, value = AgValueAdded, 3:ncol(AgValueAdded))
      AgValueAdded$Year <- AgValueAdded$Year %>% as.character %>% as.numeric
      AgValueAdded <- AgValueAdded %>% dplyr::filter(Year >= 2000)
      AgValueAdded <- AgValueAdded %>% tidyr::drop_na()
      AgValueAdded <- AgValueAdded %>% dplyr::filter(Year >= 2006)
      AgValueAdded <- AgValueAdded %>% dplyr::group_by(ISO3) %>% dplyr::summarise(AgValueAdded = median(AgValueAdded, na.rm = T))
      recentAgValueAdded <- dplyr::inner_join(x = country_codes, y = AgValueAdded, by = c("iso3c" = "ISO3"))
      recentAgValueAdded <- recentAgValueAdded %>% dplyr::select(country.name.en, iso3c, AgValueAdded)
      recentAgValueAdded <- recentAgValueAdded[complete.cases(recentAgValueAdded),]; rownames(recentAgValueAdded) <- 1:nrow(recentAgValueAdded)
      rm(AgValueAdded)
      
      ## 2. Agriculture under-employment
      ## Measure: Employment rate
      ## Units: 1000 persons
      ## Years: 1991-2014 (Selected period: 2000-2014)
      ## Countries with data: 72, Selected year: mean of 2005-2014
      
      employment <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_economic_raw_indicators.xlsx"), sheet = "underemployment")
      employment <- employment %>% dplyr::select(Country, 8:ncol(employment))
      employment <- employment %>% gather(Year, Value, -Country)
      employment <- unique(employment)
      employment <- employment[setdiff(1:nrow(employment), grep(pattern = "F$", x = employment$Year)),]
      employment$Value <- employment$Value %>% as.character %>% as.numeric
      employment <- employment %>% dplyr::group_by(Country, Year) %>% dplyr::summarise(Value = mean(Value, na.rm = T))
      employment$Year <- gsub(pattern = "Y", replacement = "", x = employment$Year) %>% as.character %>% as.numeric
      employment <- employment %>% tidyr::drop_na()
      rownames(employment) <- 1:nrow(employment)
      colnames(employment)[3] <- "Time.underemployment"
      employment <- employment %>% dplyr::filter(Year >= 2000)
      employment$Country <- as.character(employment$Country)
      employment <- employment[which(employment %>% select(Country, Year) %>% duplicated() == FALSE),]
      TimeUnderemployment <- employment %>% select(Country, Year, Time.underemployment) %>% spread(., key = Year, value = Time.underemployment)
      TimeUnderemployment$Time.underemployment <- rowMeans(x = TimeUnderemployment[,which(colnames(TimeUnderemployment)=="2005"):ncol(TimeUnderemployment)], na.rm = T)
      TimeUnderemployment <- TimeUnderemployment %>% select(Country, Time.underemployment)
      TimeUnderemployment <- dplyr::inner_join(x = country_codes, y = TimeUnderemployment, by = c("country.name.en" = "Country"))
      TimeUnderemployment <- TimeUnderemployment %>% dplyr::select(country.name.en, iso3c, Time.underemployment)
      TimeUnderemployment <- TimeUnderemployment[complete.cases(TimeUnderemployment),]; rownames(TimeUnderemployment) <- 1:nrow(TimeUnderemployment)
      TimeUnderemployment$Time.underemployment <- TimeUnderemployment$Time.underemployment %>% round(3)
      
      ## 3. GINI index for agricultural land
      ## Measure: Economic distribution
      ## Units: GINI index units
      ## Years: 2014 (Selected period: 2014)
      ## Countries with data: 86, Selected year: 2014
      
      gini_agr_land_dist <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_economic_raw_indicators.xlsx"), sheet = "gini_agrLand")
      gini_agr_land_dist <- dplyr::inner_join(x = country_codes, y = gini_agr_land_dist, by = "iso3c")
      gini_agr_land_dist <- gini_agr_land_dist %>% dplyr::select(country.name.en, iso3c, GINI.agr.land.distribution)
      gini_agr_land_dist <- gini_agr_land_dist[complete.cases(gini_agr_land_dist),]; rownames(gini_agr_land_dist) <- 1:nrow(gini_agr_land_dist)
      
      # Consolidate Economic indicators
      economicDim <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = recentAgValueAdded, by = c("country.name.en", "iso3c"))
      economicDim <- dplyr::left_join(x = economicDim, y = TimeUnderemployment, by = c("country.name.en", "iso3c"))
      economicDim <- dplyr::left_join(x = economicDim, y = gini_agr_land_dist, by = c("country.name.en", "iso3c"))
      
      economicDim <- economicDim[-which(apply(X = economicDim[,3:ncol(economicDim)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 3),]
      rownames(economicDim) <- economicDim$country.name.en
      economicDim$country.name.en <- NULL
      
      write.csv(x = economicDim, file = paste0(data_path, "/inputs_compiled/economic_dimension.csv"), row.names = T)
      
      rm(AgValueAdded, AgValueAddedList, employment, recentAgValueAdded, gini_agr_land_dist, TimeUnderemployment, yearsList)
      
    } else {
      economicDim <- read.csv(paste0(data_path, "/inputs_compiled/economic_dimension.csv"), row.names = 1)
    }
    
    # Social
    if(!file.exists(paste0(data_path, "/inputs_compiled/social_dimension.csv"))){
      
      ## 1. Employment in agriculture female
      ## Measure: Gender/Equity
      ## Units: %
      ## Years: 1990-2016 (Selected period: 2000-2016)
      ## Countries with data: 184, Selected year: 2016
      
      FemaleLaborForce <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_social_raw_indicators.xlsx"), sheet = "female_laborforce")
      FemaleLaborForce$`Indicator Name` <- FemaleLaborForce$`Indicator Code` <- NULL
      names(FemaleLaborForce)[1:2] <- c("Country", "iso3c")
      FemaleLaborForce <- FemaleLaborForce %>% tidyr::gather(Year, Female.labor.force, 3:ncol(FemaleLaborForce))
      FemaleLaborForce <- FemaleLaborForce %>% dplyr::filter(Year >= 2000)
      yearsList <- FemaleLaborForce$Year %>% unique %>% sort
      FemaleLaborForceList <- lapply(1:length(yearsList), function(i){
        df <- FemaleLaborForce %>% filter(Year == yearsList[i])
        df <- dplyr::inner_join(x = country_codes, y = df, by = "iso3c")
        return(df)
      })
      recentFemaleLaborForce <- FemaleLaborForceList[[length(FemaleLaborForceList)]]
      recentFemaleLaborForce <- recentFemaleLaborForce %>% dplyr::select(country.name.en, iso3c, Female.labor.force)
      recentFemaleLaborForce <- recentFemaleLaborForce[complete.cases(recentFemaleLaborForce),]; rownames(recentFemaleLaborForce) <- 1:nrow(recentFemaleLaborForce)
      rm(FemaleLaborForce, FemaleLaborForceList)
      
      ## 2. Predominant fair trade organizations and producers
      ## Measure: Inclusion
      ## Units: Categorical variable: (1: Fairtrade Producer Network; 2: Fairtrade Organization; 3: Both)
      ## Years: 2016? (Selected period: 2016)
      ## Countries with data: 160, Selected year: 2016
      
      FairTrade <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_social_raw_indicators.xlsx"), sheet = "fair_trade")
      FairTrade <- FairTrade %>% dplyr::select(Country, data)
      names(FairTrade)[2] <- "Fairtrade.org"
      FairTrade$Country <- as.character(FairTrade$Country)
      FairTrade <- dplyr::inner_join(x = country_codes, y = FairTrade, by = c("country.name.en" = "Country"))
      FairTrade <- FairTrade %>% dplyr::select(country.name.en, iso3c, Fairtrade.org)
      
      ## 3. Employment in agriculture
      ## Measure: Inclusion
      ## Units: %
      ## Years: 2008-2017
      ## Countries with data: depends on the year
      ## Average produces more information: 2008-2017, 149 countries
      
      AgEmployment <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_social_raw_indicators.xlsx"), sheet = "employment_agriculture")
      AgEmployment$`Series Name` <- NULL
      AgEmployment$`Series Code` <- NULL
      AgEmployment$`1990` <- NULL
      names(AgEmployment)[1:2] <- c("Country", "iso3c")
      AgEmployment$Agr.employment <- rowMeans(x = AgEmployment[,which(colnames(AgEmployment)=="2008"):ncol(AgEmployment)], na.rm = T)
      AgEmployment <- AgEmployment %>% dplyr::select(iso3c, Agr.employment)
      AgEmployment <- dplyr::inner_join(x = country_codes, y = AgEmployment, by = "iso3c")
      AgEmployment <- AgEmployment %>% dplyr::select(country.name.en, iso3c, Agr.employment)
      AgEmployment <- AgEmployment[complete.cases(AgEmployment),]; rownames(AgEmployment) <- 1:nrow(AgEmployment)
      
      # Consolidate Social indicators
      socialDim <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = recentFemaleLaborForce, by = c("country.name.en", "iso3c"))
      socialDim <- dplyr::left_join(x = socialDim, y = FairTrade, by = c("country.name.en", "iso3c"))
      socialDim <- dplyr::left_join(x = socialDim, y = AgEmployment, by = c("country.name.en", "iso3c"))
      
      socialDim <- socialDim[-which(apply(X = socialDim[,3:ncol(socialDim)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 3),]
      rownames(socialDim) <- socialDim$country.name.en
      socialDim$country.name.en <- NULL
      
      write.csv(x = socialDim, file = paste0(data_path, "/inputs_compiled/social_dimension.csv"), row.names = T)
      
      rm(AgEmployment, FemaleLaborForce, FemaleLaborForceList, recentFemaleLaborForce, FairTrade, yearsList)
      
    } else {
      socialDim <- read.csv(paste0(data_path, "/inputs_compiled/social_dimension.csv"), row.names = 1)
    }
    
    # Food and nutrition
    if(!file.exists(paste0(data_path, "/inputs_compiled/food_nutrition_dimension.csv"))){
      
      ## 1. Food available for human consumption
      ## Measure: Food security availability
      ## Units: ???
      ## Years: 2016
      ## Countries with data: 113
      
      # GFSI indices 2016
      gfsi <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "gfsi", skip = 5)
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
      gfsi <- gfsi %>% tidyr::gather(Country, Value, -Variable) %>% tidyr::spread(Variable, Value)
      nmList <- names(gfsi)
      gfsi <- data.frame(Country = gfsi$Country, apply(X = gfsi[,-1], MARGIN = 2, FUN = as.numeric))
      names(gfsi) <- nmList; rm(nmList)
      gfsi$Country <- as.character(gfsi$Country)
      
      AverageFoodSupply <- gfsi[,c(1, 8)]
      names(AverageFoodSupply)[2] <- "Food.available"
      AverageFoodSupply <- dplyr::inner_join(x = country_codes, y = AverageFoodSupply, by = c("country.name.en" = "Country"))
      AverageFoodSupply <- AverageFoodSupply %>% dplyr::select(country.name.en, iso3c, Food.available)
      
      ## 2. Food consumption
      ## Measure: Food security access
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
      
      city_access <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "city_access")
      names(city_access)[ncol(city_access)] <- "City.access"
      city_access <- dplyr::inner_join(x = country_codes, y = city_access %>% dplyr::select(ISO3, City.access), by = c("iso3c" = "ISO3"))
      city_access <- city_access %>% dplyr::select(country.name.en, iso3c, City.access)
      city_access$City.access[which(city_access$City.access == -9999)] <- NA
      city_access <- city_access %>% tidyr::drop_na()
      
      ## 4. Access to improved water resource
      ## Measure: Food security utilization
      ## Units: %
      ## Years: 1995:2014 (Selected period: 2005-2014)
      ## Countries with data: depends on the year
      ## Average produces more information: 2010-2014, 198 countries
      
      improved_water <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "access_improved_water")
      improved_water <- improved_water %>% dplyr::select(Country, Year, Value)
      names(improved_water)[3] <- "Access.improved.water"
      improved_water <- improved_water %>% tidyr::spread(key = Year, value = Access.improved.water)
      improved_water$Access.improved.water <- rowMeans(improved_water[,which(names(improved_water)=="2005"):which(names(improved_water)=="2014")], na.rm = T)
      improved_water$Country <- as.character(improved_water$Country)
      improved_water <- dplyr::inner_join(x = country_codes, y = improved_water, by = c("country.name.en" = "Country"))
      improved_water <- improved_water %>% dplyr::select(country.name.en, iso3c, Access.improved.water)
      improved_water <- improved_water[complete.cases(improved_water),]; rownames(improved_water) <- 1:nrow(improved_water)
      
      ## 5. Access to electricity
      ## Measure: Food security utilization
      ## Original name: Access to electricity
      ## Units: (%)
      ## Years: 1990:2015 (Selected period: 2000:2014)
      ## Countries with data: 211
      
      access_electricity <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "access_electricity", skip = 3)
      names(access_electricity)[1:2] <- c("Country", "ISO3")
      access_electricity <- access_electricity %>% dplyr::select(Country, ISO3, which(names(access_electricity)=="2000"):which(names(access_electricity)=="2014"))
      access_electricity <- access_electricity %>% tidyr::gather(Year, Value, -(Country:ISO3))
      access_electricity <- access_electricity %>% dplyr::select(ISO3, Country, Year, Value)
      names(access_electricity)[4] <- "Access.electricity"
      yearsList <- access_electricity$Year %>% unique %>% sort
      yearsList <- yearsList[-((length(yearsList)-1):length(yearsList))]
      access_electricityList <- lapply(1:length(yearsList), function(i){
        df <- access_electricity %>% dplyr::filter(Year == yearsList[i])
        df <- dplyr::inner_join(x = country_codes, y = df, by = c("iso3c" = "ISO3"))
        return(df)
      })
      recentAccessElectricity <- access_electricityList[[length(access_electricityList)]]
      recentAccessElectricity <- recentAccessElectricity %>% dplyr::select(country.name.en, iso3c, Access.electricity)
      recentAccessElectricity <- recentAccessElectricity[complete.cases(recentAccessElectricity),]; rownames(recentAccessElectricity) <- 1:nrow(recentAccessElectricity)
      rm(access_electricity, access_electricityList)
      
      ## 6. Price volatility index
      ## Measure: Food security stability
      ## Units: (CV %)
      ## Years: 2011:2017
      ## Countries with data: depends on the year
      ## Average produces more information: 2013-2017, 194 countries
      
      price_volatility <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "price_volatility")
      price_volatility <- price_volatility %>% dplyr::select(Country, Year, 4)
      names(price_volatility)[3] <- "Price.volatility.index"
      price_volatility$Price.volatility.index <- price_volatility$Price.volatility.index %>% as.character() %>% as.numeric()
      price_volatility <- price_volatility %>% tidyr::spread(key = Year, value = Price.volatility.index)
      price_volatility$Price.volatility.index <- rowMeans(price_volatility[,which(names(price_volatility)=="2013"):which(names(price_volatility)=="2017")], na.rm = T)
      price_volatility$Country <- as.character(price_volatility$Country)
      price_volatility <- dplyr::inner_join(x = country_codes, y = price_volatility, by = c("country.name.en" = "Country"))
      price_volatility <- price_volatility %>% dplyr::select(country.name.en, iso3c, Price.volatility.index)
      price_volatility <- price_volatility[complete.cases(price_volatility),]; rownames(price_volatility) <- 1:nrow(price_volatility)
      
      ## 7. Food supply variability
      ## Measure: Food security stability
      ## Units: ??
      ## Years: (Selected period: 2000:2011)
      ## Countries with data: 162
      
      fsvar <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "food_supply_variability")
      fsvar <- fsvar %>% select(Country, Year, Value)
      names(fsvar)[3] <- "Food.supply.variability"
      fsvar$Country <- as.character(fsvar$Country)
      fsvar <- fsvar %>% dplyr::filter(Year >= 2000)
      fsvar <- fsvar %>% filter(Year <= 2011)
      yearsList <- fsvar$Year %>% unique %>% sort
      fsvarList <- lapply(1:length(yearsList), function(i){
        df <- fsvar %>% filter(Year == yearsList[i])
        df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
        return(df)
      })
      recentFoodSupplyVar <- fsvarList[[length(fsvarList)]]
      recentFoodSupplyVar <- recentFoodSupplyVar %>% dplyr::select(country.name.en, iso3c, Food.supply.variability)
      recentFoodSupplyVar <- recentFoodSupplyVar[complete.cases(recentFoodSupplyVar),]; rownames(recentFoodSupplyVar) <- 1:nrow(recentFoodSupplyVar)
      rm(fsvar, fsvarList)
      
      ## 8. Burden of food borne illness
      ## Measure: Food safety
      ## Original name: Burden of food borne illness
      ## Units: ??
      ## Years: 2010
      ## Countries with data: 194
      
      foodBorne_illness <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "foodborne_illness")
      foodBorne_illness$Region <- NULL
      foodBorne_illness$Country <- as.character(foodBorne_illness$Country)
      foodBorne_illness <- dplyr::inner_join(x = country_codes, y = foodBorne_illness, by = c("country.name.en" = "Country"))
      foodBorne_illness <- foodBorne_illness %>% dplyr::select(country.name.en, iso3c, Foodborne.illness)
      
      ## 9. Food loss
      ## Measure: Food waste and use
      ## Original name: Total waste/total domestic supply quantity (tonnes)
      ## Units: (%)
      ## Years: 2016
      ## Countries with data: 113
      
      food_loss <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "food_loss")
      food_loss$Country <- as.character(food_loss$Country)
      food_loss <- dplyr::inner_join(x = country_codes, y = food_loss, by = c("country.name.en" = "Country"))
      food_loss <- food_loss %>% dplyr::select(country.name.en, iso3c, Food.loss)
      
      ## 10. Diet diversification
      ## Measure: Nutrition diet
      ## Original name: Diet diversification
      ## Units: (%)
      ## Years: 2001:2010
      ## Countries with data: 165
      
      diet_div <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "diet_diversification")
      diet_div <- diet_div %>% dplyr::select(Country, Year, Value)
      names(diet_div)[3] <- "Diet.diversification"
      diet_div$Country <- as.character(diet_div$Country)
      diet_div <- diet_div %>% dplyr::filter(Year >= 2000)
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
      yearsList <- diet_div$Year %>% unique %>% sort
      diet_divList <- lapply(1:length(yearsList), function(i){
        df <- diet_div %>% filter(Year == yearsList[i])
        df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
        return(df)
      })
      recentDietDiv <- diet_divList[[length(diet_divList)]]
      recentDietDiv <- recentDietDiv %>% dplyr::select(country.name.en, iso3c, Diet.diversification)
      recentDietDiv <- recentDietDiv[complete.cases(recentDietDiv),]; rownames(recentDietDiv) <- 1:nrow(recentDietDiv)
      rm(diet_div, diet_divList)
      
      ## 11. Crop diversity
      ## Measure: Nutrition and diet
      ## Units: Shannon diversity index
      ## Years: 2009:2011
      ## Countries with data: 177
      
      crop_diversity <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "crop_diversity")
      crop_diversity <- crop_diversity %>% dplyr::filter(Index == "Shannon" & Measurement == "Calories")
      crop_diversity <- crop_diversity %>% dplyr::select(Country, Year, Diversity)
      names(crop_diversity)[ncol(crop_diversity)] <- "Crop.diversity"
      crop_diversity <- crop_diversity %>% tidyr::spread(key = Year, value = Crop.diversity)
      crop_diversity$Crop.diversity <- rowMeans(x = crop_diversity[,2:ncol(crop_diversity)], na.rm = T)
      crop_diversity <- crop_diversity %>% dplyr::select(Country, Crop.diversity)
      crop_diversity$Country <- crop_diversity$Country %>% as.character
      crop_diversity <- dplyr::inner_join(x = country_codes, y = crop_diversity, by = c("country.name.en" = "Country"))
      crop_diversity <- crop_diversity %>% dplyr::select(country.name.en, iso3c, Crop.diversity)
      
      ## 12. Stunting
      ## Measure: Undernutrition
      ## Units: (%)
      ## Years: 2000:2014
      ## Countries with data: depends on the year
      ## Average produces more information: 2010-2014, 98 countries
      
      stunting <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "stunting", skip = 1)
      names(stunting)[4] <- "Stunting"
      stunting <- stunting %>% select(Country, Year, Stunting)
      stunting$Country <- as.character(stunting$Country)
      stunting <- stunting %>% dplyr::filter(Year >= 2000)
      stunting$Year <- as.numeric(as.character(stunting$Year))
      stunting <- stunting %>% spread(key = Year, value = Stunting)
      stunting$Stunting <- rowMeans(stunting[,which(names(stunting)=="2005"):which(names(stunting)=="2014")], na.rm = T)
      stunting <- stunting %>% dplyr::select(Country, Stunting)
      stunting <- dplyr::inner_join(x = country_codes, y = stunting, by = c("country.name.en" = "Country"))
      stunting <- stunting %>% dplyr::select(country.name.en, iso3c, Stunting)
      stunting <- stunting[complete.cases(stunting),]; rownames(stunting) <- 1:nrow(stunting)
      
      ## 13. Obesity
      ## Measure: Overnutrition
      ## Units: (%)
      ## Years: 2000:2014
      ## Countries with data: 191
      
      obesity <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "obesity", skip = 2)
      names(obesity)[1] <- "Country"
      names(obesity)[2:ncol(obesity)] <- gsub(pattern = "_1", replacement = "", x = names(obesity)[2:ncol(obesity)])
      obesity <- obesity[-1,]
      names(obesity)[2:ncol(obesity)] <- paste0(names(obesity)[2:ncol(obesity)], "-", obesity[1,2:ncol(obesity)])
      obesity <- obesity[-1,]
      obesity <- obesity %>% tidyr::gather(year_sex, Value, -Country) %>% tidyr::separate(year_sex, c("Year", "Sex"))
      obesity$Value <- gsub(pattern = " \\[*.*?\\]", replacement = "", x = obesity$Value) %>% as.character %>% as.numeric
      aux <- obesity %>% dplyr::group_by(Country, Year) %>% dplyr::summarise(Value = mean(Value, na.rm = T))
      aux$Sex <- "Average"
      obesity <- rbind(obesity %>% as.data.frame, aux %>% as.data.frame); rm(aux)
      obesity <- obesity %>% dplyr::filter(Sex == "Average")
      obesity$Country <- as.character(obesity$Country)
      obesity$Year <- as.numeric(as.character(obesity$Year))
      obesity$Sex <- NULL
      colnames(obesity)[3] <- "Obesity"
      obesity <- obesity %>% dplyr::filter(Year >= 2000)
      yearsList <- obesity$Year %>% unique %>% sort
      obesityList <- lapply(1:length(yearsList), function(i){
        df <- obesity %>% filter(Year == yearsList[i])
        df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
        return(df)
      })
      recentObesity <- obesityList[[length(obesityList)]]
      recentObesity <- recentObesity %>% dplyr::select(country.name.en, iso3c, Obesity)
      recentObesity <- recentObesity[complete.cases(recentObesity),]; rownames(recentObesity) <- 1:nrow(recentObesity)
      rm(obesity, obesityList)
      
      ## 14. Nutrient deficiency in vitamin A
      ## Measure: Overnutrition
      ## Units: (%)
      ## Years: 2000:2014
      ## Countries with data: 195
      
      serum_retinol <- readxl::read_excel(paste0(data_path, "/inputs_raw/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "nutrition_deficiency")
      serum_retinol$Country <- serum_retinol$Country %>% as.character
      serum_retinol <- dplyr::inner_join(x = country_codes, y = serum_retinol, by = c("country.name.en" = "Country"))
      serum_retinol <- serum_retinol %>% dplyr::select(country.name.en, iso3c, Serum.retinol.deficiency)
      
      # Consolidate Food and nutrition indicators
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
      
      foodNutDim <- foodNutDim[-which(apply(X = foodNutDim[,3:ncol(foodNutDim)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 14),]
      rownames(foodNutDim) <- foodNutDim$country.name.en
      foodNutDim$country.name.en <- NULL
      write.csv(foodNutDim, paste0(data_path, "/inputs_compiled/food_nutrition_dimension.csv"), row.names = T)
      
      rm(AverageFoodSupply, FoodConsumption, city_access, improved_water, recentAccessElectricity, price_volatility,
         recentFoodSupplyVar, food_loss, recentDietDiv, crop_diversity, stunting, recentObesity, yearsList, foodBorne_illness, serum_retinol)
      
    } else {
      food_nutritionDim <- read.csv(paste0(data_path, "/inputs_compiled/food_nutrition_dimension.csv"), row.names = 1)
    }
    
  } else {
    
    cat("    1.2. Intermediate target files already exists: loading them ...\n")
    
    # Load country codes list
    country_codes <- read.csv(paste0(data_path, "/country_codes/country_codes_28_09_18.csv"))
    
    Dimensions <- c("environmental", "economic", "social", "food_nutrition")
    grep2      <- Vectorize(grep, vectorize.args = "pattern") %>% unlist
    listed_fls <- listed_fls[grep2(Dimensions, listed_fls)]; rm(grep2)
    loaded_tbs <- listed_fls %>% purrr::map(., function(.){read.csv(., row.names = 1)}); rm(listed_fls)
    names(loaded_tbs) <- Dimensions; rm(Dimensions)
    
    all_data <- dplyr::left_join(x = country_codes %>% dplyr::select(iso3c), y = loaded_tbs[["environmental"]], by = "iso3c")
    all_data <- dplyr::left_join(x = all_data, y = loaded_tbs[["economic"]], by = "iso3c")
    all_data <- dplyr::left_join(x = all_data, y = loaded_tbs[["social"]], by = "iso3c")
    all_data <- dplyr::left_join(x = all_data, y = loaded_tbs[["food_nutrition"]], by = "iso3c")
    all_data <- all_data[-which(apply(X = all_data[,2:ncol(all_data)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 27),]
    
    all_data <- dplyr::right_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = all_data, by = "iso3c")
    rownames(all_data) <- all_data$country.name.en; all_data$country.name.en <- NULL
    rm(environmentDim, food_nutritionDim, socialDim, economicDim)
    write.csv(x = all_data, file = "./sfs_raw_indicators.csv", row.names = T)
    
  }
  
  return(cat(""))
  
}

## ========================================================================== ##
## ENVIRONMENT
## ========================================================================== ##



suppressMessages(pacman::p_load(tabplot, GGally, corrplot))

# Distributions and missing values representation
tableplot(environmentDim[,-1], nBins = nrow(environmentDim))

# Correlation
environmentDim[,-1] %>%
  cor(use = "complete.obs", method = "spearman") %>%
  corrplot(method = "square")

# PCA
FactoMineR::PCA(X = environmentDim[complete.cases(environmentDim),-1])

## ========================================================================== ##
## ECONOMICS
## ========================================================================== ##



# Distributions and missing values representation
tableplot(economicDim[,-1], nBins = nrow(economicDim))

# Correlation
economicDim[,-1] %>%
  cor(use = "complete.obs", method = "spearman") %>%
  corrplot(method = "square")

# PCA
FactoMineR::PCA(X = economicDim[complete.cases(economicDim),-1])

## ========================================================================== ##
## SOCIAL
## ========================================================================== ##



# Distributions and missing values representation
tableplot(socialDim[,-1], nBins = nrow(socialDim))

# Correlation
socialDim[,-1] %>%
  cor(use = "complete.obs", method = "spearman") %>%
  corrplot(method = "square")

# PCA
FactoMineR::PCA(X = socialDim[complete.cases(socialDim),-1])

## ========================================================================== ##
## FOOD AND NUTRITION
## ========================================================================== ##



# Distributions and missing values representation
tableplot(food_nutritionDim[,-1], nBins = nrow(food_nutritionDim))

# Correlation
food_nutritionDim[,-1] %>%
  cor(use = "complete.obs", method = "spearman") %>%
  corrplot(method = "square")

# PCA
FactoMineR::PCA(X = food_nutritionDim[complete.cases(food_nutritionDim),-1])
