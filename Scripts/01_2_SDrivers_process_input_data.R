# Processing and integrating data: SFS project - drivers
# Implemented by: H. Achicanoy & P. Alvarez
# CIAT, 2018

# R options
g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)

OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
wk_dir   <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9/Sustainable_Food_System/Drivers", "Windows" = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Drivers")
setwd(wk_dir); rm(wk_dir, OSysPath, OSys)

# Load packages
library(pacman)
pacman::p_load(raster, rgdal, maptools, jsonlite, foreach, doParallel, XML, plspm, reshape, tidyverse, countrycode, caret,
               missMDA, missForest, treemap, viridisLite, highcharter, corrplot, cluster, factoextra, FactoMineR, gghighlight,
               EnvStats, compiler, caretEnsemble)

## ========================================================================== ##
## Define countries to work with
## ========================================================================== ##

# Country code translation
country_codes <- countrycode::codelist %>% dplyr::select(country.name.en, iso3c, iso3n, iso2c, fao, wb)
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
## DEMAND-CONSUMER
## ========================================================================== ##

# We need to include water quality indicators
if(!file.exists("./demand_consumer.csv")){
  
  ## 1. Growing attention paid to diet and health
  ## Measure: Healthy diet, junk food, obesity, organic food
  ## Units: Search importance (0-100)
  ## Years: 2012:2016
  ## Countries with data: 95
  google_trends <- read.csv("./drivers_CB/Databases_modified/Demand_Consumer/Final/Change_google_trends.csv")
  
  google_trends <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = google_trends, by = "iso3c")
  google_trends %>% complete.cases %>% sum
  
  
  ## 2. Population growth ### change average over last 3 years
  ## Measure: Population growth
  ## Units: (annual %)
  ## Years: 2015
  ## Countries with data: 214
  
  pop_growth <- read.csv("./drivers_CB/Databases_modified/Demand_Consumer/Final/population_growth_annual_CB.csv")
  pop_growth$Country.Name <- pop_growth$Indicator.Name <- pop_growth$Indicator.Code <- NULL
  colnames(pop_growth)[1] <- "iso3c"
  colnames(pop_growth)[-1] <- gsub("X", "Y", colnames(pop_growth)[-1])
  
  pop_growth$chg_pop_growth <- pop_growth %>% dplyr::select(Y2014:Y2016) %>% rowMeans(., na.rm = T)
  pop_growth <- pop_growth %>% dplyr::select(iso3c, chg_pop_growth)
  
  pop_growth <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = pop_growth, by = "iso3c")
  pop_growth %>% complete.cases %>% sum
  
  
  ## 3. GDP growth  ### change average over last 3 years
  ## Measure: Raise in consumers' income
  ## Units: (annual %)
  ## Years: 2015
  ## Countries with data: 192
  
  gdp_growth <- read.csv("./drivers_CB/Databases_modified/Demand_Consumer/Final/gdp_annual_growth_CB.csv")
  gdp_growth$Country.Name <- gdp_growth$Series.Name <- gdp_growth$Series.Code <- NULL
  colnames(gdp_growth)[1] <- "iso3c"
  colnames(gdp_growth)[-1] <- paste0("Y", 2000:2016)
  
  gdp_growth$chg_gdp_growth <- gdp_growth %>% dplyr::select(Y2014:Y2016) %>% rowMeans(., na.rm = T)
  gdp_growth <- gdp_growth %>% dplyr::select(iso3c, chg_gdp_growth)
  
  gdp_growth <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = gdp_growth, by = "iso3c")
  gdp_growth %>% complete.cases %>% sum
  
  
  ## 4. Urban population ### change over time 10 years
  ## Measure: Urbanization
  ## Units: (% of total)
  ## Years: 2005:2015
  ## Countries with data: 213
  
  urban_pop <- read.csv("./drivers_CB/Databases_modified/Demand_Consumer/Final/urb_pop_perc_total_CB.csv")
  urban_pop$Country <- urban_pop$Indicator.Name <- urban_pop$Indicator.Code <- NULL
  colnames(urban_pop)[1] <- "iso3c"
  colnames(urban_pop)[-1] <- paste0("Y", 1960:2016)
  
  base <- urban_pop %>% dplyr::select(Y2004:Y2006) %>% rowMeans(., na.rm = T)
  recent <- urban_pop %>% dplyr::select(Y2014:Y2016) %>% rowMeans(., na.rm = T)
  urban_pop$chg_urban_pop <- (recent - base); rm(recent, base)
  urban_pop <- urban_pop %>% dplyr::select(iso3c, chg_urban_pop)
  
  urban_pop <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = urban_pop, by = "iso3c")
  urban_pop %>% complete.cases %>% sum
  
  
  ## 5. Employers, female ### change over time 10 years
  ## Measure: Woman involvement
  ## Units: (%)
  ## Years: 2008:2015
  ## Countries with data: 78
  
  employers <- read.csv("./drivers_CB/Databases_modified/Demand_Consumer/Final/female_employment_CB.csv")
  employers$Country.Name <- employers$Series.Name <- employers$Series.Code <- NULL
  colnames(employers)[1] <- "iso3c"
  colnames(employers)[-1] <- gsub("X", "Y", colnames(employers)[-1])
  
  base <- employers %>% dplyr::select(Y2007:Y2009) %>% rowMeans(., na.rm = T)
  recent <- employers %>% dplyr::select(Y2014:Y2016) %>% rowMeans(., na.rm = T)
  employers$chg_employers <- (recent - base); rm(recent, base)
  employers <- employers %>% dplyr::select(iso3c, chg_employers)
  
  employers <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = employers, by = "iso3c")
  employers %>% complete.cases %>% sum
  
  
  ## 6. Employment in industry, female ### change over time 10 years
  ## Measure: Woman involvement
  ## Units: (%)
  ## Years: 2005:2015
  ## Countries with data: 91
  
  empl_industry <- read.csv("./drivers_CB/Databases_modified/Demand_Consumer/Final/employment_in_industry_female_CB.csv")
  empl_industry$Country.Name <- empl_industry$Indicator.Name <- empl_industry$Indicator.Code <- NULL  ### se van a crear las columnas Series.name, Series.Code y Country.Name
  colnames(empl_industry)[1] <- "iso3c"
  colnames(empl_industry)[-1] <- gsub("X", "Y", colnames(empl_industry)[-1])
  
  base <- empl_industry %>% dplyr::select(Y2004:Y2006) %>% rowMeans(., na.rm = T)
  recent <- empl_industry %>% dplyr::select(Y2014:Y2016) %>% rowMeans(., na.rm = T)
  empl_industry$chg_empl_industry <- (recent - base); rm(recent, base)
  empl_industry <- empl_industry %>% dplyr::select(iso3c, chg_empl_industry)
  
  empl_industry <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = empl_industry, by = "iso3c")
  empl_industry %>% complete.cases %>% sum
  
  
  ## 7. Employment services, female ### change over time 10 years
  ## Measure: Woman involvement
  ## Units: (%)
  ## Years: 2005:2015
  ## Countries with data: 92
  
  empl_services <- read.csv("./drivers_CB/Databases_modified/Demand_Consumer/Final/employment_in_services_female_CB.csv")
  empl_services$Country.Name <- empl_services$Indicator.Name <- empl_services$Indicator.Code <- NULL
  colnames(empl_services)[1] <- "iso3c"
  colnames(empl_services)[-1] <- gsub("X", "Y", colnames(empl_services)[-1])
  
  base <- empl_services %>% dplyr::select(Y2004:Y2006) %>% rowMeans(., na.rm = T)
  recent <- empl_services %>% dplyr::select(Y2014:Y2016) %>% rowMeans(., na.rm = T)
  empl_services$chg_empl_services <- (recent - base); rm(recent, base)
  empl_services <- empl_services %>% dplyr::select(iso3c, chg_empl_services)
  
  empl_services <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = empl_services, by = "iso3c")
  empl_services %>% complete.cases %>% sum
  
  
  ## 8. Employment agriculture, female ### change over time 10 years
  ## Measure: Woman involvement
  ## Units: (%)
  ## Years: 2005:2015
  ## Countries with data: 87
  
  empl_agriculture <- read.csv("./drivers_CB/Databases_modified/Demand_Consumer/Final/employment_in_agriculture_female_CB.csv")
  empl_agriculture$Country.Name <- empl_agriculture$Indicator.Name <- empl_agriculture$Indicator.Code <- NULL
  colnames(empl_agriculture)[1] <- "iso3c"
  colnames(empl_agriculture)[-1] <- gsub("X", "Y", colnames(empl_agriculture)[-1])
  
  base <- empl_agriculture %>% dplyr::select(Y2004:Y2006) %>% rowMeans(., na.rm = T)
  recent <- empl_agriculture %>% dplyr::select(Y2014:Y2016) %>% rowMeans(., na.rm = T)
  empl_agriculture$chg_empl_agriculture <- (recent - base); rm(recent, base)
  empl_agriculture <- empl_agriculture %>% dplyr::select(iso3c, chg_empl_agriculture)
  
  empl_agriculture <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = empl_agriculture, by = "iso3c")
  empl_agriculture %>% complete.cases %>% sum
  
  
  demand_consumer <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = google_trends, by = c("country.name.en", "iso3c"))
  demand_consumer <- dplyr::left_join(x = demand_consumer, y = pop_growth, by = c("country.name.en", "iso3c"))
  demand_consumer <- dplyr::left_join(x = demand_consumer, y = gdp_growth, by = c("country.name.en", "iso3c"))
  demand_consumer <- dplyr::left_join(x = demand_consumer, y = urban_pop, by = c("country.name.en", "iso3c"))
  demand_consumer <- dplyr::left_join(x = demand_consumer, y = employers, by = c("country.name.en", "iso3c"))
  demand_consumer <- dplyr::left_join(x = demand_consumer, y = empl_industry, by = c("country.name.en", "iso3c"))
  demand_consumer <- dplyr::left_join(x = demand_consumer, y = empl_services, by = c("country.name.en", "iso3c"))
  demand_consumer <- dplyr::left_join(x = demand_consumer, y = empl_agriculture, by = c("country.name.en", "iso3c"))
  
  demand_consumer <- demand_consumer[-which(is.na(demand_consumer$iso3c)),]
  demand_consumer <- demand_consumer[-which(apply(X = demand_consumer[,3:ncol(demand_consumer)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 8),]
  rownames(demand_consumer) <- demand_consumer$country.name.en
  demand_consumer$country.name.en <- NULL
  
  write.csv(x = demand_consumer,
            file = "./demand_consumer.csv",
            row.names = T)
  rm(list = setdiff(ls(), c("demand_consumer", "country_codes")))
  
} else {
  demand_consumer <- read.csv("./demand_consumer.csv", row.names = 1)
  demand_consumer %>% complete.cases %>% sum
  demand_consumer[,-1] %>%
    cor(use = "pairwise.complete.obs", method = "spearman") %>%
    corrplot::corrplot(method = "square")
  demand_consumer[,-1] %>%
    FactoMineR::PCA(scale.unit = T, graph = T)
}

## ========================================================================== ##
## PRODUCTION SUPPLY
## ========================================================================== ##

if(!file.exists("./production_supply.csv")){
  
  ## 1. Annual temperature per country ## trend all years
  ## Units: (Celsius)
  ## Years: 1991:2015
  ## Countries with data: 227
  
  temp <- read.csv("./drivers_CB/Databases_modified/Production_Supply/Final/annual_temperature.csv")
  temp <- temp %>% tidyr::spread(key = Year, value = tas)
  temp$chg_hist_temp <- apply(temp, 1, function(x){
    y <- as.numeric(x[-1])
    TS <- ts(data = y, start = 1991, end = 2015, frequency = 1)
    slope <- trend::sens.slope(x = TS)
    return(slope$estimates)
  })
  colnames(temp)[1] <- "iso3c"
  temp <- temp %>% dplyr::select(iso3c, chg_hist_temp)

  temp <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = temp, by = "iso3c")
  temp %>% complete.cases %>% sum
  
  
  ## 2. Annual precipitation per country ## trend all years
  ## Units: (mm)
  ## Years: 1991:2015
  ## Countries with data: 205
  
  prec <- read.csv("./drivers_CB/Databases_modified/Production_Supply/Final/annual_rainfall.csv")
  prec <- prec %>% tidyr::spread(key = Year, value = pr)
  prec$chg_hist_prec <- apply(prec, 1, function(x){
    y <- as.numeric(x[-1])
    TS <- ts(data = y, start = 1991, end = 2015, frequency = 1)
    slope <- trend::sens.slope(x = TS)
    return(slope$estimates)
  })
  colnames(prec)[1] <- "iso3c"
  prec <- prec %>% dplyr::select(iso3c, chg_hist_prec)

  prec <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = prec, by = "iso3c")
  prec %>% complete.cases %>% sum
  
  
  ## 3. Annual variability in temperature per country ## trend all years
  ## Units: (Celsius)
  ## Years: 1991:2015
  ## Countries with data: 227
  
  sd_temp <- read.csv("./drivers_CB/Databases_modified/Production_Supply/Final/annual_variability_temperature.csv")
  sd_temp <- sd_temp %>% tidyr::spread(key = Year, value = tas)
  sd_temp$chg_sd_temp <- apply(sd_temp, 1, function(x){
    y <- as.numeric(x[-1])
    TS <- ts(data = y, start = 1991, end = 2015, frequency = 1)
    slope <- trend::sens.slope(x = TS)
    return(slope$estimates)
  })
  colnames(sd_temp)[1] <- "iso3c"
  sd_temp <- sd_temp %>% dplyr::select(iso3c, chg_sd_temp)
  
  sd_temp <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = sd_temp, by = "iso3c")
  sd_temp %>% complete.cases %>% sum
  
  
  ## 4. Annual variability in precipitation per country ## trend all years
  ## Units: (Celsius)
  ## Years: 1991:2015
  ## Countries with data: 205
  
  sd_prec <- read.csv("./drivers_CB/Databases_modified/Production_Supply/Final/annual_variability_rainfall.csv")
  sd_prec <- sd_prec %>% tidyr::spread(key = Year, value = pr)
  sd_prec$chg_sd_prec <- apply(sd_prec, 1, function(x){
    y <- as.numeric(x[-1])
    TS <- ts(data = y, start = 1991, end = 2015, frequency = 1)
    slope <- trend::sens.slope(x = TS)
    return(slope$estimates)
  })
  colnames(sd_prec)[1] <- "iso3c"
  sd_prec <- sd_prec %>% dplyr::select(iso3c, chg_sd_prec)
  
  sd_prec <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = sd_prec, by = "iso3c")
  sd_prec %>% complete.cases %>% sum
  
  
  ## 5. Cereal yield ## average last 10 years
  ## Measure:
  ## Units: (kh/ha)
  ## Years: 2005:2015
  ## Countries with data: 179
  
  cereal_yld <- read.csv("./drivers_CB/Databases_modified/Production_Supply/final/cereal_yield.csv")
  cereal_yld$Country.Name <- cereal_yld$Indicator.Name <- cereal_yld$Indicator.Code <- NULL
  colnames(cereal_yld)[1] <- "iso3c"
  colnames(cereal_yld)[-1] <- paste0("Y", 1961:2016)
  
  base <- cereal_yld %>% dplyr::select(Y2004:Y2006) %>% rowMeans(., na.rm = T)
  recent <- cereal_yld %>% dplyr::select(Y2014:Y2016) %>% rowMeans(., na.rm = T)
  cereal_yld$chg_cereal_yld <- (recent - base); rm(recent, base)
  cereal_yld <- cereal_yld %>% dplyr::select(iso3c, chg_cereal_yld)
  
  cereal_yld <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = cereal_yld, by = "iso3c")
  cereal_yld %>% complete.cases %>% sum
  
  
  ## 6. Agricultural land
  ## Measure: 
  ## Original name: 
  ## Units: ()
  ## Years: 
  ## Countries with data: 206
  
  ag_land <- read.csv("./drivers_CB/Databases_modified/Production_Supply/final/agricultural_land.csv")
  ag_land$Country.Name <- ag_land$Indicator.Name <- ag_land$Indicator.Code <- NULL
  colnames(ag_land)[1] <- "iso3c"
  colnames(ag_land)[-1] <- paste0("Y", 1960:2015)
  
  base <- ag_land %>% dplyr::select(Y2003:Y2005) %>% rowMeans(., na.rm = T)
  recent <- ag_land %>% dplyr::select(Y2013:Y2015) %>% rowMeans(., na.rm = T)
  ag_land$chg_ag_land <- (recent - base); rm(recent, base)
  ag_land <- ag_land %>% dplyr::select(iso3c, chg_ag_land)
  
  ag_land <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = ag_land, by = "iso3c")
  ag_land %>% complete.cases %>% sum
  
  
  ## 7. Road infrastructure
  ## Measure: 
  ## Units: (m/km)
  ## Years: 
  ## Countries with data: 246
  
  road_infr <- read.csv("./drivers_CB/Databases_modified/Production_Supply/final/Proxy_chg_road_infr.csv")
  road_infr <- road_infr %>% select(iso3c, MEAN)
  colnames(road_infr)[2] <- "chg_road_infr"
  road_infr <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = road_infr, by = "iso3c")
  road_infr %>% complete.cases %>% sum
  
  
  ## 8. Mobile cellular subscriptions (per 100 people)
  ## Measure: 
  ## Units: ()
  ## Years: 
  ## Countries with data: 201
  
  mobile <- read.csv("./drivers_CB/Databases_modified/Production_Supply/final/cellphone.csv")
  mobile$Country.Name <- mobile$Indicator.Name <- NULL
  colnames(mobile)[1] <- "iso3c"
  colnames(mobile)[-1] <- gsub("X", "Y", colnames(mobile)[-1])
  
  base <- mobile %>% dplyr::select(Y2004:Y2006) %>% rowMeans(., na.rm = T); base[which(base == 0)] <- 0.2841842
  recent <- mobile %>% dplyr::select(Y2014:Y2016) %>% rowMeans(., na.rm = T)
  mobile$chg_mobile <- (recent - base); rm(recent, base)
  mobile <- mobile %>% dplyr::select(iso3c, chg_mobile)
  
  mobile <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = mobile, by = "iso3c")
  mobile %>% complete.cases %>% sum
  
  
  ## 9. Cereal crop yield vs fertilizer application
  ## Measure: 
  ## Units: ()
  ## Years: 2003:2012
  ## Countries with data: 149
  
  yield_fertil <- read.csv("./drivers_CB/Databases_modified/Production_Supply/cereal_yield_vs_fertilizer.csv") %>% tbl_df()
  yield_fertil$id <- yield_fertil %>% group_indices(Country.Code) 
  yield_fertil$Total.population..Gapminder. <- NULL
  names(yield_fertil)[4:5] <- gsub(pattern = ".", replacement = "_", names(yield_fertil)[4:5], fixed = T)
  
  yield_fertil_base <- yield_fertil %>% 
    filter(Year >= 2002, Year <= 2004) %>% 
    group_by(Country.Code, id) %>% 
    dplyr::summarise(mean_Fertilizer_application_base = mean(Fertilizer_application, na.rm = T),
                     mean_Cereal_yield_base = mean(Cereal_yield, na.rm = T))
  
  yield_fertil_recent <- yield_fertil %>% 
    filter(Year >= 2011, Year <= 2013) %>% 
    group_by(Country.Code, id) %>% 
    dplyr::summarise(mean_Fertilizer_application_recent = mean(Fertilizer_application, na.rm = T),
                     mean_Cereal_yield_recent = mean(Cereal_yield, na.rm = T))
  
  yield_fertil_all <- inner_join(x = yield_fertil_base, y = yield_fertil_recent, by = c("Country.Code","id"))
  yield_fertil_all$Fertilizer_application_change <- (yield_fertil_all$mean_Fertilizer_application_recent - yield_fertil_all$mean_Fertilizer_application_base) #/yield_fertil_all$mean_Fertilizer_application_base
  yield_fertil_all$Cereal_yield_change <- (yield_fertil_all$mean_Cereal_yield_recent - yield_fertil_all$mean_Cereal_yield_base) #/yield_fertil_all$mean_Cereal_yield_base
  yield_fertil_all$ch_yield_fertil  <- yield_fertil_all$Cereal_yield_change / yield_fertil_all$Fertilizer_application_change 
  
  names(yield_fertil_all)
  
  Country <- yield_fertil %>% 
    dplyr::select( Country.Code, Countries) %>% 
    unique
  
  chg_yield_fert <- yield_fertil_all %>% 
    dplyr::select(Country.Code, ch_yield_fertil) %>% 
    left_join(Country,  ., by = 'Country.Code') %>% 
    rename(iso3c = 'Country.Code', country.name.en = 'Countries')
  rm(yield_fertil, yield_fertil_base, yield_fertil_recent, yield_fertil_all)
  chg_yield_fert %>% complete.cases %>% sum
  
  
  ## 10. Fertilizer consumption (kg/ha of arable land)
  ## Measure: 
  ## Units: ()
  ## Years: 2003:2013
  ## Countries with data: 152
  
  fertil_consump <- read.csv("./drivers_CB/Databases_modified/Production_Supply/final/fert_consump_kg_CB.csv")
  fertil_consump$Country.Name <- fertil_consump$Indicator.Name <- fertil_consump$Indicator.Code <- NULL
  colnames(fertil_consump)[1] <- "iso3c"
  colnames(fertil_consump)[-1] <- gsub("X", "Y", colnames(fertil_consump)[-1])
  
  base <- fertil_consump %>% dplyr::select(Y2002:Y2003) %>% rowMeans(., na.rm = T)
  recent <- fertil_consump %>% dplyr::select(Y2012:Y2014) %>% rowMeans(., na.rm = T)
  fertil_consump$chg_fertil_consump <- (recent - base); rm(recent, base)
  fertil_consump <- fertil_consump %>% dplyr::select(iso3c, chg_fertil_consump)
  
  fertil_consump <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = fertil_consump, by = "iso3c")
  fertil_consump %>% complete.cases %>% sum
  
  
  
  production_supply <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = temp, by = c("country.name.en", "iso3c"))
  production_supply <- dplyr::left_join(x = production_supply, y = prec, by = c("country.name.en", "iso3c"))
  production_supply <- dplyr::left_join(x = production_supply, y = sd_temp, by = c("country.name.en", "iso3c"))
  production_supply <- dplyr::left_join(x = production_supply, y = sd_prec, by = c("country.name.en", "iso3c"))
  production_supply <- dplyr::left_join(x = production_supply, y = cereal_yld, by = c("country.name.en", "iso3c"))
  production_supply <- dplyr::left_join(x = production_supply, y = ag_land, by = c("country.name.en", "iso3c"))
  production_supply <- dplyr::left_join(x = production_supply, y = road_infr, by = c("country.name.en", "iso3c"))
  production_supply <- dplyr::left_join(x = production_supply, y = mobile, by = c("country.name.en", "iso3c"))
  production_supply <- dplyr::left_join(x = production_supply, y = chg_yield_fert, by = c("country.name.en", "iso3c"))
  production_supply <- dplyr::left_join(x = production_supply, y = fertil_consump, by = c("country.name.en", "iso3c"))
  
  production_supply <- production_supply[-which(is.na(production_supply$iso3c)),]
  production_supply <- production_supply[-which(apply(X = production_supply[,3:ncol(production_supply)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 10),]
  rownames(production_supply) <- production_supply$country.name.en
  production_supply$country.name.en <- NULL
  
  write.csv(x = production_supply,
            file = "./production_supply.csv",
            row.names = T)
  rm(list = setdiff(ls(), c("production_supply", "country_codes")))
  
} else {
  production_supply <- read.csv("./production_supply.csv", row.names = 1)
  production_supply %>% complete.cases %>% sum
  production_supply[,-1] %>%
    cor(use = "pairwise.complete.obs", method = "spearman") %>%
    corrplot::corrplot(method = "square")
  production_supply[,-1] %>%
    FactoMineR::PCA(scale.unit = T, graph = T)
}

## ========================================================================== ##
## TRADE DISTRIBUTION
## ========================================================================== ##

if(!file.exists("./trade_distribution.csv")){
  
  ## 1. Food exports (% of merchandise exports)
  ## Measure: 
  ## Units:
  ## Years: 2005:2015
  ## Countries with data: 144
  
  food_export <- read.csv("./drivers_CB/Databases_modified/Trade_Distribution/food_export.csv")
  food_export$Country.name <- food_export$Indicator.Name <- food_export$Indicator.Code <- NULL
  colnames(food_export)[1] <- "iso3c"
  colnames(food_export)[-1] <- gsub("X", "Y", colnames(food_export)[-1])
  
  base <- food_export %>% dplyr::select(Y2004:Y2006) %>% rowMeans(., na.rm = T)
  recent <- food_export %>% dplyr::select(Y2014:Y2016) %>% rowMeans(., na.rm = T)
  food_export$chg_food_export <- (recent - base); rm(recent, base)
  food_export <- food_export %>% dplyr::select(iso3c, chg_food_export)
  
  food_export <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = food_export, by = "iso3c")
  food_export %>% complete.cases %>% sum
  
  
  ## 2. Change over time in foreign direct investment (US$ dollars per capita)
  ## Measure: 
  ## Units:
  ## Years: 2004:2014
  ## Countries with data: 186
  
  foreign_invest <- read.csv("./drivers_CB/Databases_modified/Trade_Distribution/foreign_dir_invest_dollars_CB.csv")
  foreign_invest$Country.Name <- foreign_invest$Indicator.Name <- foreign_invest$Indicator.Code <- NULL
  colnames(foreign_invest)[1] <- "iso3c"
  colnames(foreign_invest)[-1] <- gsub("X", "Y", colnames(foreign_invest)[-1])
  
  population <- read.csv("./drivers_CB/Databases_modified/Demand_Consumer/Final/population_total_annual_CB.csv")
  population$Country.Name <- population$Series.Name <- population$Series.Code <- NULL
  colnames(population)[1] <- "iso3c"
  colnames(population)[-1] <- gsub("X", "Y", colnames(population)[-1])
  population <- population %>% dplyr::select(iso3c, Y1970:Y2017)
  
  # Foreign investments
  base <- foreign_invest %>% dplyr::select(Y2004:Y2006) %>% rowMeans(., na.rm = T)
  recent <- foreign_invest %>% dplyr::select(Y2014:Y2016) %>% rowMeans(., na.rm = T)
  FDI <- data.frame(iso3c = foreign_invest$iso3c, base = base, recent = recent)
  base <- population %>% dplyr::select(Y2004:Y2006) %>% rowMeans(., na.rm = T)
  recent <- population %>% dplyr::select(Y2014:Y2016) %>% rowMeans(., na.rm = T)
  POP <- data.frame(iso3c = population$iso3c, base = base, recent = recent)
  ALL <- dplyr::inner_join(x = FDI, y = POP, by = "iso3c"); rm(base, recent, FDI, POP)
  base <- ALL$base.x/ALL$base.y
  recent <- ALL$recent.x/ALL$recent.y
  foreign_invest <- data.frame(iso3c = ALL$iso3c, chg_foreign_invest = (recent - base)); rm(recent, base)
  
  foreign_invest <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = foreign_invest, by = "iso3c")
  foreign_invest %>% complete.cases %>% sum
  
  
  ## 3. Change over time in services trade (US$ dollars per capita)
  ## Measure: 
  ## Units: 
  ## Years: 2004:2014
  ## Countries with data: 189
  
  serv_exp <- read.csv("./drivers_CB/Databases_modified/Trade_Distribution/service_exports.csv")
  serv_exp$Country.Name <- serv_exp$Indicator.Name <- serv_exp$Indicator.Code <- NULL
  colnames(serv_exp)[1] <- "iso3c"
  colnames(serv_exp)[-1] <- gsub("X", "Y", colnames(serv_exp)[-1])
  
  serv_imp <- read.csv("./drivers_CB/Databases_modified/Trade_Distribution/service_imports.csv")
  serv_imp$Country.Name <- serv_imp$Indicator.Name <- serv_imp$Indicator.Code <- NULL
  colnames(serv_imp)[1] <- "iso3c"
  colnames(serv_imp)[-1] <- gsub("X", "Y", colnames(serv_imp)[-1])
  
  serv_trd <- data.frame(iso3c = serv_exp$iso3c,
                         ifelse(is.na(as.matrix(serv_exp[,-1])),
                                ifelse(is.na(as.matrix(serv_imp[,-1])),
                                       NA,
                                       as.matrix(serv_imp[,-1])),
                                ifelse(is.na(as.matrix(serv_imp[,-1])),
                                       as.matrix(serv_exp[,-1]),
                                       as.matrix(serv_exp[,-1]) + as.matrix(serv_imp[,-1])))
                         )
  rm(serv_exp, serv_imp)
  serv_trd <- serv_trd %>% dplyr::select(iso3c, Y1970:Y2017)
  
  population <- read.csv("./drivers_CB/Databases_modified/Demand_Consumer/Final/population_total_annual_CB.csv")
  population$Country.Name <- population$Series.Name <- population$Series.Code <- NULL
  colnames(population)[1] <- "iso3c"
  colnames(population)[-1] <- gsub("X", "Y", colnames(population)[-1])
  population <- population %>% dplyr::select(iso3c, Y1970:Y2017)
  
  base <- serv_trd %>% dplyr::select(Y2004:Y2006) %>% rowMeans(., na.rm = T)
  recent <- serv_trd %>% dplyr::select(Y2014:Y2016) %>% rowMeans(., na.rm = T)
  SERV <- data.frame(iso3c = serv_trd$iso3c, base = base, recent = recent)
  base <- population %>% dplyr::select(Y2004:Y2006) %>% rowMeans(., na.rm = T)
  recent <- population %>% dplyr::select(Y2014:Y2016) %>% rowMeans(., na.rm = T)
  POP <- data.frame(iso3c = population$iso3c, base = base, recent = recent)
  ALL <- dplyr::inner_join(x = SERV, y = POP, by = "iso3c"); rm(base, recent, SERV, POP)
  base <- ALL$base.x/ALL$base.y
  recent <- ALL$recent.x/ALL$recent.y
  serv_trd <- data.frame(iso3c = ALL$iso3c, chg_serv_trd = (recent - base)); rm(recent, base)
  
  serv_trd <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = serv_trd, by = "iso3c")
  serv_trd %>% complete.cases %>% sum
  
  
  trade_distribution <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = food_export, by = c("country.name.en", "iso3c"))
  trade_distribution <- dplyr::left_join(x = trade_distribution, y = foreign_invest, by = c("country.name.en", "iso3c"))
  trade_distribution <- dplyr::left_join(x = trade_distribution, y = serv_trd, by = c("country.name.en", "iso3c"))
  
  trade_distribution <- trade_distribution[-which(is.na(trade_distribution$iso3c)),]
  trade_distribution <- trade_distribution[-which(apply(X = trade_distribution[,3:ncol(trade_distribution)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 3),]
  rownames(trade_distribution) <- trade_distribution$country.name.en
  trade_distribution$country.name.en <- NULL
  
  write.csv(x = trade_distribution,
            file = "./trade_distribution.csv",
            row.names = T)
  rm(list = setdiff(ls(), c("trade_distribution", "country_codes")))
  
} else {
  trade_distribution <- read.csv("./trade_distribution.csv", row.names = 1)
  trade_distribution %>% complete.cases %>% sum
  trade_distribution[,-1] %>%
    cor(use = "pairwise.complete.obs", method = "spearman") %>%
    corrplot::corrplot(method = "square")
  trade_distribution[,-1] %>%
    FactoMineR::PCA(scale.unit = T, graph = T)
}

## ========================================================================== ##
## ALL DRIVERS
## ========================================================================== ##

if(file.exists("./demand_consumer.csv")){demand_consumer <- read.csv("./demand_consumer.csv", row.names = 1)}
if(file.exists("./production_supply.csv")){production_supply <- read.csv("./production_supply.csv", row.names = 1)}
if(file.exists("./trade_distribution.csv")){trade_distribution <- read.csv("./trade_distribution.csv", row.names = 1)}

drivers <- dplyr::left_join(x = country_codes %>% dplyr::select(iso3c), y = demand_consumer, by = "iso3c")
drivers <- dplyr::left_join(x = drivers, y = production_supply, by = "iso3c")
drivers <- dplyr::left_join(x = drivers, y = trade_distribution, by = "iso3c")
drivers <- drivers[-which(is.na(drivers$iso3c)),]
drivers <- drivers[-which(apply(X = drivers[,2:ncol(drivers)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 19),]

drivers <- dplyr::right_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = drivers, by = "iso3c")
rownames(drivers) <- drivers$country.name.en; drivers$country.name.en <- NULL
rm(demand_consumer, production_supply, trade_distribution)

drivers %>% complete.cases %>% sum
drivers[,-1] %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
  corrplot::corrplot(method = "square")
drivers[,-1] %>%
  FactoMineR::PCA(scale.unit = T, graph = T) # %>%
  # FactoMineR::HCPC(res = ., nb.clust = -1) %>%
