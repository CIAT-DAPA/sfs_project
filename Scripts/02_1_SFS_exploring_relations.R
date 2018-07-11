# Sensitivity analysis: SFS project
# Implemented by: H. Achicanoy & P. Alvarez
# CIAT, 2018

# R options
g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)
wk_dir <- "D:/OneDrive - CGIAR/escritorio2/SFS_A4NH_conference/Data/Indicators"
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
# country_codes <- countrycode_data %>% dplyr::select(country.name.en, iso3c, iso3n, iso2c, fao, wb)
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
## Load data by dimension
## ========================================================================== ##

if(file.exists("environmental_dimension.csv")){environmentDim <- read.csv("environmental_dimension.csv", row.names = 1)}
if(file.exists("economic_dimension.csv")){economicDim <- read.csv("economic_dimension.csv", row.names = 1)}
if(file.exists("social_dimension.csv")){socialDim <- read.csv("social_dimension.csv", row.names = 1)}
if(file.exists("food_nutrition_dimension.csv")){food_nutritionDim <- read.csv("food_nutrition_dimension.csv", row.names = 1)}

all_data <- dplyr::left_join(x = country_codes %>% dplyr::select(iso3c), y = environmentDim, by = "iso3c")
all_data <- dplyr::left_join(x = all_data, y = economicDim, by = "iso3c")
all_data <- dplyr::left_join(x = all_data, y = socialDim, by = "iso3c")
all_data <- dplyr::left_join(x = all_data, y = food_nutritionDim, by = "iso3c")
all_data <- all_data[-which(apply(X = all_data[,2:ncol(all_data)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 27),]
all_data <- all_data[-which(is.na(all_data$iso3c)),]

all_data <- dplyr::right_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = all_data, by = "iso3c")
rownames(all_data) <- all_data$country.name.en; all_data$country.name.en <- NULL
rm(environmentDim, food_nutritionDim, socialDim, economicDim)

## ========================================================================== ##
## Function for calculating SFS index
## ========================================================================== ##

calc_sfs_index <- function(combList = textFile2_uptd[[17]], data = all_data, fnt_type = "geometric"){
  
  # Step 1. Normalization function for all indicators
  normalization <- function(x){
    y = x/max(x, na.rm = T)
    # y = (x - min(x))/(max(x) - min(x))
    return(y)
  }
  
  for(j in 2:ncol(data)){
    data[,j] <- normalization(x = data[,j])
    data[which(data[,j] == 0), j] <- data[which(data[,j] == 0), j] + 0.01
  }; rm(j)
  
  # Updating dimension indexes
  signs <- c(NA, -1, +1, -1, +1, -1, +1, -1, +1, -1, +1, +1, +1, +1, +1, -1, +1, +1, +1, -1, -1, -1, -1, +1, +1, -1, -1, -1)
  envPos <- 2:8; ecoPos <- 9:11; socPos <- 12:14; fntPos <- 15:28
  mtch <- match(combList, names(data))
  envUpt <- base::intersect(envPos, mtch)
  ecoUpt <- base::intersect(ecoPos, mtch)
  socUpt <- base::intersect(socPos, mtch)
  fntUpt <- base::intersect(fntPos, mtch)
  
  # Updating data set
  data <- data[which(complete.cases(data[, mtch])),]
  
  # HDI approach
  HDI_approach <- function(data = data, varInd = mtch, theory = theory, fnt_type = "geometric"){
    
    rNames <- data$iso3c
    
    # Step 2. Normalize indicadors and apply a correction for those indicators which have negative polarity
    for(m in mtch){
      if(theory == "true"){
        if(signs[m] < 0){
          data[,m] <- 1 - data[,m]
          data[which(data[,m] == 0), m] <- data[which(data[,m] == 0), m] + 0.01
        }
      }
    }; rm(m)
    
    # Step 3. Calculate an index for each dimension
    # Environmental: geometric mean
    if(length(envUpt) > 1){envAve <- apply(X = data[,envUpt], MARGIN = 1, EnvStats::geoMean)} else {envAve <- data[,envUpt]}
    # Economic: arithmetic mean
    if(length(ecoUpt) > 1){ecoAve <- rowMeans(data[,ecoUpt])} else {ecoAve <- data[,ecoUpt]}
    # Social: geometric mean
    if(length(socUpt) > 1){socAve <- apply(X = data[,socUpt], MARGIN = 1, EnvStats::geoMean)} else {socAve <- data[,socUpt]}
    # Food and nutrition: For testing
    if(fnt_type == "geometric"){
      if(length(fntUpt) > 1){fntAve <- apply(X = data[,fntUpt], MARGIN = 1, EnvStats::geoMean)} else {fntAve <- data[,fntUpt]}
    } else {
      if(fnt_type == "arithmetic"){
        if(length(fntUpt) > 1){fntAve <- rowMeans(data[,fntUpt])} else {fntAve <- data[,fntUpt]}
      }
    }
    
    indices <- data.frame(Environment = envAve, Economic = ecoAve, Social = socAve, Food_nutrition = fntAve)
    
    # Step 4. Calculate a final composite index
    indices$SFS_index <- indices %>% dplyr::select(Environment:Food_nutrition) %>% apply(X = ., MARGIN = 1, EnvStats::geoMean)
    rownames(indices) <- rNames
    
    return(indices)
  }
  ref_vals <- HDI_approach(data = data, varInd = mtch, theory = "true", fnt_type = fnt_type)
  return(ref_vals)
  
}

## ========================================================================== ##
## Combinatory analysis
## ========================================================================== ##
# Post-process indicators names
textFile <- readLines("./Results/modelling_results/auxTxt.txt")
textFile <- unlist(strsplit(x = textFile, split = "], [", fixed = T))
textFile <- lapply(textFile, function(x){
  
  txt_str <- x
  txt_str <- gsub(pattern = "_", replacement = ", ", x = txt_str)
  txt_str <- gsub(pattern = "'", replacement = "", x = txt_str)
  txt_str <- gsub(pattern = "\\]\\]", replacement = "", x = txt_str)
  txt_str <- gsub(pattern = "\\[\\[", replacement = "", x = txt_str)
  txt_str <- unlist(strsplit(x = txt_str, split = ", "))
  return(txt_str)
  
})

## ========================================================================== ##
## Selecting the maximum combination values
## ========================================================================== ##
dfs <- readRDS("./Results/modelling_results/metrics_modeA_maxIter500.RDS")
dfs$Combination <- 1:nrow(dfs)
nInd <- dfs$nIndicators %>% unique %>% sort
combID <- rep(NA, length(nInd))
for(i in 1:length(nInd)){
  df <- dfs %>% select(nCountries, nIndicators, Combination)
  df <- df %>% filter(nIndicators == nInd[i])
  df <- df[which.max(df$nCountries),]
  combID[i] <- df$Combination
}; rm(i, df, dfs, nInd)
textFile2 <- textFile[combID]; rm(combID)

## ========================================================================== ##
## Calculate SFS index
## ========================================================================== ##

sfs_index <- calc_sfs_index(combList = textFile2[[17]], data = all_data, fnt_type = "arithmetic")
sfs_index$iso3c <- rownames(sfs_index)
sfs_index <- left_join(x = sfs_index, y = country_codes %>% select(iso3c, country.name.en), by = "iso3c")
write.csv(sfs_index, "./sfs_index.csv", row.names = F)

sfs_index <- left_join(x = sfs_index, y = all_data %>% select(iso3c, textFile2[[17]]), by = "iso3c")
write.csv(sfs_index, "./sfs_index_indicators.csv", row.names = F)

sfs_index2 <- sfs_index

normalization <- function(x){
  y = x/max(x, na.rm = T)
  # y = (x - min(x))/(max(x) - min(x))
  return(y)
}
sfs_index2[,textFile2[[17]]] <- apply(sfs_index2[,textFile2[[17]]], 2, function(x){
  x <- normalization(x = x)
  x[which(x == 0)] <- x[which(x == 0)] + 0.01
  return(x)
})

signs <- c(-1, -1, 1, -1, 1, 1, 1, 1, -1, 1, 1, 1, -1, -1, -1, -1, 1, 1, -1, -1)
mtch <- match(textFile2[[17]], names(sfs_index2))

for(m in mtch){
  if(signs[m-7] < 0){
    sfs_index2[,m] <- 1 - sfs_index2[,m]
    sfs_index2[which(sfs_index2[,m] == 0), m] <- sfs_index2[which(sfs_index2[,m] == 0), m] + 0.01
  }
}; rm(m)

write.csv(sfs_index2, "./sfs_index_normalized_indicators.csv", row.names = F)
sfs_index <- read.csv("./sfs_index_normalized_indicators.csv")

sfs_index %>%
  select(SFS_index, Emissions.agriculture.total:Serum.retinol.deficiency) %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
  corrplot::corrplot(method = "square")

sfs_index %>%
  select(SFS_index, Emissions.agriculture.total:Serum.retinol.deficiency) %>%
  glm(SFS_index ~ ., data = ., family = binomial) %>%
  summary

library(ranger)
rf_fit <- sfs_index %>%
  select(SFS_index, Emissions.agriculture.total:Serum.retinol.deficiency) %>%
  ranger(SFS_index ~ ., data = ., num.trees = 2000, importance = "impurity")

sort(rf_fit$variable.importance, decreasing = T)

library(DALEX)
explainer_rf <- DALEX::explain(model = rf_fit, data = sfs_index %>%
                          select(Emissions.agriculture.total:Serum.retinol.deficiency),
                        y = sfs_index$SFS_index)

vd_rf <- DALEX::variable_importance(explainer = explainer_rf, type = "raw")

expl_rf <- variable_response(explainer_rf, variable = "Diet.diversification", type = "pdp")
plot(expl_rf)


explainer <- lime::lime(x = sfs_index, rf_fit)
model_type(rf_fit)
explanation <- lime::explain(x = sfs_index %>%
                               select(Emissions.agriculture.total:Serum.retinol.deficiency),
                             explainer = explainer,
                             )


## ========================================================================== ##
## Drivers processed: change over time
## ========================================================================== ##

drivers <- read.csv("../Drivers/drivers_chng_processed.csv")
drivers <- drivers %>% dplyr::filter(!is.na(iso3c))
drivers$aux <- apply(drivers[,-1], 1, function(x){sum(is.na(x))})
drivers <- drivers %>% dplyr::filter(aux != 16)
rownames(drivers) <- drivers$country.name.en
drivers$country.name.en <- NULL
drivers$aux <- NULL
drivers$chg_pop_growth <- drivers$chg_pop_growth %>% as.character %>% as.numeric

## ========================================================================== ##
## Correlations
## ========================================================================== ##

M <- cor(drivers[,-1], use = "pairwise.complete.obs", method = "spearman")
corrplot(M, method = "square")

## ========================================================================== ##
## Missing data
## ========================================================================== ##

library(tabplot)
tab <- tabplot::tableplot(dat = drivers[,-1], nBins = nrow(drivers), fontsize = 6)
tabplot::tableSave(tab, filename = "missing_data_drivers.png", width = 8, height = 6, fontsize = 6, legend.lines = 6)

#############################################################################
sfsMap <- function(Scores){
  
  thm <- 
    hc_theme(
      colors = c("#1a6ecc", "#434348", "#90ed7d"),
      chart = list(
        backgroundColor = "transparent",
        style = list(fontFamily = "Source Sans Pro")
      ),
      xAxis = list(gridLineWidth = 1)
    )
  
  data("worldgeojson")
  # Scores <- apply(X = Scores, MARGIN = 2, FUN = function(x){(x - min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}) %>% as.data.frame
  indices <- data.frame(iso3 = rownames(Scores), round(Scores, 2))
  
  n <- 4
  colstops <- data.frame(
    q = 0:n/n,
    c = substring(viridis(n + 1), 0, 7)) %>%
    list.parse2()
  
  return(
    highchart(type = "map") %>%
      hc_add_series_map(map = worldgeojson, df = indices, value = "SFS_index", joinBy = "iso3") %>%
      hc_colorAxis(stops = color_stops()) %>%
      hc_tooltip(useHTML = TRUE, headerFormat = "",
                 pointFormat = "{point.name} has a SFS index of {point.SFS_index}") %>%
      # hc_colorAxis(stops = colstops, min = 0, max = 1) %>%
      hc_colorAxis(stops = colstops, min = min(indices$SFS_index, na.rm = T), max = max(indices$SFS_index, na.rm = T)) %>%
      hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
      hc_mapNavigation(enabled = TRUE) %>%
      hc_add_theme(thm)
  )
  
}
drivers2 <- drivers
rownames(drivers2) <- drivers2$iso3c
drivers2$iso3c <- NULL
aux <- drivers2 %>% select(chg_empl_services)
colnames(aux) <- "SFS_index"
aux <- data.frame(aux)
#aux <- aux %>% filter(!(row.names(.) %in% c("BIH", "NRU")))
#rownames(aux) <- drivers$iso3c[setdiff(1:nrow(drivers), which(drivers$iso3c == "BIH" | drivers$iso3c == "NRU"))]
sfsMap(Scores = aux)
#############################################################################

drivers3 <- drivers2[,which(apply(drivers2, 2, function(x){sum(is.na(x))/length(x)}) < 0.3)]

tbl <- merge(x = sfs_index, y = drivers3, by = "row.names")
sum(complete.cases(tbl))

M <- cor(tbl[,-1], use = "pairwise.complete.obs", method = "spearman")
corrplot(M, method = "square")

library(caret)
library(ranger)

tbl %>% select(SFS_index, chg_pop_growth:chg_merch_trade) %>% pairs
lm(SFS_index ~ ., data = tbl %>% select(SFS_index, chg_pop_growth:chg_merch_trade)) %>% summary

rf_fit <- caret::train(SFS_index ~ .,
                       data = tbl %>% select(SFS_index, chg_pop_growth:chg_merch_trade) %>% filter(complete.cases(.)),
                       method = "ranger")
partial(rf_fit, pred.var = "chg_merch_trade", plot = T, rug = T)
