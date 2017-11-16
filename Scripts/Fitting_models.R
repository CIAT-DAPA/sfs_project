# Fitting models: SFS project
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
suppressMessages(if(!require(plspm)){install.packages('plspm'); library(plspm)} else {library(plspm)})
suppressMessages(if(!require(caret)){install.packages('caret'); library(caret)} else {library(caret)})
suppressMessages(if(!require(missMDA)){install.packages('missMDA'); library(missMDA)} else {library(missMDA)})
suppressMessages(if(!require(missForest)){install.packages('missForest'); library(missForest)} else {library(missForest)})
suppressMessages(if(!require(treemap)){install.packages('treemap'); library(treemap)} else {library(treemap)})
suppressMessages(if(!require(viridisLite)){install.packages('viridisLite'); library(viridisLite)} else {library(viridisLite)})
suppressMessages(if(!require(highcharter)){install.packages('highcharter'); library(highcharter)} else {library(highcharter)})
suppressMessages(if(!require(corrplot)){install.packages('corrplot'); library(corrplot)} else {library(corrplot)})
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
## Load data by dimension
## ========================================================================== ##

if(file.exists("environmental_dimension.csv")){environmentDim <- read.csv("environmental_dimension.csv", row.names = 1)}
if(file.exists("economic_dimension.csv")){economicDim <- read.csv("economic_dimension.csv", row.names = 1)}
if(file.exists("social_dimension.csv")){socialDim <- read.csv("social_dimension.csv", row.names = 1)}
if(file.exists("food_nutrition_dimension.csv")){food_nutritionDim <- read.csv("food_nutrition_dimension.csv", row.names = 1)}

environmentDim <- environmentDim[-which(apply(X = environmentDim[,2:ncol(environmentDim)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == round((ncol(environmentDim)-1)/2)),]
food_nutritionDim <- food_nutritionDim[-which(apply(X = food_nutritionDim[,2:ncol(food_nutritionDim)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == round((ncol(food_nutritionDim)-1)/2)+1),]

all_data <- dplyr::left_join(x = country_codes %>% dplyr::select(iso3c), y = environmentDim, by = "iso3c")
all_data <- dplyr::left_join(x = all_data, y = economicDim, by = "iso3c")
all_data <- dplyr::left_join(x = all_data, y = socialDim, by = "iso3c")
all_data <- dplyr::left_join(x = all_data, y = food_nutritionDim, by = "iso3c")

all_data <- all_data[-which(apply(X = all_data[,2:ncol(all_data)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 23),]
all_data <- all_data[-which(is.na(all_data$iso3c)),]
rownames(all_data) <- all_data$iso3c
all_data[complete.cases(all_data),] %>% View

tabplot::tableplot(all_data[,-1], nBins = nrow(all_data))
pdf("corrMat_allVariables.pdf", height = 8, width = 8)
corrplot::corrplot.mixed(cor(all_data[complete.cases(all_data),-1], method = "spearman"), upper = "square",
                         tl.pos = "lt", diag = "u", mar = c(0, 0, 0, 0), number.cex = 0.5)
dev.off()

## ========================================================================== ##
## Exploring missing data imputation techniques
## ========================================================================== ##

## Missing data for rows
all_data$mdCount <- apply(X = all_data[,-1], MARGIN = 1, FUN = function(x) sum(is.na(x)))
missingDataMap <- function(Scores){
  
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
  
  Scores <- Scores[,-1]
  indices <- data.frame(iso3 = rownames(Scores), round(Scores, 2))
  
  n <- 4
  colstops <- data.frame(
    q = 0:n/n,
    c = substring(viridis(n + 1), 0, 7)) %>%
    list.parse2()
  
  return(
    highchart(type = "map") %>%
      hc_add_series_map(map = worldgeojson, df = indices, value = "mdCount", joinBy = "iso3") %>%
      hc_colorAxis(stops = color_stops()) %>%
      hc_tooltip(useHTML = TRUE, headerFormat = "",
                 pointFormat = "Number of missing data for {point.name}: {point.mdCount}") %>%
      hc_colorAxis(stops = colstops) %>%
      hc_legend(valueDecimals = 0, valueSuffix = "") %>%
      hc_mapNavigation(enabled = TRUE) %>%
      hc_add_theme(thm)
  )
  
}
missingDataMap(Scores = all_data)

all_data %>% ggplot(aes(x = mdCount)) + geom_histogram(bins = 20) +
  xlab("Number of missing data") + ylab("Frequency")

# Omiting variables with more than 10 missing data per all variables
all_data <- all_data %>% filter(mdCount < 10)
rownames(all_data) <- all_data$iso3c; all_data$mdCount <- NULL
tabplot::tableplot(all_data[,-1], nBins = nrow(all_data))

## Exploring missing data distribution
miss.enviroment <- data.frame(Dimension = rep("Enviroment", ncol(environmentDim)-1),
                              Indicator = names(environmentDim)[2:ncol(environmentDim)],
                              Missing.data = apply(X = environmentDim[match(all_data$iso3c, environmentDim$iso3c),-1], MARGIN = 2, FUN = function(x){round(100*(sum(is.na(x))/nrow(all_data)), 2)}))
rownames(miss.enviroment) <- 1:nrow(miss.enviroment)
miss.economic <- data.frame(Dimension = rep("Economic", ncol(economicDim)-1),
                            Indicator = names(economicDim)[2:ncol(economicDim)],
                            Missing.data = apply(X = economicDim[match(all_data$iso3c, economicDim$iso3c),-1], MARGIN = 2, FUN = function(x){round(100*(sum(is.na(x))/nrow(all_data)), 2)}))
rownames(miss.economic) <- 1:nrow(miss.economic)
miss.social <- data.frame(Dimension = rep("Social", ncol(socialDim)-1),
                            Indicator = names(socialDim)[2:ncol(socialDim)],
                            Missing.data = apply(X = socialDim[match(all_data$iso3c, socialDim$iso3c),-1], MARGIN = 2, FUN = function(x){round(100*(sum(is.na(x))/nrow(all_data)), 2)}))
rownames(miss.social) <- 1:nrow(miss.social)
miss.foodNut <- data.frame(Dimension = rep("Food_nutrition", ncol(food_nutritionDim)-1),
                          Indicator = names(food_nutritionDim)[2:ncol(food_nutritionDim)],
                          Missing.data = apply(X = food_nutritionDim[match(all_data$iso3c, food_nutritionDim$iso3c),-1], MARGIN = 2, FUN = function(x){round(100*(sum(is.na(x))/nrow(all_data)), 2)}))
rownames(miss.foodNut) <- 1:nrow(miss.foodNut)
missing_data <- rbind(miss.enviroment, miss.economic, miss.social, miss.foodNut)
rm(miss.enviroment, miss.economic, miss.social, miss.foodNut)

thm <- 
  hc_theme(
    colors = c("#1a6ecc", "#434348", "#90ed7d"),
    chart = list(
      backgroundColor = "transparent",
      style = list(fontFamily = "Source Sans Pro")
    ),
    xAxis = list(
      gridLineWidth = 1
    )
  )
tm <- treemap(missing_data, index = c("Dimension", "Indicator"),
              vSize = "Missing.data", vColor = "Missing.data",
              type = "value", palette = rev(viridis(6)))
highchart() %>% 
  hc_add_series_treemap(tm, allowDrillToNode = TRUE,
                        layoutAlgorithm = "squarified") %>% 
  hc_add_theme(thm)
rm(missing_data)

## Applying Random Forest for missing data imputation
set.seed(1235) # set.seed(2456); set.seed(4859); set.seed(7863)
seeds <- round(runif(n = 100000, min = 0, max = 150000))
set.seed(seeds[1])
all_data.imp <- missForest(all_data[,-1])

## ========================================================================== ##
## Fitting PLS-PM with missing data
## ========================================================================== ##

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
  Scores <- apply(X = Scores, MARGIN = 2, FUN = function(x){(x - min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}) %>% as.data.frame
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
      hc_colorAxis(stops = colstops) %>%
      hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
      hc_mapNavigation(enabled = TRUE) %>%
      hc_add_theme(thm)
  )
  
}

# ----------------------------------------------------- #
# Repeated indicators approach
# ----------------------------------------------------- #

# Inner model
sfs_path <- rbind(c(0, 0, 0, 0, 0),
                  c(0, 0 ,0 ,0, 0),
                  c(0, 0, 0, 0, 0),
                  c(0, 0, 0, 0, 0),
                  c(1, 1, 1, 1, 0))
rownames(sfs_path) <- colnames(sfs_path) <- c("Environment", "Economic", "Social", "Food_nutrition", "SFS_index")
innerplot(sfs_path)

# Blocks of variables: repeated indicators approach
sfs_blocks1 <- list(2:8, 9:10, 11:12, 13:24, 2:24)

# Scaling
sfs_scaling <- list(rep("NUM", length(sfs_blocks1[[1]])),
                    rep("NUM", length(sfs_blocks1[[2]])),
                    rep("NUM", length(sfs_blocks1[[3]])),
                    rep("NUM", length(sfs_blocks1[[4]])),
                    rep("NUM", length(sfs_blocks1[[5]])))

# Modes
sfs_modes <- c("A", "A", "A", "A", "A")

# PLS-PM with missing data
sfs_pls1 <- plspm(all_data[complete.cases(all_data),], sfs_path, sfs_blocks1, scaling = sfs_scaling, 
                  modes = sfs_modes, scheme = "centroid", plscomp = c(1,1,1,1,1), tol = 0.0000001)
                  # boot.val = T, br = 500) # plscomp = c(1,1,1,1), tol = 0.0000001
plot(sfs_pls1)
pairs(sfs_pls1$scores)
sfsMap(Scores = sfs_pls1$scores)

# PLS-PM without missing data, imputed by Random Forest
all_data.RF.Imp <- data.frame(all_data$iso3c, all_data.imp$ximp)
rownames(all_data.RF.Imp) <- all_data$iso3c
sfs_pls2 <- plspm(all_data.RF.Imp, sfs_path, sfs_blocks1, scaling = sfs_scaling, 
                  modes = sfs_modes, scheme = "centroid", plscomp = c(1,1,1,1,1), tol = 0.0000001)
                  # boot.val = T, br = 500) # plscomp = c(1,1,1,1), tol = 0.0000001
plot(sfs_pls2)
pairs(sfs_pls2$scores)
sfsMap(Scores = sfs_pls2$scores)

sfs_rf <- randomForest(Food_nutrition ~ ., data=sfs_pls2$scores)
plot(sfs_rf)
partialPlot(sfs_rf, sfs_pls2$scores, Environment)
partialPlot(sfs_rf, sfs_pls2$scores, Economic)
partialPlot(sfs_rf, sfs_pls2$scores, Social)

# ----------------------------------------------------- #
# Two-step approach
# ----------------------------------------------------- #

pairs(
  data.frame(
    FactoMineR::PCA(X = all_data[complete.cases(all_data),sfs_blocks1[[1]]], scale.unit = T, graph = F)$ind$coord[,1],
    FactoMineR::PCA(X = all_data[complete.cases(all_data),sfs_blocks1[[2]]], scale.unit = T, graph = F)$ind$coord[,1],
    FactoMineR::PCA(X = all_data[complete.cases(all_data),sfs_blocks1[[3]]], scale.unit = T, graph = F)$ind$coord[,1],
    FactoMineR::PCA(X = all_data[complete.cases(all_data),sfs_blocks1[[4]]], scale.unit = T, graph = F)$ind$coord[,1]
  )
)

pairs(
  data.frame(
    plsdepot::nipals(Data = all_data[complete.cases(all_data),sfs_blocks1[[1]]], scaled = T)$scores[,1],
    plsdepot::nipals(Data = all_data[complete.cases(all_data),sfs_blocks1[[2]]], scaled = T)$scores[,1],
    plsdepot::nipals(Data = all_data[complete.cases(all_data),sfs_blocks1[[3]]], scaled = T)$scores[,1],
    plsdepot::nipals(Data = all_data[complete.cases(all_data),sfs_blocks1[[4]]], scaled = T)$scores[,1]
  )
)

pca_y_svd <- function(x, modos){
  
  n = dim(x)[1]
  x0 = scale(x)#*(sqrt(n)/sqrt(n-1))
  svd_o <- corpcor::fast.svd(x0)
  comp  <- svd_o$u[,1:modos,drop=F]%*%diag(svd_o$d[1:modos],length(svd_o$d[1:modos]),length(svd_o$d[1:modos])) 
  vect  <- svd_o$v[,1:modos]
  output <- list(comp,vect)
  return(output)
  
}
pairs(
  data.frame(
    pca_y_svd(x = all_data[complete.cases(all_data),sfs_blocks1[[1]]], modos = 2)[[1]][,1],
    pca_y_svd(x = all_data[complete.cases(all_data),sfs_blocks1[[2]]], modos = 2)[[1]][,1],
    pca_y_svd(x = all_data[complete.cases(all_data),sfs_blocks1[[3]]], modos = 2)[[1]][,1],
    pca_y_svd(x = all_data[complete.cases(all_data),sfs_blocks1[[4]]], modos = 2)[[1]][,1]
  )
)

pairs(
  data.frame(
    prcomp(x = all_data[complete.cases(all_data),sfs_blocks1[[1]]], center = T, scale. = T)$x[,1],
    prcomp(x = all_data[complete.cases(all_data),sfs_blocks1[[2]]], center = T, scale. = T)$x[,1],
    prcomp(x = all_data[complete.cases(all_data),sfs_blocks1[[3]]], center = T, scale. = T)$x[,1],
    prcomp(x = all_data[complete.cases(all_data),sfs_blocks1[[4]]], center = T, scale. = T)$x[,1]
  )
)

# ----------------------------------------------------- #
# Hybrid approach
# ----------------------------------------------------- #
