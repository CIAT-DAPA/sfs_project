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

all_data <- dplyr::left_join(x = country_codes %>% dplyr::select(iso3c), y = environmentDim, by = "iso3c")
all_data <- dplyr::left_join(x = all_data, y = economicDim, by = "iso3c")
all_data <- dplyr::left_join(x = all_data, y = socialDim, by = "iso3c")
all_data <- dplyr::left_join(x = all_data, y = food_nutritionDim, by = "iso3c")

all_data <- all_data[-which(apply(X = all_data[,2:ncol(all_data)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 23),]
all_data <- all_data[-which(is.na(all_data$iso3c)),]
rownames(all_data) <- 1:nrow(all_data)

tabplot::tableplot(all_data[,-1], nBins = nrow(all_data))

all_data[complete.cases(all_data),] %>% View

## ========================================================================== ##
## Exploring missing data imputation techniques
## ========================================================================== ##

# Just for Food and nutrition dimension
nbdim <- estim_ncpPCA(food_nutritionDim[,-1])
res.comp <- MIPCA(food_nutritionDim[,-1], ncp = nbdim$ncp, nboot = 1000)
mdi_food_nutrition <- list(nDim = nbdim,
                           imputedData = res.comp)
# plot(res.comp)
saveRDS(object = mdi_environment, file = "./Results/_imputation/_mdi_food_nutrition.RDS")

# Complete data
nbdim <- estim_ncpPCA(all_data[,-1])
res.comp <- MIPCA(all_data[,-1], ncp = nbdim$ncp, nboot = 1000)
mdi_food_nutrition <- list(nDim = nbdim,
                           imputedData = res.comp)
# plot(res.comp)
saveRDS(object = mdi_environment, file = "./Results/_imputation/_mdi_food_nutrition.RDS")

## ========================================================================== ##
## Fitting PLS-PM with missing data
## ========================================================================== ##

# Inner model
sfs_path <- rbind(c(0, 0, 0, 0),
                  c(0, 0 ,0 ,0),
                  c(0, 0, 0, 0),
                  c(1, 1, 1, 0))
rownames(sfs_path) <- colnames(sfs_path) <- c("Environment", "Economic", "Social", "Food_nutrition")
innerplot(sfs_path)

# Blocks of variables
sfs_blocks <- list(2:8, 9:10, 11:12, 13:24)

# Scaling
sfs_scaling <- list(rep("NUM", length(sfs_blocks[[1]])),
                    rep("NUM", length(sfs_blocks[[2]])),
                    rep("NUM", length(sfs_blocks[[3]])),
                    rep("NUM", length(sfs_blocks[[4]])))

# Modes
sfs_modes <- c("A", "A", "A", "A")

# PLS-PM
sfs_pls1 <- plspm(all_data[complete.cases(all_data),], sfs_path, sfs_blocks, scaling = sfs_scaling, 
                  modes = sfs_modes, scheme = "centroid", plscomp = c(1,1,1,1), tol = 0.0000001) # plscomp = c(1,1,1,1), tol = 0.0000001
plot(sfs_pls1)
pairs(sfs_pls1$scores)





