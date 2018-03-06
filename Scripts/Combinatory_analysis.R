# Determine the optimal combination between number of indicators and number of countries: SFS project
# Implemented by: H. Achicanoy & P. Alvarez
# CIAT, 2018

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
suppressMessages(if(!require(plspm)){install.packages('plspm'); library(plspm)} else {library(plspm)})
suppressMessages(if(!require(caret)){install.packages('caret'); library(caret)} else {library(caret)})
suppressMessages(if(!require(missMDA)){install.packages('missMDA'); library(missMDA)} else {library(missMDA)})
suppressMessages(if(!require(missForest)){install.packages('missForest'); library(missForest)} else {library(missForest)})
suppressMessages(if(!require(treemap)){install.packages('treemap'); library(treemap)} else {library(treemap)})
suppressMessages(if(!require(viridisLite)){install.packages('viridisLite'); library(viridisLite)} else {library(viridisLite)})
suppressMessages(if(!require(highcharter)){install.packages('highcharter'); library(highcharter)} else {library(highcharter)})
suppressMessages(if(!require(corrplot)){install.packages('corrplot'); library(corrplot)} else {library(corrplot)})
suppressMessages(if(!require(cluster)){install.packages('cluster'); library(cluster)} else {library(cluster)})
suppressMessages(if(!require(factoextra)){install.packages('factoextra'); library(factoextra)} else {library(factoextra)})
suppressMessages(if(!require(gghighlight)){install.packages('gghighlight'); library(gghighlight)} else {library(gghighlight)})
library(EnvStats)
suppressMessages(library(compiler))

## ========================================================================== ##
## Define countries to work with
## ========================================================================== ##

# Worldwide shapefile
countries <- rgdal::readOGR(dsn = "./Input_data/world_shape", "all_countries")
countries$COUNTRY <- iconv(countries$COUNTRY, from = "UTF-8", to = "latin1")

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
## Combinatory analysis to determine best possible combination of indicators
## ========================================================================== ##

if(!file.exists("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/Results/modelling_results/auxTxt.txt")){
  
  ## ========================================================================== ##
  ## Determine country counts by dimension
  ## ========================================================================== ##
  
  tradeoff <- function(){
    # Environment
    envPos <- 2:8
    envCountries <- lapply(1:length(envPos), function(i){
      combinations <- combn(x = names(all_data)[envPos], m = i)
      combCounts <- unlist(lapply(1:ncol(combinations), function(j){
        nCountries <- sum(complete.cases(all_data[,combinations[,j]]))
        return(nCountries)
      }))
    })
    names(envCountries) <- 1:length(envPos)
    envCountries2 <- data.frame(Indicators = rep(names(envCountries), lapply(envCountries, length)),
                                Countries = unlist(envCountries),
                                Dimension = "Environment")
    rm(envPos, envCountries)
    
    # Economic
    ecoPos <- 9:11
    ecoCountries <- lapply(1:length(ecoPos), function(i){
      combinations <- combn(x = names(all_data)[ecoPos], m = i)
      combCounts <- unlist(lapply(1:ncol(combinations), function(j){
        nCountries <- sum(complete.cases(all_data[,combinations[,j]]))
        return(nCountries)
      }))
    })
    names(ecoCountries) <- 1:length(ecoPos)
    ecoCountries2 <- data.frame(Indicators = rep(names(ecoCountries), lapply(ecoCountries, length)),
                                Countries = unlist(ecoCountries),
                                Dimension = "Economic")
    rm(ecoPos, ecoCountries)
    
    # Social
    socPos <- 12:14
    socCountries <- lapply(1:length(socPos), function(i){
      combinations <- combn(x = names(all_data)[socPos], m = i)
      combCounts <- unlist(lapply(1:ncol(combinations), function(j){
        nCountries <- sum(complete.cases(all_data[,combinations[,j]]))
        return(nCountries)
      }))
    })
    names(socCountries) <- 1:length(socPos)
    socCountries2 <- data.frame(Indicators = rep(names(socCountries), lapply(socCountries, length)),
                                Countries = unlist(socCountries),
                                Dimension = "Social")
    rm(socPos, socCountries)
    
    # Food and nutrition
    fntPos <- 15:28
    fntCountries <- lapply(1:length(fntPos), function(i){
      combinations <- combn(x = names(all_data)[fntPos], m = i)
      combCounts <- unlist(lapply(1:ncol(combinations), function(j){
        nCountries <- sum(complete.cases(all_data[,combinations[,j]]))
        return(nCountries)
      }))
    })
    names(fntCountries) <- 1:length(fntPos)
    fntCountries2 <- data.frame(Indicators = rep(names(fntCountries), lapply(fntCountries, length)),
                                Countries = unlist(fntCountries),
                                Dimension = "Food and nutrition")
    rm(fntPos, fntCountries)
    
    
    # All combinations
    all_combinations <- rbind(envCountries2, ecoCountries2, socCountries2, fntCountries2)
    all_combinations$Indicators <- all_combinations$Indicators %>% as.character %>% as.numeric
    all_combinations %>% ggplot(aes(x = Indicators, y = Countries)) +
      xlab("Number of indicators") +
      ylab("Countries with complete data") +
      geom_point() + facet_wrap(~Dimension) +
      scale_x_continuous(breaks = 1:14) +
      theme_bw()
    if(!file.exists("./Results/graphs/indicators_vs_countries_per_dimension.png")){
      ggsave(filename = "./Results/graphs/indicators_vs_countries_per_dimension.png", width = 8, height = 8, units = "in", dpi = 300)
    }
    rm(envCountries, envCountries2, ecoCountries, ecoCountries2, socCountries, socCountries2, fntCountries, fntCountries2)
    
    cat("Done\n")
    
    return(all_combinations)
  }
  all_combinations <- tradeoff()
  all_combinations2 <- all_combinations %>% group_by(Indicators, Dimension) %>% summarise(MaxCount = max(Countries))
  all_combinations2 %>% ggplot(aes(x = Indicators, y = MaxCount)) +
    xlab("Number of indicators") +
    ylab("Countries with complete data") +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE) +
    geom_ribbon(aes(ymin = 100, ymax = 200, alpha = 0.2)) +
    facet_wrap(~Dimension) +
    scale_x_continuous(breaks = 1:14) +
    theme_bw() +
    guides(alpha = FALSE)
  if(!file.exists("./Results/graphs/indicators_vs_countries_per_dimension_max.png")){
    ggsave(filename = "./Results/graphs/indicators_vs_countries_per_dimension_max.png", width = 8, height = 8, units = "in", dpi = 300)
  }
  
  # With everything: don't try to run this stuff in your PC
  # allPos <- 2:28
  # allCountries <- lapply(1:length(allPos), function(i){
  #   combinations <- combn(x = names(all_data)[allPos], m = i)
  #   combCounts <- unlist(lapply(1:ncol(combinations), function(j){
  #     nCountries <- sum(complete.cases(all_data[,combinations[,j]]))
  #     return(nCountries)
  #   }))
  # })
  # names(allCountries) <- 1:length(fntPos)
  # allCountries2 <- data.frame(Indicators = rep(names(allCountries), lapply(allCountries, length)),
  #                             Countries = unlist(allCountries),
  #                             Dimension = "All")
  # library(tidyverse)
  # allCountries2 %>% ggplot(aes(x = Indicators, y = Countries)) + geom_point()
  
  ## ========================================================================== ##
  ## Determine the best combination
  ## ========================================================================== ##
  
  # Environment
  envPos <- 2:8
  envCountries <- lapply(1:length(envPos), function(i){
    combinations <- combn(x = names(all_data)[envPos], m = i) # Combination of indicators
    # For each combination of indicators calculate the number of countries with complete data
    combCounts <- lapply(1:ncol(combinations), function(j){
      lComb <- list(Countries = sum(complete.cases(all_data[,combinations[,j]])),
                    Indicators = combinations[,j])
      return(lComb)
    })
    return(combCounts)
  })
  names(envCountries) <- 1:length(envPos)
  env_mxCount <- purrr::modify_depth(envCountries, 2, "Countries") %>% purrr::map(., which.max)
  env_mxIndc <- purrr::modify_depth(envCountries, 2, "Indicators")
  env_mxIndc <- purrr::map2(env_mxIndc, env_mxCount, function(x, y) x[y]); rm(envPos, envCountries, env_mxCount)
  
  # Economic
  ecoPos <- 9:11
  ecoCountries <- lapply(1:length(ecoPos), function(i){
    combinations <- combn(x = names(all_data)[ecoPos], m = i) # Combination of indicators
    # For each combination of indicators calculate the number of countries with complete data
    combCounts <- lapply(1:ncol(combinations), function(j){
      lComb <- list(Countries = sum(complete.cases(all_data[,combinations[,j]])),
                    Indicators = combinations[,j])
      return(lComb)
    })
    return(combCounts)
  })
  names(ecoCountries) <- 1:length(ecoPos)
  eco_mxCount <- purrr::modify_depth(ecoCountries, 2, "Countries") %>% purrr::map(., which.max)
  eco_mxIndc <- purrr::modify_depth(ecoCountries, 2, "Indicators")
  eco_mxIndc <- purrr::map2(eco_mxIndc, eco_mxCount, function(x, y) x[y]); rm(ecoPos, ecoCountries, eco_mxCount)
  
  # Social
  socPos <- 12:14
  socCountries <- lapply(1:length(socPos), function(i){
    combinations <- combn(x = names(all_data)[socPos], m = i) # Combination of indicators
    # For each combination of indicators calculate the number of countries with complete data
    combCounts <- lapply(1:ncol(combinations), function(j){
      lComb <- list(Countries = sum(complete.cases(all_data[,combinations[,j]])),
                    Indicators = combinations[,j])
      return(lComb)
    })
    return(combCounts)
  })
  names(socCountries) <- 1:length(socPos)
  soc_mxCount <- purrr::modify_depth(socCountries, 2, "Countries") %>% purrr::map(., which.max)
  soc_mxIndc <- purrr::modify_depth(socCountries, 2, "Indicators")
  soc_mxIndc <- purrr::map2(soc_mxIndc, soc_mxCount, function(x, y) x[y]); rm(socPos, socCountries, soc_mxCount)
  
  # Food and nutrition
  fntPos <- 15:28
  fntCountries <- lapply(1:length(fntPos), function(i){
    combinations <- combn(x = names(all_data)[fntPos], m = i) # Combination of indicators
    # For each combination of indicators calculate the number of countries with complete data
    combCounts <- lapply(1:ncol(combinations), function(j){
      lComb <- list(Countries = sum(complete.cases(all_data[,combinations[,j]])),
                    Indicators = combinations[,j])
      return(lComb)
    })
    return(combCounts)
  })
  names(fntCountries) <- 1:length(fntPos)
  fnt_mxCount <- purrr::modify_depth(fntCountries, 2, "Countries") %>% purrr::map(., which.max)
  fnt_mxIndc <- purrr::modify_depth(fntCountries, 2, "Indicators")
  fnt_mxIndc <- purrr::map2(fnt_mxIndc, fnt_mxCount, function(x, y) x[y]); rm(fntPos, fntCountries, fnt_mxCount)
  
  # Pre-process indicators names
  env_mxIndc <- env_mxIndc %>% purrr::map(., function(x){paste(x[[1]], collapse = "_")})
  eco_mxIndc <- eco_mxIndc %>% purrr::map(., function(x){paste(x[[1]], collapse = "_")})
  soc_mxIndc <- soc_mxIndc %>% purrr::map(., function(x){paste(x[[1]], collapse = "_")})
  fnt_mxIndc <- fnt_mxIndc %>% purrr::map(., function(x){paste(x[[1]], collapse = "_")})
  
  a <- list(unlist(env_mxIndc),
            unlist(eco_mxIndc),
            unlist(soc_mxIndc),
            unlist(fnt_mxIndc))
  a <- paste0('[',
              '[', paste('"', unlist(env_mxIndc), '"', sep = '', collapse = ','),']', ',',
              '[', paste('"', unlist(eco_mxIndc), '"', sep = '', collapse = ','),']', ',',
              '[', paste('"', unlist(soc_mxIndc), '"', sep = '', collapse = ','),']', ',',
              '[', paste('"', unlist(fnt_mxIndc), '"', sep = '', collapse = ','),']',
              ']')
  
  if(!file.exists("D:/ToBackup/repositories/cc-repo/sfs_project/Scripts/auxTxt.txt")){
    # Apply python function to determine all possible combinations
    setwd("~")
    createCode <- function(code){
      
      sink(code)
      cat(paste0('a=', a), fill = T)
      cat('r=[[]]', fill = T)
      cat('for x in a:', fill = T)
      cat('\t t = []', fill = T)
      cat('\t for y in x:', fill = T)
      cat('\t \t for i in r:', fill = T)
      cat('\t \t \t t.append(i+[y])', fill = T)
      cat('\t r = t', fill = T)
      # cat('f = open("C://Users/haachicanoy/Documents/testfile.txt", "w")', fill = T)
      cat('f = open("D:/ToBackup/repositories/cc-repo/sfs_project/Scripts/auxTxt.txt", "w")', fill = T)
      cat('z = str(r)', fill = T)
      cat('f.write(z)', fill = T)
      cat('f.close()', fill = T)
      sink()
      shell(code)# system2(paste0('python ', code));# shell.exec(code)
      
    }
    createCode(code = 'D:/ToBackup/repositories/cc-repo/sfs_project/Scripts/Do_mixed_combinations.py')
  }
  
  if(!file.exists("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/Results/modelling_results/auxTxt.txt")){
    file.copy(from = "D:/ToBackup/repositories/cc-repo/sfs_project/Scripts/auxTxt.txt",
              to = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/Results/modelling_results/auxTxt.txt",
              overwrite = F)
  }
  
  # Post-process indicators names
  textFile <- readLines("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/Results/modelling_results/auxTxt.txt")
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
  
  finalCombinations <- lapply(1:length(textFile), function(i){
    df <- data.frame(
      Countries = sum(complete.cases(all_data[,textFile[[i]]])),
      Indicators = length(textFile[[i]])
    )
    return(df)
  })
  finalCombinations <- do.call(rbind, finalCombinations)
  finalCombinations %>% ggplot(aes(x = Indicators, y = Countries)) + geom_point() +
    xlab("Number of indicators") +
    ylab("Countries with complete data") +
    scale_x_continuous(breaks = 1:27) +
    theme_bw()
  OSys <- Sys.info()[1]
  OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
  wk_dir   <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9/Sustainable_Food_System/SFS_indicators", "Windows" = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators")
  setwd(wk_dir); rm(wk_dir, OSysPath, OSys)
  if(!file.exists("./Results/graphs/indicators_vs_countriesFinal.png")){
    ggsave(filename = "./Results/graphs/indicators_vs_countriesFinal.png", width = 8, height = 8, units = "in", dpi = 300)
  }
  
  finalCombinations2 <- finalCombinations %>% group_by(Indicators) %>% summarise(MaxCount = max(Countries))
  finalCombinations2 %>% ggplot(aes(x = Indicators, y = MaxCount)) + geom_point() +
    xlab("Number of indicators") +
    ylab("Countries with complete data") +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE) +
    geom_ribbon(aes(ymin = 100, ymax = 200, alpha = 0.2)) +
    scale_x_continuous(breaks = 4:27) +
    guides(alpha = FALSE) +
    theme_bw()
  if(!file.exists("./Results/graphs/indicators_vs_countriesFinal_max.png")){
    ggsave(filename = "./Results/graphs/indicators_vs_countriesFinal_max.png", width = 8, height = 8, units = "in", dpi = 300)
  }
  
} else {
  # Post-process indicators names
  textFile <- readLines("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/Results/modelling_results/auxTxt.txt")
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
}

## ========================================================================== ##
## Fitting several models (and measuring their performance)
## ========================================================================== ##

fittingModels <- function(data = all_data, combList, Mode = "A"){
  
  # Updating dimension indexes
  envPos <- 2:8; ecoPos <- 9:11; socPos <- 12:14; fntPos <- 15:28
  mtch <- match(combList, names(data))
  envUpt <- base::intersect(envPos, mtch)
  ecoUpt <- base::intersect(ecoPos, mtch)
  socUpt <- base::intersect(socPos, mtch)
  fntUpt <- base::intersect(fntPos, mtch)
  
  # Updating data set
  data <- data[which(complete.cases(data[, mtch])),]
  
  # Fitting model
  # Repeated indicators approach
  
  # Inner model
  sfs_path <- rbind(c(0, 0, 0, 0, 0),
                    c(0, 0 ,0 ,0, 0),
                    c(0, 0, 0, 0, 0),
                    c(0, 0, 0, 0, 0),
                    c(1, 1, 1, 1, 0))
  rownames(sfs_path) <- colnames(sfs_path) <- c("Environment", "Economic", "Social", "Food_nutrition", "SFS_index")
  # innerplot(sfs_path)
  
  # Blocks of variables: repeated indicators approach
  sfs_blocks1 <- list(envUpt, ecoUpt, socUpt, fntUpt, c(envUpt, ecoUpt, socUpt, fntUpt))
  
  # Scaling
  sfs_scaling <- list(rep("NUM", length(sfs_blocks1[[1]])),
                      rep("NUM", length(sfs_blocks1[[2]])),
                      rep("NUM", length(sfs_blocks1[[3]])),
                      rep("NUM", length(sfs_blocks1[[4]])),
                      rep("NUM", length(sfs_blocks1[[5]])))
  
  # Modes
  sfs_modes <- rep(Mode, 5)# c("B", "B", "B", "B", "B")
  
  # Run PLS-PM
  tryCatch(expr = {
    set.seed(1235)
    sfs_pls1 <- plspm::plspm(data, sfs_path, sfs_blocks1, scaling = sfs_scaling, 
                             modes = sfs_modes, scheme = "path", tol = 0.00000001, scaled = TRUE, maxiter = 500)
  },
  error = function(e){
    cat("Modeling process failed for present combination\n")
    return("Done\n")
  })
  
  if(exists('sfs_pls1')){
    # Saving outputs
    df <- data_frame(
      GoF = sfs_pls1$gof,
      nCountries = nrow(data),
      nIndicators = length(mtch),
      nEnv = length(envUpt),
      nEco = length(ecoUpt),
      nSoc = length(socUpt),
      nFnt = length(fntUpt)
    )
    
    results <- list(performance = df,
                    model = sfs_pls1)
  } else {
    df <- data_frame(
      GoF = NA,
      nCountries = nrow(data),
      nIndicators = length(mtch),
      nEnv = length(envUpt),
      nEco = length(ecoUpt),
      nSoc = length(socUpt),
      nFnt = length(fntUpt)
    )
    
    results <- list(performance = df,
                    model = "Failed model")
  }
  
  return(results)
  
}
results <- lapply(X = textFile, FUN = function(x) fittingModels(data = all_data, combList = x, Mode = "B"))
dfs <- lapply(results, function(x) x[[1]])
models <- lapply(results, function(x) x[[2]])
dfs <- do.call(rbind, dfs)
dfs <- as_data_frame(dfs)
dfs <- dfs %>% dplyr::mutate(model = purrr::map(models, function(x) x))
rm(results, models)

dfs <- dfs %>% dplyr::mutate(coefs = purrr::map(.$model, .f = function(x){paste0(x$path_coefs[5,], collapse = "_")}))
dfs <- dfs %>% tidyr::separate(data = ., col = coefs, sep = "_", into = c("Environment_coef", "Economic_coef", "Social_coef", "Food_nutrition_coef", "SFS_index"))
dfs$Environment_coef <- dfs$Environment_coef %>% as.numeric()
dfs$Economic_coef <- dfs$Economic_coef %>% as.numeric()
dfs$Social_coef <- dfs$Social_coef %>% as.numeric()
dfs$Food_nutrition_coef <- dfs$Food_nutrition_coef %>% as.numeric()
dfs$SFS_index <- NULL

saveRDS(object = dfs, file = "./Results/modelling_results/metrics.RDS")

dfs <- readRDS("./Results/modelling_results/metrics_modeA_maxIter500.RDS")
dfs$Combination <- 1:nrow(dfs)
nInd <- dfs$nIndicators %>% unique %>% sort
combID <- rep(NA, length(nInd))
for(i in 1:length(nInd)){
  df <- dfs %>% select(nCountries, nIndicators, Combination)
  df <- df %>% filter(nIndicators == nInd[i])
  df <- df[which.max(df$nCountries),]
  combID[i] <- df$Combination
}; rm(i, df)
textFile2 <- textFile[combID]




dfs2 <- dfs
dfs2 <- dfs2 %>% group_by(nIndicators) %>% summarise(MaxCount = max(nCountries))
dfs2 %>% ggplot(aes(x = nIndicators, y = MaxCount)) + geom_point() +
  scale_x_continuous(breaks = 4:27) +
  xlab("Number of indicators") +
  ylab("Countries with complete data") +
  theme_classic() +
  theme(text = element_text(size = 25))
ggsave(filename = "./Results/graphs/Indicators_vs_Countries.png", width = 10, height = 8, units = "in", dpi = 300)

# GoF vs Countries
dfs %>% ggplot(aes(x = nCountries, y = GoF)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE) + theme_bw()
if(!file.exists("./Results/graphs/GoF_vs_nCountries.png")){
  ggsave(filename = "./Results/graphs/GoF_vs_nCountries.png", width = 8, height = 8, units = "in", dpi = 300)
}
# GoF vs Indicators
dfs %>% ggplot(aes(x = nIndicators, y = GoF)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE) +
  scale_x_continuous(breaks = 4:27) +
  theme_bw()
if(!file.exists("./Results/graphs/GoF_vs_nIndicators.png")){
  ggsave(filename = "./Results/graphs/GoF_vs_nIndicators.png", width = 8, height = 8, units = "in", dpi = 300)
}

dfs %>% ggplot(aes(x = nEnv, y = Environment_coef)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE) +
  geom_hline(yintercept = 0, colour = "red", size = 1.5) +
  scale_x_continuous(breaks = 1:7) +
  theme_bw()
if(!file.exists("./Results/graphs/EnvCoeff_vs_nEnv.png")){
  ggsave(filename = "./Results/graphs/EnvCoeff_vs_nEnv.png", width = 8, height = 8, units = "in", dpi = 300)
}

dfs %>% ggplot(aes(x = nEco, y = Economic_coef)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE) +
  geom_hline(yintercept = 0, colour = "red", size = 1.5) +
  scale_x_continuous(breaks = 1:3) +
  theme_bw()
if(!file.exists("./Results/graphs/EcoCoeff_vs_nEco.png")){
  ggsave(filename = "./Results/graphs/EcoCoeff_vs_nEco.png", width = 8, height = 8, units = "in", dpi = 300)
}

dfs %>% ggplot(aes(x = nSoc, y = Social_coef)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE) +
  geom_hline(yintercept = 0, colour = "red", size = 1.5) +
  scale_x_continuous(breaks = 1:3) +
  theme_bw()
if(!file.exists("./Results/graphs/SocCoeff_vs_nSoc.png")){
  ggsave(filename = "./Results/graphs/SocCoeff_vs_nSoc.png", width = 8, height = 8, units = "in", dpi = 300)
}

dfs %>% ggplot(aes(x = nFnt, y = Food_nutrition_coef)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE) +
  geom_hline(yintercept = 0, colour = "red", size = 1.5) +
  scale_x_continuous(breaks = 1:14) +
  theme_bw()
if(!file.exists("./Results/graphs/FoodCoeff_vs_nFnt.png")){
  ggsave(filename = "./Results/graphs/FoodCoeff_vs_nFnt.png", width = 8, height = 8, units = "in", dpi = 300)
}

dfs %>% gghighlight_point(aes(x = nIndicators, y = nCountries, colour = GoF), GoF > .6) + theme_bw()
if(!file.exists("./Results/graphs/bestCombination.60.png")){
  ggsave(filename = "./Results/graphs/bestCombination.60.png", width = 8, height = 8, units = "in", dpi = 300)
}

# stdy_case <- dfs %>% filter(GoF > .7 & nCountries == 79 & nIndicators == 15)
# stdy_case <- dfs %>% filter(nCountries == 164 & nIndicators == 4)
stdy_case <- dfs %>% filter(nCountries == 95 & nIndicators == 20)
myScores <- stdy_case$model[[1]]$scores
myScores <- as.data.frame(myScores)
myScores$Country <- rownames(myScores)
myScores <- dplyr::left_join(x = myScores, y = country_codes %>% dplyr::select(country.name.en, iso3c), by = c("Country" = "country.name.en"))
rownames(myScores) <- myScores$iso3c
myScores$Country <- NULL
myScores$iso3c <- NULL

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
#############################################################################

sfsMap(Scores = myScores)


dfs %>% ggplot(aes(x = Environment_coef, y = Food_nutrition_coef)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE)


ctrl <- caret::trainControl(method = "LGOCV", p = 0.8, number = 10, savePredictions = T)
ctrl <- caret::trainControl(method = "repeatedcv", p = 0.8, number = 10, savePredictions = T)

grd <- expand.grid(mtry = seq(1, 5, 1))
rf.res <- caret::train(GoF ~ .,
                       data = dfs %>% dplyr::select(GoF, nCountries, nIndicators, nEnv, nSoc, nFnt),
                       method = 'rf',
                       tuneGrid = grd,
                       importance = TRUE,
                       ntree = 2000,
                       metric = 'logLoss',
                       trControl = ctrl)
plot(rf.res)
plot(rf.res$finalModel)
plot(varImp(rf.res), top = 10)

rsm.res <- rsm::rsm(GoF ~ FO(nCountries, nIndicators), data = dfs)
summary(rsm.res)


dfs2 <- dfs %>% filter(is.na(GoF))
dfs <- dfs %>% filter(!is.na(GoF))

# ------------------------------------------------------------------------------------------------------------ #
# Use some alternative ways to construct indices
# ------------------------------------------------------------------------------------------------------------ #

calculateIndices <- function(data = all_data, combList = textFile2[[17]]){
  
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
  HDI_approach <- function(data = data, varInd = mtch){
    
    rNames <- data$iso3c
    
    # Step 1. Normalization function for all indicators
    normalization <- function(x){
      y = (x - min(x))/(max(x) - min(x))
      return(y)
    }
    
    # Step 2. Normalize indicadors and apply a correction for those indicators which have negative polarity
    for(m in mtch){
      data[,m] <- normalization(x = data[,m])
      if(signs[m] < 0){data[,m] <- 1 - data[,m]}
    }; rm(m)
    
    # Step 3. Calculate an index for each dimension
    if(length(envUpt) > 1){envAve <- rowMeans(data[,envUpt])} else {envAve <- data[,envUpt]}
    if(length(ecoUpt) > 1){ecoAve <- rowMeans(data[,ecoUpt])} else {ecoAve <- data[,ecoUpt]}
    if(length(socUpt) > 1){socAve <- rowMeans(data[,socUpt])} else {socAve <- data[,socUpt]}
    if(length(fntUpt) > 1){fntAve <- rowMeans(data[,fntUpt])} else {fntAve <- data[,fntUpt]}
    indices <- data.frame(Environment = envAve, Economic = ecoAve, Social = socAve, Food_nutrition = fntAve)
    
    # Step 4. Calculate a final composite index
    indices$SFS_index <- indices %>% select(Environment:Food_nutrition) %>% apply(X = ., MARGIN = 1, EnvStats::geoMean)
    rownames(indices) <- rNames
    
    return(indices)
  }
  
  # HPI approach
  HPI_approach <- function(data = data, varInd = mtch){
    
    rNames <- data$iso3c
    
    # Step 1. Normalization function for all indicators
    normalization <- function(x){
      y = (x - min(x))/(max(x) - min(x))
      return(y)
    }
    
    # Step 2. Normalize indicators and apply a correction for those indicators which have negative polarity
    for(m in mtch){
      data[,m] <- normalization(x = data[,m])
      if(signs[m] < 0){data[,m] <- 1 - data[,m]}
    }; rm(m)
    
    # Step 3. Calculate an index for each dimension
    if(length(envUpt) > 1){envAve <- rowMeans(data[,envUpt])} else {envAve <- data[,envUpt]}
    if(length(ecoUpt) > 1){ecoAve <- rowMeans(data[,ecoUpt])} else {ecoAve <- data[,ecoUpt]}
    if(length(socUpt) > 1){socAve <- rowMeans(data[,socUpt])} else {socAve <- data[,socUpt]}
    if(length(fntUpt) > 1){fntAve <- rowMeans(data[,fntUpt])} else {fntAve <- data[,fntUpt]}
    indices <- data.frame(Environment = envAve, Economic = ecoAve, Social = socAve, Food_nutrition = fntAve)
    
    # Step 4. Calculate a final composite index
    indices$SFS_index <- indices %>% select(Environment:Food_nutrition) %>% apply(X = ., MARGIN = 1, FUN = function(x){mean(x^4)^(1/4)})
    rownames(indices) <- rNames
    
    return(indices)
  }
  
  # MPI approach
  MPI_approach <- function(data = data, varInd = mtch){
    
    rNames <- data$iso3c
    
    # Step 1. Normalization function for all indicators
    normalizationMPI <- function(x, sgn){
      y = 100 + sgn * ((x - mean(x))/sd(x)) * 10
      return(y)
    }
    for(m in mtch){
      data[,m] <- normalizationMPI(x = data[,m], sgn = signs[m])
    }; rm(m)
    # Step 2. Aggregation by dimension
    if(length(envUpt) > 1){
      M <- rowMeans(data[,envUpt])
      S <- apply(X = data[,envUpt], MARGIN = 1, FUN = sd)
      CV <- S/M
      envAve <- M - (S*CV)
    } else {
      envAve <- data[,envUpt]
    }
    if(length(ecoUpt) > 1){
      M <- rowMeans(data[,ecoUpt])
      S <- apply(X = data[,ecoUpt], MARGIN = 1, FUN = sd)
      CV <- S/M
      ecoAve <- M - (S*CV)
    } else {
      ecoAve <- data[,ecoUpt]
    }
    if(length(socUpt) > 1){
      M <- rowMeans(data[,socUpt])
      S <- apply(X = data[,socUpt], MARGIN = 1, FUN = sd)
      CV <- S/M
      socAve <- M - (S*CV)
    } else {
      socAve <- data[,socUpt]
    }
    if(length(fntUpt) > 1){
      M <- rowMeans(data[,fntUpt])
      S <- apply(X = data[,fntUpt], MARGIN = 1, FUN = sd)
      CV <- S/M
      fntAve <- M - (S*CV)
    } else {
      fntAve <- data[,fntUpt]
    }
    indices <- data.frame(Environment = envAve, Economic = ecoAve, Social = socAve, Food_nutrition = fntAve)
    # Step 4. Calculate a final composite index
    # indices$SFS_index <- indices %>% select(Environment:Food_nutrition) %>% rowMeans()
    indices$SFS_index <- indices %>% select(Environment:Food_nutrition) %>% apply(X = ., MARGIN = 1, FUN = psych::geometric.mean)
    rownames(indices) <- rNames
    
    return(indices)
  }
  
  HDI_approach(data = data, varInd = mtch)
  HPI_approach(data = data, varInd = mtch)
  MPI_approach(data = data, varInd = mtch)
  
  # HDI_results <- HDI_approach(data = data, varInd = mtch)
  # results <- tibble(Country = rownames(data), HDI = HDI_results$SFS_index, HPI = HPI_results$SFS_index, MPI = (MPI_results$SFS_index-min(MPI_results$SFS_index))/(max(MPI_results$SFS_index)-min(MPI_results$SFS_index)))
  parcoords::parcoords(results, rownames = FALSE,
                       reorder = TRUE, brushMode="1D",
                       color = list(
                         colorScale = htmlwidgets::JS('d3.scale.category10()'),
                         colorBy = "sp"))
  
  ownJackknife <- function(x = data, FUN = HDI_approach, funName = "HDI_approach"){
    folds <- nrow(x)
    Function <- FUN
    jckk <- lapply(X = 1:folds, FUN = function(i){
      x <- x[-i,]
      df <- Function(data = x, varInd = mtch)
      df$Combination <- i
      df$Approach <- gsub(pattern = "_approach", replacement = "", x = funName)
      df$iso3c <- rownames(df)
      return(df)
    })
    return(jckk)
  }
  HDI_results <- ownJackknife(x = data, FUN = HDI_approach, funName = "HDI_approach")
  HDI_results <- do.call(rbind, HDI_results)
  rownames(HDI_results) <- 1:nrow(HDI_results)
  
  HDI_results %>% ggplot(aes(x = Combination, y = SFS_index, group = iso3c)) + geom_line()
  
  HPI_results <- ownJackknife(x = data, FUN = HPI_approach, funName = "HPI_approach")
  HPI_results <- do.call(rbind, HPI_results)
  rownames(HPI_results) <- 1:nrow(HPI_results)
  
  HPI_results %>% ggplot(aes(x = Combination, y = SFS_index, group = iso3c)) + geom_line()
  
  MPI_results <- ownJackknife(x = data, FUN = MPI_approach, funName = "MPI_approach")
  MPI_results <- do.call(rbind, MPI_results)
  rownames(MPI_results) <- 1:nrow(MPI_results)
  
  MPI_results %>% ggplot(aes(x = Combination, y = SFS_index, group = iso3c)) + geom_line()
  
  plot(HDI_results$SFS_index[HDI_results$Combination == "1"], HPI_results$SFS_index[HPI_results$Combination == "1"], pch = 20,
       xlab = "HDI approach", ylab = "HPI approach")
  abline(0 ,1)
  plot(HDI_results$SFS_index[HDI_results$Combination == "1"], MPI_results$SFS_index[MPI_results$Combination == "1"], pch = 20,
       xlab = "HDI approach", ylab = "MPI approach")
  abline(0 ,1)
  plot(HPI_results$SFS_index[HPI_results$Combination == "1"], MPI_results$SFS_index[MPI_results$Combination == "1"], pch = 20,
       xlab = "HPI approach", ylab = "MPI approach")
  abline(0 ,1)
  
  bootstrap::jackknife(x = data, theta = function(x) HDI_approach(data = x, varInd = mtch))
  
  # Fitting model
  # Repeated indicators approach
  
  # Inner model
  sfs_path <- rbind(c(0, 0, 0, 0, 0),
                    c(0, 0 ,0 ,0, 0),
                    c(0, 0, 0, 0, 0),
                    c(0, 0, 0, 0, 0),
                    c(1, 1, 1, 1, 0))
  rownames(sfs_path) <- colnames(sfs_path) <- c("Environment", "Economic", "Social", "Food_nutrition", "SFS_index")
  # innerplot(sfs_path)
  
  # Blocks of variables: repeated indicators approach
  sfs_blocks1 <- list(envUpt, ecoUpt, socUpt, fntUpt, c(envUpt, ecoUpt, socUpt, fntUpt))
  
  # Scaling
  sfs_scaling <- list(rep("NUM", length(sfs_blocks1[[1]])),
                      rep("NUM", length(sfs_blocks1[[2]])),
                      rep("NUM", length(sfs_blocks1[[3]])),
                      rep("NUM", length(sfs_blocks1[[4]])),
                      rep("NUM", length(sfs_blocks1[[5]])))
  
  # Modes
  sfs_modes <- rep(Mode, 5)# c("B", "B", "B", "B", "B")
  
  # Run PLS-PM
  tryCatch(expr = {
    set.seed(1235)
    sfs_pls1 <- plspm::plspm(data, sfs_path, sfs_blocks1, scaling = sfs_scaling, 
                             modes = sfs_modes, scheme = "path", tol = 0.00000001, scaled = TRUE, maxiter = 500)
  },
  error = function(e){
    cat("Modeling process failed for present combination\n")
    return("Done\n")
  })
  
  if(exists('sfs_pls1')){
    # Saving outputs
    df <- data_frame(
      GoF = sfs_pls1$gof,
      nCountries = nrow(data),
      nIndicators = length(mtch),
      nEnv = length(envUpt),
      nEco = length(ecoUpt),
      nSoc = length(socUpt),
      nFnt = length(fntUpt)
    )
    
    results <- list(performance = df,
                    model = sfs_pls1)
  } else {
    df <- data_frame(
      GoF = NA,
      nCountries = nrow(data),
      nIndicators = length(mtch),
      nEnv = length(envUpt),
      nEco = length(ecoUpt),
      nSoc = length(socUpt),
      nFnt = length(fntUpt)
    )
    
    results <- list(performance = df,
                    model = "Failed model")
  }
  
  return(results)
  
}
results <- lapply(X = textFile, FUN = function(x) fittingModels(data = all_data, combList = x, Mode = "B"))
