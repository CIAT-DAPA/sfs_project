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
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(raster, rgdal, maptools, jsonlite, foreach, doParallel, XML, plspm, reshape, tidyverse, countrycode, caret,
                                missMDA, missForest, treemap, viridisLite, highcharter, corrplot, cluster, factoextra, FactoMineR, gghighlight,
                                EnvStats,compiler))

## ========================================================================== ##
## Define countries to work with
## ========================================================================== ##

# Country code translation
country_codes <- read.csv("./country_codes_update_28_09_18.csv")

## ========================================================================== ##
## Load data by dimension
## ========================================================================== ##

if(!file.exists("./sfs_raw_indicators.csv")){
  
  if(file.exists("environmental_dimension.csv")){environmentDim <- read.csv("environmental_dimension.csv", row.names = 1)}
  if(file.exists("economic_dimension.csv")){economicDim <- read.csv("economic_dimension.csv", row.names = 1)}
  if(file.exists("social_dimension.csv")){socialDim <- read.csv("social_dimension.csv", row.names = 1)}
  if(file.exists("food_nutrition_dimension.csv")){food_nutritionDim <- read.csv("food_nutrition_dimension.csv", row.names = 1)}
  
  all_data <- dplyr::left_join(x = country_codes %>% dplyr::select(iso3c), y = environmentDim, by = "iso3c")
  all_data <- dplyr::left_join(x = all_data, y = economicDim, by = "iso3c")
  all_data <- dplyr::left_join(x = all_data, y = socialDim, by = "iso3c")
  all_data <- dplyr::left_join(x = all_data, y = food_nutritionDim, by = "iso3c")
  all_data <- all_data[-which(apply(X = all_data[,2:ncol(all_data)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 27),]
  
  all_data <- dplyr::right_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = all_data, by = "iso3c")
  rownames(all_data) <- all_data$country.name.en; all_data$country.name.en <- NULL
  rm(environmentDim, food_nutritionDim, socialDim, economicDim)
  write.csv(x = all_data, file = "./sfs_raw_indicators.csv", row.names = T)
  
} else {
  all_data <- read.csv("./sfs_raw_indicators.csv", row.names = 1)
}


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
      cat('f = open("D:/ToBackup/repositories/cc-repo/sfs_project/Scripts/auxTxt_checked2.txt", "w")', fill = T)
      cat('z = str(r)', fill = T)
      cat('f.write(z)', fill = T)
      cat('f.close()', fill = T)
      sink()
      shell(code)# system2(paste0('python ', code));# shell.exec(code)
      
    }
    createCode(code = 'D:/ToBackup/repositories/cc-repo/sfs_project/Scripts/Do_mixed_combinations_checked.py')
  }
  
  if(!file.exists("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/Results/modelling_results/auxTxt.txt")){
    file.copy(from = "D:/ToBackup/repositories/cc-repo/sfs_project/Scripts/auxTxt_checked2.txt",
              to = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/Results/modelling_results/auxTxt.txt",
              overwrite = T)
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
