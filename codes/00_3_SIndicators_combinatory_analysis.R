# Optimal combination: number of indicators vs countries: SFS project
# Implemented by: H. Achicanoy & P. Alvarez
# CIAT, 2019

# R options
g <- gc(reset = T); rm(list = ls()); options(scipen = 999, warn = -1)

# Load packages
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(raster, rgdal, maptools, jsonlite, foreach, doParallel, XML, plspm, reshape, tidyverse, countrycode, caret,
                                missMDA, missForest, treemap, viridisLite, highcharter, corrplot, cluster, factoextra, FactoMineR, gghighlight,
                                EnvStats, compiler, caretEnsemble))

# Define data directory
data_path <- "D:/ToBackup/sustainable_food_systems/sfs_repo/data"
# data_path <- "//dapadfs.cgiarad.org/workspace_cluster_9/Sustainable_Food_System/data"

# Load scale adjusted data
all_data <- read.csv(paste0(data_path,"/outputs/sfs_raw_indicators_scales_adjusted.csv"), row.names = 1)

# Function to dentify edge's frontier
edge_frontier <- function(df = all_data, envPos = 2:9, ecoPos = 10:12, socPos = 13:15, fntPos = 16:28){
  
  # Environmental dimension
  envCountries <- lapply(1:length(envPos), function(i){
    combinations <- combn(x = names(df)[envPos], m = i) # Combination of indicators
    # For each combination of indicators calculate the number of countries with complete data
    combCounts <- lapply(1:ncol(combinations), function(j){
      lComb <- list(Countries = sum(complete.cases(df[,combinations[,j]])),
                    Indicators = combinations[,j])
      return(lComb)
    })
    return(combCounts)
  })
  names(envCountries) <- 1:length(envPos)
  env_mxCount <- purrr::modify_depth(envCountries, 2, "Countries") %>% purrr::map(., which.max)
  env_mxIndc <- purrr::modify_depth(envCountries, 2, "Indicators")
  env_mxIndc <- purrr::map2(env_mxIndc, env_mxCount, function(x, y) x[y]); rm(envCountries, env_mxCount)
  
  # Economic dimension
  ecoCountries <- lapply(1:length(ecoPos), function(i){
    combinations <- combn(x = names(df)[ecoPos], m = i) # Combination of indicators
    # For each combination of indicators calculate the number of countries with complete data
    combCounts <- lapply(1:ncol(combinations), function(j){
      lComb <- list(Countries = sum(complete.cases(df[,combinations[,j]])),
                    Indicators = combinations[,j])
      return(lComb)
    })
    return(combCounts)
  })
  names(ecoCountries) <- 1:length(ecoPos)
  eco_mxCount <- purrr::modify_depth(ecoCountries, 2, "Countries") %>% purrr::map(., which.max)
  eco_mxIndc <- purrr::modify_depth(ecoCountries, 2, "Indicators")
  eco_mxIndc <- purrr::map2(eco_mxIndc, eco_mxCount, function(x, y) x[y]); rm(ecoCountries, eco_mxCount)
  
  # Social
  socCountries <- lapply(1:length(socPos), function(i){
    combinations <- combn(x = names(df)[socPos], m = i) # Combination of indicators
    # For each combination of indicators calculate the number of countries with complete data
    combCounts <- lapply(1:ncol(combinations), function(j){
      lComb <- list(Countries = sum(complete.cases(df[,combinations[,j]])),
                    Indicators = combinations[,j])
      return(lComb)
    })
    return(combCounts)
  })
  names(socCountries) <- 1:length(socPos)
  soc_mxCount <- purrr::modify_depth(socCountries, 2, "Countries") %>% purrr::map(., which.max)
  soc_mxIndc <- purrr::modify_depth(socCountries, 2, "Indicators")
  soc_mxIndc <- purrr::map2(soc_mxIndc, soc_mxCount, function(x, y) x[y]); rm(socCountries, soc_mxCount)
  
  # Food and nutrition
  fntCountries <- lapply(1:length(fntPos), function(i){
    combinations <- combn(x = names(df)[fntPos], m = i) # Combination of indicators
    # For each combination of indicators calculate the number of countries with complete data
    combCounts <- lapply(1:ncol(combinations), function(j){
      lComb <- list(Countries = sum(complete.cases(df[,combinations[,j]])),
                    Indicators = combinations[,j])
      return(lComb)
    })
    return(combCounts)
  })
  names(fntCountries) <- 1:length(fntPos)
  fnt_mxCount <- purrr::modify_depth(fntCountries, 2, "Countries") %>% purrr::map(., which.max)
  fnt_mxIndc <- purrr::modify_depth(fntCountries, 2, "Indicators")
  fnt_mxIndc <- purrr::map2(fnt_mxIndc, fnt_mxCount, function(x, y) x[y]); rm(fntCountries, fnt_mxCount)
  
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
  
  # Obtain all maximum possible combinations
  outfile <- paste0(data_path, "/outputs/edge_frontier/all_maximum_combinations.txt")
  if(!file.exists(outfile)){
    # Apply python function to determine all maximum possible combinations
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
      cat(paste0('f = open("',outfile,'", "w")'), fill = T)
      cat('z = str(r)', fill = T)
      cat('f.write(z)', fill = T)
      cat('f.close()', fill = T)
      sink()
      shell(code) # system2(paste0('python ', code));# shell.exec(code)
      
    }
    createCode(code = paste0(data_path, "/outputs/edge_frontier/obtain_maximum_combinations.py"))
    
    # Post-process indicators names
    textFile <- readLines(outfile)
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
    saveRDS(textFile, file = gsub("txt", "RDS", outfile))
    textFile <- readRDS(gsub("txt", "RDS", outfile))
    
  } else {
    
    if(!file.exists(gsub("txt", "RDS", outfile))){
      
      # Post-process indicators names
      textFile <- readLines(outfile)
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
      saveRDS(textFile, file = gsub("txt", "RDS", outfile))
      
    } else {
      textFile <- readRDS(gsub("txt", "RDS", outfile))
    }
    
  }
  
  # Return edge's frontier combinations
  if(!file.exists(paste0(data_path,"/outputs/edge_frontier/all_maximum_combinations_tibble.RDS"))){
    fnl_cmb <- lapply(1:length(textFile), function(i){
      mtch <- match(textFile[[i]], names(df))
      db <- data.frame(
        nCountries  = sum(complete.cases(all_data[,textFile[[i]]])),
        nIndicators = length(textFile[[i]]),
        nEnv        = length(base::intersect(envPos, mtch)),
        nEco        = length(base::intersect(ecoPos, mtch)),
        nSoc        = length(base::intersect(socPos, mtch)),
        nFnt        = length(base::intersect(fntPos, mtch))
      )
      return(db)
    })
    fnl_cmb <- do.call(rbind, fnl_cmb)
    fnl_cmb %>% dplyr::group_by(nIndicators) %>% dplyr::mutate(max = which.max(nCountries))
    fnl_cmb$max <- NA
    for(i in 4:27){
      fnl_cmb$max[which(fnl_cmb$nIndicators == i)[which(fnl_cmb$nCountries[fnl_cmb$nIndicators == i] == max(fnl_cmb$nCountries[fnl_cmb$nIndicators == i]))]] <- 1
    }; rm(i)
    write.csv(fnl_cmb, paste0(data_path,"/outputs/edge_frontier/all_maximum_combinations.csv"), row.names = F)
    fnl_cmb <- fnl_cmb %>% tibble::as.tibble()
    fnl_cmb <- fnl_cmb %>% dplyr::mutate(indicators_list = textFile)
    saveRDS(fnl_cmb, paste0(data_path,"/outputs/edge_frontier/all_maximum_combinations_tibble.RDS"))
  } else {
    fnl_cmb <- readRDS(paste0(data_path,"/outputs/edge_frontier/all_maximum_combinations_tibble.RDS"))
  }
  
  return(fnl_cmb)
  
}
# df: Data frame with scale adjusted data SFS indicators
# envPos: Positions for environmental indicators
# ecoPos: Positions for economic indicators
# socPos: Positions for social indicators
# fntPos: Positions for food and nutrition indicators
frontier_df   <- edge_frontier(df = all_data, envPos = 2:9, ecoPos = 10:12, socPos = 13:15, fntPos = 16:28)
rm(data_path, all_data, edge_frontier, frontier_df)
