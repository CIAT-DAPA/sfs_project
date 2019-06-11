# Path finder function: calculate all backwards possible combinations
# Implemented by: H. Achicanoy & P. Alvarez
# CIAT, 2019

# R options
stop("This code needs to be run just once!"); g <- gc(reset = T); rm(list = ls()); options(scipen = 999, warn = -1)

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

# Load edge's frontier
frontier_df <- readRDS(paste0(data_path,"/outputs/edge_frontier/all_maximum_combinations_tibble.RDS"))
frontier_df_fltrd <- frontier_df %>% dplyr::filter(max == 1)
frontier_df_fltrd <- frontier_df_fltrd %>% dplyr::arrange(nIndicators); rm(frontier_df)

# Function to calculate all backwards possible combinations
path_finder <- function(df = all_data, combList = dfs2$Indicators[[761]], id = nInd){
  
  # -------------------------------------------------------------------- #
  # Verify names position
  # -------------------------------------------------------------------- #
  envPos <- 2:9; ecoPos <- 10:12; socPos <- 13:15; fntPos <- 16:28
  mtch <- match(combList, names(df))
  envUpt <- base::intersect(envPos, mtch)
  ecoUpt <- base::intersect(ecoPos, mtch)
  socUpt <- base::intersect(socPos, mtch)
  fntUpt <- base::intersect(fntPos, mtch)
  
  # -------------------------------------------------------------------- #
  # Create all possible combinations for each dimension
  # -------------------------------------------------------------------- #
  # Environment
  envPos <- envUpt
  envCountries <- lapply(1:length(envPos), function(i){
    combinations <- combn(x = names(df)[envPos], m = i) # Combination of indicators
    combCounts <- lapply(1:ncol(combinations), function(j){
      lComb <- list(Indicators = combinations[,j])
      return(lComb)
    })
    return(combCounts)
  })
  names(envCountries) <- 1:length(envPos)
  env_mxIndc <- purrr::modify_depth(envCountries, 2, "Indicators")
  
  # Economic
  ecoPos <- ecoUpt
  ecoCountries <- lapply(1:length(ecoPos), function(i){
    combinations <- combn(x = names(df)[ecoPos], m = i) # Combination of indicators
    combCounts <- lapply(1:ncol(combinations), function(j){
      lComb <- list(Indicators = combinations[,j])
      return(lComb)
    })
    return(combCounts)
  })
  names(ecoCountries) <- 1:length(ecoPos)
  eco_mxIndc <- purrr::modify_depth(ecoCountries, 2, "Indicators")
  
  # Social
  socPos <- socUpt
  socCountries <- lapply(1:length(socPos), function(i){
    combinations <- combn(x = names(df)[socPos], m = i) # Combination of indicators
    combCounts <- lapply(1:ncol(combinations), function(j){
      lComb <- list(Indicators = combinations[,j])
      return(lComb)
    })
    return(combCounts)
  })
  names(socCountries) <- 1:length(socPos)
  soc_mxIndc <- purrr::modify_depth(socCountries, 2, "Indicators")
  
  # Food and nutrition
  fntPos <- fntUpt
  fntCountries <- lapply(1:length(fntPos), function(i){
    combinations <- combn(x = names(df)[fntPos], m = i) # Combination of indicators
    # For each combination of indicators calculate the number of countries with complete data
    combCounts <- lapply(1:ncol(combinations), function(j){
      lComb <- list(Indicators = combinations[,j])
      return(lComb)
    })
    return(combCounts)
  })
  names(fntCountries) <- 1:length(fntPos)
  fnt_mxIndc <- purrr::modify_depth(fntCountries, 2, "Indicators")
  
  # -------------------------------------------------------------------- #
  # Concatenate names within all possibilities in each dimension
  # -------------------------------------------------------------------- #
  env_mxIndc <- env_mxIndc %>% purrr::map(., function(x){lapply(1:length(x), function(i){paste(x[[i]], collapse = "_")})})
  eco_mxIndc <- eco_mxIndc %>% purrr::map(., function(x){lapply(1:length(x), function(i){paste(x[[i]], collapse = "_")})})
  soc_mxIndc <- soc_mxIndc %>% purrr::map(., function(x){lapply(1:length(x), function(i){paste(x[[i]], collapse = "_")})})
  fnt_mxIndc <- fnt_mxIndc %>% purrr::map(., function(x){lapply(1:length(x), function(i){paste(x[[i]], collapse = "_")})})
  
  outfile <- paste0(data_path, "/outputs/edge_frontier/bckwd_combinations/bckwd_",id,".txt")
  
  if(!file.exists(outfile)){
    
    # -------------------------------------------------------------------- #
    # Concatenate names between all possibilities
    # -------------------------------------------------------------------- #
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
      shell(code)
      
    }
    createCode(code = gsub("txt","py",outfile))
    
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
    
    grep2 <- Vectorize(grep, "pattern") %>% unlist()
    
    finalCombinations <- lapply(1:length(textFile), function(i){
      mtch <- match(textFile[[i]], names(df))
      df <- data.frame(
        nCountries  = sum(complete.cases(df[,textFile[[i]]])),
        nIndicators = length(textFile[[i]]),
        nEnv        = length(base::intersect(envPos, mtch)),
        nEco        = length(base::intersect(ecoPos, mtch)),
        nSoc        = length(base::intersect(socPos, mtch)),
        nFnt        = length(base::intersect(fntPos, mtch))
      )
      return(df)
    })
    finalCombinations <- do.call(rbind, finalCombinations)
    
    cnts <- table(finalCombinations$nIndicators) %>% as.numeric
    cmbt <- names(table(finalCombinations$nIndicators))
    finalCombinations2 <- lapply(1:length(cnts), function(i){
      if(cnts[i] > 100){
        
        df_aux  <- finalCombinations %>% dplyr::filter(nIndicators == cmbt[i])
        txt_aux <- textFile[textFile %>% purrr::map(., length) %>% unlist %in% cmbt[i] %>% which()]
        nzx    <- caret::nearZeroVar(df_aux)
        idx    <- clhs::clhs(df_aux[,-nzx], size = 50, progress = F, iter = 1000)
        df_aux <- df_aux[idx,]
        textFile_upd <- txt_aux[idx]
        df_aux <- df_aux %>% dplyr::mutate(Indicators = textFile_upd)
        
      } else {
        
        df_aux  <- finalCombinations %>% dplyr::filter(nIndicators == cmbt[i])
        txt_aux <- textFile[textFile %>% purrr::map(., length) %>% unlist %in% cmbt[i] %>% which()]
        df_aux  <- df_aux %>% dplyr::mutate(Indicators = txt_aux)
        
      }
      return(df_aux)
    })
    finalCombinations2 <- do.call(rbind, finalCombinations2)
    saveRDS(finalCombinations2, paste0(data_path, "/outputs/edge_frontier/bckwd_combinations/bckwd_",id,".RDS"))
    
    return(cat("Done.\n"))
    
  } else {
    
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
    
    grep2 <- Vectorize(grep, "pattern") %>% unlist()
    
    finalCombinations <- lapply(1:length(textFile), function(i){
      mtch <- match(textFile[[i]], names(df))
      df <- data.frame(
        nCountries  = sum(complete.cases(df[,textFile[[i]]])),
        nIndicators = length(textFile[[i]]),
        nEnv        = length(base::intersect(envPos, mtch)),
        nEco        = length(base::intersect(ecoPos, mtch)),
        nSoc        = length(base::intersect(socPos, mtch)),
        nFnt        = length(base::intersect(fntPos, mtch))
      )
      return(df)
    })
    finalCombinations <- do.call(rbind, finalCombinations)
    
    cnts <- table(finalCombinations$nIndicators) %>% as.numeric
    cmbt <- names(table(finalCombinations$nIndicators))
    finalCombinations2 <- lapply(1:length(cnts), function(i){
      if(cnts[i] > 100){
        
        df_aux  <- finalCombinations %>% dplyr::filter(nIndicators == cmbt[i])
        txt_aux <- textFile[textFile %>% purrr::map(., length) %>% unlist %in% cmbt[i] %>% which()]
        nzx    <- caret::nearZeroVar(df_aux)
        tryCatch(expr = {idx <- clhs::clhs(df_aux[,-nzx], size = 50, progress = F, iter = 1000)}, error = function(e){idx <<- which.max(df_aux$nCountries)})
        df_aux <- df_aux[idx,]
        textFile_upd <- txt_aux[idx]
        df_aux <- df_aux %>% dplyr::mutate(Indicators = textFile_upd)
        
      } else {
        
        df_aux  <- finalCombinations %>% dplyr::filter(nIndicators == cmbt[i])
        txt_aux <- textFile[textFile %>% purrr::map(., length) %>% unlist %in% cmbt[i] %>% which()]
        df_aux  <- df_aux %>% dplyr::mutate(Indicators = txt_aux)
        
      }
      return(df_aux)
    })
    finalCombinations2 <- do.call(rbind, finalCombinations2)
    saveRDS(finalCombinations2, paste0(data_path, "/outputs/edge_frontier/bckwd_combinations/bckwd_",id,".RDS"))
    
    return(cat("Done.\n"))
    
  }
  
}

nInd <- unlist(lapply(1:length(frontier_df_fltrd$indicators_list), function(i) length(frontier_df_fltrd$indicators_list[[i]])))
set.seed(1235); nInd <- paste0(nInd,"_",sample(1:100,32,replace = F))

# Parallelization
clusterExport <- local({
  gets <- function(n, v) { assign(n, v, envir = .GlobalEnv); NULL }
  function(cl, list, envir = .GlobalEnv) {
    ## do this with only one clusterCall--loop on slaves?
    for (name in list) {
      clusterCall(cl, gets, name, get(name, envir = envir))
    }
  }
})
createCluster <- function(noCores, logfile = "/dev/null", export = NULL, lib = NULL) {
  require(doSNOW)
  cl <- makeCluster(noCores, type = "SOCK", outfile = logfile)
  if(!is.null(export)) clusterExport(cl, export)
  if(!is.null(lib)) {
    plyr::l_ply(lib, function(dum){
      clusterExport(cl, "dum", envir = environment())
      clusterEvalQ(cl, library(dum, character.only = TRUE))
    })
  }
  registerDoSNOW(cl)
  return(cl)
}
cl  <- createCluster(16, export = list("path_finder","data_path","all_data","frontier_df_fltrd","nInd"), lib = list("tidyverse","clhs","caret"))
paths <- parallel::parLapply(cl, 1:length(nInd), function(i){
  cat("Processing combination:", nInd[i], "...\n")
  path_finder(df = all_data, combList = frontier_df_fltrd$indicators_list[i][[1]], id = nInd[i])
})
parallel::stopCluster(cl)
rm(data_path, all_data, path_finder, nInd, clusterExport, createCluster, cl)
