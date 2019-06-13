# Sensitivity analysis
# Proposed by: C. Bene
# Implemented by: H. Achicanoy
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
all_data <- read.csv(paste0(data_path,"/outputs/indicators/sfs_raw_indicators_scales_adjusted.csv"), row.names = 1)

# Load edge's frontier
frontier_df <- readRDS(paste0(data_path,"/outputs/indicators/edge_frontier/all_maximum_combinations_tibble.RDS"))
frontier_df_fltrd <- frontier_df %>% dplyr::filter(max == 1)
frontier_df_fltrd <- frontier_df_fltrd %>% dplyr::arrange(nIndicators); rm(frontier_df)

# Calculate SFS index
calc_sfs_index  <- function(combList = frontier_df_fltrd$indicators_list[24][[1]], data = all_data, fnt_type = "arithmetic"){
  
  # Updating dimension indexes
  signs <- c(NA,
             -1, -1, -1, +1, -1, +1, +1, -1,
             +1, -1, -1,
             +1, +1, +1,
             +1, -1, -1, +1, +1, -1, -1, -1, -1, +1, -1, -1, -1)
  envPos <- 2:9; ecoPos <- 10:12; socPos <- 13:15; fntPos <- 16:28
  mtch <- match(combList, names(data))
  envUpt <- base::intersect(envPos, mtch)
  ecoUpt <- base::intersect(ecoPos, mtch)
  socUpt <- base::intersect(socPos, mtch)
  fntUpt <- base::intersect(fntPos, mtch)
  
  # Step 1. Normalization function for all indicators
  normalization <- function(x){
    y <- (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
    return(y)
  }
  
  for(j in 2:ncol(data)){
    data[,j] <- normalization(x = data[,j])
    data[which(data[,j] == 0), j] <- data[which(data[,j] == 0), j] + 0.01
  }; rm(j)
  
  # Updating data set
  data <- data[which(complete.cases(data[, mtch])),]
  
  # Step 2. Normalize indicadors and apply a correction for those indicators which have negative polarity
  for(m in mtch){
    if(signs[m] < 0){
      data[,m] <- 1 - data[,m]
      data[which(data[,m] == 0), m] <- data[which(data[,m] == 0), m] + 0.01
    }
  }; rm(m)
  
  # HDI approach
  HDI_approach <- function(data = data, varInd = mtch, fnt_type = "geometric"){
    
    rNames <- data$iso3c
    
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
  ref_vals <- HDI_approach(data = data, varInd = mtch, fnt_type = fnt_type)
  return(ref_vals)
  
}

# Sensitiivty analysis
sensitivity_introduce_changes <- function(all_data = all_data, nCountries = c(10,20), chg = c(0.1,0.2,0.3)){
  
  # Obtain reference values
  reference <- calc_sfs_index(combList = frontier_df_fltrd$indicators_list[24][[1]], data = all_data, fnt_type = "arithmetic")
  
  # Environment
  df <- all_data
  
  countries <- match(rownames(reference), df$iso3c)
  #df$iso3c[rowsID]
  
  set.seed(1235)
  rowsID <- sample(countries, nCountries, replace = F) %>% sort()
  
  df$Emissions.agriculture.total[rowsID] <- df$Emissions.agriculture.total[rowsID] * (1 + chg)
  chg_env_dim <- calc_sfs_index(combList = frontier_df_fltrd$indicators_list[24][[1]], data = df, fnt_type = "arithmetic")
  
  # Economic
  df <- all_data
  
  df$AgValueAdded[rowsID] <- df$AgValueAdded[rowsID] * (1 + chg)
  chg_eco_dim <- calc_sfs_index(combList = frontier_df_fltrd$indicators_list[24][[1]], data = df, fnt_type = "arithmetic")
  
  # Social
  df <- all_data
  
  df$Female.labor.force[rowsID] <- df$Female.labor.force[rowsID] * (1 + chg)
  chg_soc_dim <- calc_sfs_index(combList = frontier_df_fltrd$indicators_list[24][[1]], data = df, fnt_type = "arithmetic")
  
  # Food and nutrition
  df <- all_data
  
  df$Food.loss[rowsID] <- df$Food.loss[rowsID] * (1 + chg)
  chg_fnt_dim <- calc_sfs_index(combList = frontier_df_fltrd$indicators_list[24][[1]], data = df, fnt_type = "arithmetic")
  
  
  df_res <- data.frame(reference   = reference$SFS_index,
                       environment = chg_env_dim$SFS_index,
                       economic    = chg_eco_dim$SFS_index,
                       social      = chg_soc_dim$SFS_index,
                       food_nutr   = chg_fnt_dim$SFS_index)
  rownames(df_res) <- rownames(reference)
  cases <- df_res[which(rownames(df_res) %in% df$iso3c[rowsID]),]
  cases[,2] <- (cases[,2] - cases[,1])/cases[,1]
  cases[,3] <- (cases[,3] - cases[,1])/cases[,1]
  cases[,4] <- (cases[,4] - cases[,1])/cases[,1]
  cases[,5] <- (cases[,5] - cases[,1])/cases[,1]
  cases %>% apply(2, mean)
  
  df_res2 <- df_res
  df_res2[,2] <- (df_res2[,2] - df_res2[,1])/df_res2[,1]
  df_res2[,3] <- (df_res2[,3] - df_res2[,1])/df_res2[,1]
  df_res2[,4] <- (df_res2[,4] - df_res2[,1])/df_res2[,1]
  df_res2[,5] <- (df_res2[,5] - df_res2[,1])/df_res2[,1]
  df_res2$social[df_res2$environment < 0] <- 0
  df_res2$social[df_res2$economic < 0]    <- 0
  df_res2$social[df_res2$social < 0]      <- 0
  df_res2$economic[df_res2$food_nutr < 0] <- 0
  df_res2 %>% apply(2, mean)
  
  barplot(cases$social, names.arg = rownames(cases))
  
}
