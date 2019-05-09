# Sensitivity analysis: SFS project
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

# Load edge's frontier
frontier_df <- readRDS(paste0(data_path,"/outputs/edge_frontier/all_maximum_combinations_tibble.RDS"))
frontier_df_fltrd <- frontier_df %>% dplyr::filter(max == 1)
frontier_df_fltrd <- frontier_df_fltrd %>% dplyr::arrange(nIndicators); rm(frontier_df)

# Plot: All possible combinations
if(!file.exists(paste0(data_path,"/outputs/edge_frontier/all_possible_combinations.png"))){
  # All possible combinations
  frontier_df %>%
    ggplot(aes(x = nIndicators, y = nCountries)) + geom_point(size = 4) +
    theme_bw() +
    scale_x_continuous(breaks = 4:27) +
    scale_y_continuous(breaks = seq(0, 170, 25), limits = c(0, 170)) +
    xlab("Number of indicators") +
    ylab("Number of countries with complete data") +
    theme(axis.title = element_text(size = 20),
          axis.text  = element_text(size = 15)) +
    ggsave(filename = paste0(data_path,"/outputs/edge_frontier/all_possible_combinations.png"), units = "in", width = 8, height = 8)
}
# Plot: Edge's frontier
if(!file.exists(paste0(data_path,"/outputs/edge_frontier/countries_vs_indicators.png"))){
  frontier_df_fltrd %>%
    ggplot(aes(x = nIndicators, y = nCountries)) + geom_point(size = 4) +
    theme_bw() +
    scale_x_continuous(breaks = 4:27) +
    scale_y_continuous(breaks = seq(0, 170, 25), limits = c(0, 170)) +
    xlab("Number of indicators") +
    ylab("Number of countries with complete data") +
    theme(axis.title = element_text(size = 20),
          axis.text  = element_text(size = 15)) +
    ggsave(filename = paste0(data_path,"/outputs/edge_frontier/countries_vs_indicators.png"), units = "in", width = 8, height = 8)
}

# Calculate SFS index
calc_sfs_index <- function(combList = textFile2[[17]], data = all_data, fnt_type = "arithmetic"){
  
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

# Optimize category assignation of categorical data

# Adjust Fairtrade categories
## Frequency-based assignment
if(!file.exists(paste0(data_path,"/outputs/edge_frontier/SFS_index_frontier_values_fairtrade_frequency.csv"))){
  indices <- lapply(1:length(frontier_df_fltrd$indicators_list), function(i){
    df1 <- all_data[,c('iso3c', frontier_df_fltrd$indicators_list[i][[1]])]
    df1$Country <- rownames(df1)
    df1 <- df1 %>% dplyr::select(iso3c, Country)
    df2 <- calc_sfs_index(combList = frontier_df_fltrd$indicators_list[i][[1]], data = all_data, fnt_type = "arithmetic")
    df2$iso3c <- rownames(df2); rownames(df2) <- 1:nrow(df2)
    df2 <- df2 %>% dplyr::select(iso3c, SFS_index)
    df3 <- dplyr::inner_join(x = df1, y = df2, by = 'iso3c'); rm(df1, df2)
    return(df3)
  })
  country_codes <- read.csv(paste0(data_path,"/inputs_raw/country_codes.csv"))
  country_codes <- country_codes %>% dplyr::select(iso3c, country.name.en)
  colnames(country_codes)[2] <- "Country"
  indices[[1]] <- dplyr::left_join(x = country_codes, y = indices[[1]], by = c("iso3c", "Country"))
  indices_df <- Reduce(function(x, y) merge(x, y, by = c("iso3c", "Country"), all.x = T), indices)
  colnames(indices_df)[3:ncol(indices_df)] <- paste0("SFS_index_", nInd)
  indices_df$ToRemove <- apply(indices_df[3:ncol(indices_df)], 1, function(x) sum(is.na(x)))
  indices_df <- indices_df[which(indices_df$ToRemove != 32),]
  write.csv(indices_df, paste0(data_path,"/outputs/edge_frontier/SFS_index_frontier_values_fairtrade_frequency.csv"), row.names = F)
}
## Binary-based assignment
if(!file.exists(paste0(data_path,"/outputs/edge_frontier/SFS_index_frontier_values_fairtrade_binary.csv"))){
  all_data <- read.csv(paste0(data_path,"/outputs/sfs_raw_indicators_scales_adjusted_fairtrade_binary.csv"))
  indices <- lapply(1:length(frontier_df_fltrd$indicators_list), function(i){
    df1 <- all_data[,c('iso3c', frontier_df_fltrd$indicators_list[i][[1]])]
    df1$Country <- rownames(df1)
    df1 <- df1 %>% dplyr::select(iso3c, Country)
    df2 <- calc_sfs_index(combList = frontier_df_fltrd$indicators_list[i][[1]], data = all_data, fnt_type = "arithmetic")
    df2$iso3c <- rownames(df2); rownames(df2) <- 1:nrow(df2)
    df2 <- df2 %>% dplyr::select(iso3c, SFS_index)
    df3 <- dplyr::inner_join(x = df1, y = df2, by = 'iso3c'); rm(df1, df2)
    return(df3)
  })
  country_codes <- read.csv(paste0(data_path,"/inputs_raw/country_codes.csv"))
  country_codes <- country_codes %>% dplyr::select(iso3c, country.name.en)
  colnames(country_codes)[2] <- "Country"
  indices[[1]] <- dplyr::left_join(x = country_codes, y = indices[[1]], by = c("iso3c", "Country"))
  indices_df <- Reduce(function(x, y) merge(x, y, by = c("iso3c", "Country"), all.x = T), indices)
  colnames(indices_df)[3:ncol(indices_df)] <- paste0("SFS_index_", nInd)
  indices_df$ToRemove <- apply(indices_df[3:ncol(indices_df)], 1, function(x) sum(is.na(x)))
  indices_df <- indices_df[which(indices_df$ToRemove != 32),]
  write.csv(indices_df, paste0(data_path,"/outputs/edge_frontier/SFS_index_frontier_values_fairtrade_binary.csv"), row.names = F)
}
## Optimal scaling-based assignment
if(!file.exists(paste0(data_path,"/outputs/edge_frontier/SFS_index_frontier_values_fairtrade_optiscale.csv"))){
  ft <- readxl::read_excel(paste0(data_path,"/inputs_raw/sfs_social_raw_indicators.xlsx"), sheet = "fair_trade")
  indices_df <- read.csv(paste0(data_path,"/outputs/edge_frontier/SFS_index_frontier_values_fairtrade_frequency.csv"))
  indices_df2 <- dplyr::left_join(x = indices_df, y = ft, by = "Country")
  # Optimal scaling for Fairtrade categories
  cols_id <- frontier_df_fltrd$indicators_list %>% purrr::map(function(x){!("Fairtrade.ctg" %in% x)}) %>% unlist %>% which() %>% + 2
  opt_scl <- cols_id %>% purrr::map(function(i){optiscale::opscale(x.qual = indices_df2$Category, x.quant = indices_df2[,i], level = 1, process = 1)})
  opt_scl <- opt_scl %>% purrr::map(function(x){data.frame(Categories = x$qual, Quantification = x$os)})
  opt_scl <- do.call(base::cbind, opt_scl)
  opt_scl <- opt_scl[,-grep("Categories",colnames(opt_scl))[-1]]
  opt_scl$Median <- apply(opt_scl[,-1], 1, median, na.rm = T)
  opt_scl$iso3c <- indices_df2$iso3c
  opt_scl <- opt_scl[,c("iso3c","Median")]
  all_data$Fairtrade.ctg <- NA
  all_data$Fairtrade.ctg[match(opt_scl$iso3c, all_data$iso3c)] <- opt_scl$Median
  write.csv(x = all_data, file = paste0(data_path,"/outputs/sfs_raw_indicators_scales_adjusted_final.csv"), row.names = T)
  
  # Re-calculate indices
  indices <- lapply(1:length(frontier_df_fltrd$indicators_list), function(i){
    df1 <- all_data[,c('iso3c', frontier_df_fltrd$indicators_list[i][[1]])]
    df1$Country <- rownames(df1)
    df1 <- df1 %>% dplyr::select(iso3c, Country)
    df2 <- calc_sfs_index(combList = frontier_df_fltrd$indicators_list[i][[1]],
                          data     = all_data,
                          fnt_type = "arithmetic")
    df2$iso3c <- rownames(df2); rownames(df2) <- 1:nrow(df2)
    df2 <- df2 %>% dplyr::select(iso3c, SFS_index)
    df3 <- dplyr::inner_join(x = df1, y = df2, by = 'iso3c'); rm(df1, df2)
    return(df3)
  })
  country_codes <- read.csv(paste0(data_path,"/inputs_raw/country_codes.csv"))
  country_codes <- country_codes %>% dplyr::select(iso3c, country.name.en)
  colnames(country_codes)[2] <- "Country"
  indices[[1]] <- dplyr::left_join(x = country_codes, y = indices[[1]], by = c("iso3c", "Country"))
  indices_df <- Reduce(function(x, y) merge(x, y, by = c("iso3c", "Country"), all.x = T), indices)
  colnames(indices_df)[3:ncol(indices_df)] <- paste0("SFS_index_", nInd)
  indices_df$ToRemove <- apply(indices_df[3:ncol(indices_df)], 1, function(x) sum(is.na(x)))
  indices_df <- indices_df[which(indices_df$ToRemove != 32),]
  
  write.csv(indices_df, paste0(data_path,"/outputs/edge_frontier/SFS_index_frontier_values_fairtrade_optiscale.csv"), row.names = F)
}

comparison <- T
if(comparison){
  db1 <- readxl::read_excel(paste0(data_path,"/outputs/edge_frontier/edge_frontier_fairtrade_categories_comparison.xlsx"), sheet = 1)
  db2 <- readxl::read_excel(paste0(data_path,"/outputs/edge_frontier/edge_frontier_fairtrade_categories_comparison.xlsx"), sheet = 2)
  db3 <- readxl::read_excel(paste0(data_path,"/outputs/edge_frontier/edge_frontier_fairtrade_categories_comparison.xlsx"), sheet = 3)
  
  db1 <- db1 %>% tidyr::gather(Combination, SFS_index, -c(iso3c:Country))
  db1$SFS_index <- db1$SFS_index %>% as.numeric()
  db1$Method    <- "Frecuency"
  
  db2 <- db2 %>% tidyr::gather(Combination, SFS_index, -c(iso3c:Country))
  db2$SFS_index <- db2$SFS_index %>% as.numeric()
  db2$Method    <- "Binary"
  
  db3 <- db3 %>% tidyr::gather(Combination, SFS_index, -c(iso3c:Country))
  db3$SFS_index <- db3$SFS_index %>% as.numeric()
  db3$Method    <- "Optimal_scaling"
  
  db <- rbind(db1, db2, db3); rm(db1, db2, db3)
  
  db %>%
    dplyr::filter(Combination == "SFS_index_22") %>%
    ggplot2::ggplot(aes(x = reorder(Country, SFS_index), y = SFS_index, colour = Method)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::xlab("") +
    ggplot2::ylab("Country food system sustainability scores") +
    ggplot2::theme(axis.text.x = element_text(hjust = 0.95, vjust = 0.2, angle = 90))
  
}

all_data  <- read.csv(paste0(data_path,"/outputs/sfs_raw_indicators_scales_adjusted_final.csv"), row.names = 1)
sfs_index <- calc_sfs_index(combList = frontier_df_fltrd$indicators_list[24][[1]],
                            data     = all_data,
                            fnt_type = "arithmetic")
countries <- read.csv(paste0(data_path,"/inputs_raw/country_codes.csv"))
sfs_index$iso3c <- rownames(sfs_index)
rownames(sfs_index) <- 1:nrow(sfs_index)
sfs_index <- dplyr::left_join(x = sfs_index, y = countries %>% dplyr::select(iso3c, country.name.en), by = "iso3c")
sfs_index <- sfs_index %>% dplyr::select(iso3c, country.name.en, Environment, Economic, Social, Food_nutrition, SFS_index)
rownames(sfs_index) <- sfs_index$country.name.en; sfs_index$country.name.en <- NULL
write.csv(sfs_index, paste0(data_path,"/outputs/sfs_final_index.csv"), row.names = T)

## =================================================================================== ##
## Sensitivity analysis: standarizing the dataset as first step
## =================================================================================== ##
skew_methods <- c("none", "log", "box_cox")
calculateIndices2 <- function(data = all_data, correct_skew = "none", combList = textFile2[[17]], theory = "true", fnt_type = "arithmetic"){
  
  # Measure skewness and apply scale transformations
  skew <- apply(X = data, MARGIN = 2, FUN = function(x){x %>% as.numeric %>% moments::skewness(., na.rm = T)})
  if(correct_skew == "log"){
    for(i in 2:length(skew)){
      if(skew[i] > 2 | skew[i] < -2){data[,i][which(data[,i] == 0)] <- 0.01; data[,i] <- log(data[,i])} else {data[,i] <- data[,i]}
    }
  } else {
    if(correct_skew == "box_cox"){
      for(i in 2:length(skew)){
        if(skew[i] > 2 | skew[i] < -2){
          data[,i][which(data[,i] == 0)] <- 0.01
          optPar   <- EnvStats::boxcox(x = data[,i], optimize = T)
          data[,i] <- EnvStats::boxcoxTransform(x = data[,i], lambda = optPar$lambda)
        } else { data[,i] <- data[,i] }
      }
    } else {
      if(correct_skew == "none"){
        data <- data
      }
    }
  }
  
  theory <<- theory
  fnt_type <<- fnt_type
  
  # Updating dimension indexes
  signs <- c(NA, -1, -1, -1, +1, -1, +1, -1, +1, -1, +1, +1, +1, +1, +1, -1, -1, +1, +1, -1, -1, -1, -1, +1, +1, -1, -1, -1)
  envPos <- 2:8; ecoPos <- 9:11; socPos <- 12:14; fntPos <- 15:28
  mtch <- match(combList, names(data))
  envUpt <- base::intersect(envPos, mtch)
  ecoUpt <- base::intersect(ecoPos, mtch)
  socUpt <- base::intersect(socPos, mtch)
  fntUpt <- base::intersect(fntPos, mtch)
  
  # Step 1. Normalization function for all indicators
  normalization <- function(x){
    # y <- x/max(x, na.rm = T)
    y <- (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
    return(y)
  }
  
  for(j in 2:ncol(data)){
    data[,j] <- normalization(x = data[,j])
    data[which(data[,j] == 0), j] <- data[which(data[,j] == 0), j] + 0.01
  }; rm(j)
  
  # Updating data set
  data <- data[which(complete.cases(data[, mtch])),]
  
  # HDI approach
  HDI_approach <- function(data = data, varInd = mtch, theory = theory, fnt_type = "geometric"){
    
    rNames <- data$iso3c
    
    # Step 2. Apply a correction for those indicators which have negative polarity
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
  
  ownJackknife <- function(x = data, FUN = HDI_approach, funName = "HDI_approach"){
    folds <- nrow(x)
    Function <- FUN
    jckk <- lapply(X = 1:folds, FUN = function(i){
      x <- x[-i,]
      df <- Function(data = x, varInd = mtch, theory = theory, fnt_type = fnt_type)
      df$Subsample <- i
      df$Approach <- gsub(pattern = "_approach", replacement = "", x = funName)
      df$iso3c <- rownames(df)
      return(df)
    })
    return(jckk)
  }
  HDI_results <- ownJackknife(x = data, FUN = HDI_approach, funName = "HDI_approach")
  HDI_results <- do.call(rbind, HDI_results)
  rownames(HDI_results) <- 1:nrow(HDI_results)
  
  return(HDI_results)
  
}

# combFiles <- list.files(path = "./Best_combinations/RDSfiles", full.names = T) %>% gtools::mixedsort()
# sensitivity_results <- lapply(X = 1:length(combFiles), function(i){
#   
#   db <- readRDS(combFiles[i])
#   db$matches <- lapply(1:nrow(db), function(i){ sum(textFile2[[1]] %in% db$Indicators[[i]]) }) %>% unlist
#   db <- db %>% dplyr::filter(matches == 4)
#   nInd <- db$nIndicators %>% unique %>% sort
#   smpls <- table(db$nIndicators)[2]
#   if(length(nInd) > 4){
#     db_new <- lapply(X = 1:length(nInd), function(j){
#       db_nInd <- db %>% dplyr::filter(nIndicators == nInd[j])
#       if(nrow(db_nInd) > 1){
#         set.seed(1234)
#         smpl <- sample(x = 1:nrow(db_nInd), size = smpls, replace = F)
#         db_nInd <- db_nInd[smpl,]
#         return(db_nInd)
#       } else {
#         return(db_nInd)
#       }
#     })
#     db_new <- do.call(rbind, db_new)
#   } else {
#     db_new <- db; rm(db)
#   }
#   
#   textFile2 <- db_new$Indicators
#   all_combinations2 <- lapply(X = 1:length(textFile2), FUN = function(i){
#     
#     theoryList <- c("true")
#     theoryResults <- lapply(X = 1:length(theoryList), function(j){
#       
#       typeList <- c("arithmetic")
#       typeResults <- lapply(X = 1:length(typeList), function(k){
#         
#         results <- calculateIndices2(data = all_data,
#                                      correct_skew = "box_cox",
#                                      combList = textFile2[[i]],
#                                      theory = theoryList[j],
#                                      fnt_type = typeList[k])
#         # results$mean_type <- typeList[k]
#         # results$theory <- theoryList[j]
#         results$combination <- i
#         results$nIndicators <- length(textFile2[[i]])
#         return(results)
#         
#       })
#       typeResults <- do.call(rbind, typeResults)
#       return(typeResults)
#       
#     })
#     theoryResults <- do.call(rbind, theoryResults)
#     return(theoryResults)
#     
#   })
#   all_combinations2 <- do.call(rbind, all_combinations2)
#   
#   return(all_combinations2)
# })
# saveRDS(sensitivity_results, "./sensitivity_analysis_skewness_fltr.rds")
sensitivity_results <- readRDS("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/sensitivity_analysis_skewness_fltr2.rds")

## =================================================================================== ##
## Stability plot: internal variability after backwards process
## =================================================================================== ##

tsv <- sensitivity_results[[17]] %>% group_by(nIndicators, iso3c) %>%
  summarise(SFS_index = mean(SFS_index)) %>%
  filter(iso3c %in% c("ARG", "COL", "FRA", "USA", "CAN", "VNM")) %>%
  ggplot(aes(x = as.numeric(nIndicators), y = SFS_index)) +
  geom_point() + facet_wrap(~iso3c, ncol = 3) +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_continuous(breaks = 4:27, labels = 4:27) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("Number of indicators") +
  ylab("Sustainability aggregated score") +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text  = element_text(size = 15))
ggsave(filename = "./_graphs/stability_backwards_20indicators.png", plot = tsv, device = "png", units = "in", width = 16, height = 8)

## =================================================================================== ##
## Instability plot: external variability without backwards process
## =================================================================================== ##

calc_sfs_index <- function(combList = textFile2[[17]], correct_skew = "box_cox", data = all_data, fnt_type = "arithmetic"){
  
  # Measure skewness and apply scale transformations
  skew <- apply(X = data, MARGIN = 2, FUN = function(x){x %>% as.numeric %>% moments::skewness(., na.rm = T)})
  if(correct_skew == "log"){
    for(i in 2:length(skew)){
      if(skew[i] > 2 | skew[i] < -2){data[,i][which(data[,i] == 0)] <- 0.01; data[,i] <- log(data[,i])} else {data[,i] <- data[,i]}
    }
  } else {
    if(correct_skew == "box_cox"){
      for(i in 2:length(skew)){
        if(skew[i] > 2 | skew[i] < -2){
          data[,i][which(data[,i] == 0)] <- 0.01
          optPar   <- EnvStats::boxcox(x = data[,i], optimize = T)
          data[,i] <- EnvStats::boxcoxTransform(x = data[,i], lambda = optPar$lambda)
        } else { data[,i] <- data[,i] }
      }
    } else {
      if(correct_skew == "none"){
        data <- data
      }
    }
  }
  
  # Updating dimension indexes
  signs <- c(NA, -1, -1, -1, +1, -1, +1, -1, +1, -1, +1, +1, +1, +1, +1, -1, -1, +1, +1, -1, -1, -1, -1, +1, +1, -1, -1, -1)
  envPos <- 2:8; ecoPos <- 9:11; socPos <- 12:14; fntPos <- 15:28
  mtch <- match(combList, names(data))
  envUpt <- base::intersect(envPos, mtch)
  ecoUpt <- base::intersect(ecoPos, mtch)
  socUpt <- base::intersect(socPos, mtch)
  fntUpt <- base::intersect(fntPos, mtch)
  
  # Step 1. Normalization function for all indicators
  normalization <- function(x){
    # y = x/max(x, na.rm = T)
    y = (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
    return(y)
  }
  
  for(j in 2:ncol(data)){
    data[,j] <- normalization(x = data[,j])
    data[which(data[,j] == 0), j] <- data[which(data[,j] == 0), j] + 0.01
  }; rm(j)
  
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

# write.csv(cbind(data[,c(1, mtch)], ref_vals), "./sfs_prcs_indicators_plus_indexes.csv", row.names = T)
# ttst <- cbind(data[,c(1, mtch)], ref_vals)

edge_path <- lapply(1:length(textFile2), function(i){
  tbl <- calc_sfs_index(combList = textFile2[[i]], correct_skew = "box_cox", data = all_data, fnt_type = "arithmetic")
  tbl$nIndicators <- length(textFile2[[i]])
  tbl$iso3c <- rownames(tbl)
  rownames(tbl) <- 1:nrow(tbl)
  tbl$nCountries <- nrow(tbl)
  return(tbl)
})
edge_path <- do.call(rbind, edge_path)

# Example 6 countries
tsv <- edge_path %>% group_by(nIndicators, iso3c) %>%
  summarise(SFS_index = mean(SFS_index)) %>%
  filter(iso3c %in% c("ARG", "COL", "FRA", "USA", "CAN", "VNM")) %>%
  ggplot(aes(x = as.numeric(nIndicators), y = SFS_index)) +
  geom_point() + facet_wrap(~iso3c, ncol = 3) +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_continuous(breaks = 4:27, labels = 4:27) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("Number of indicators") +
  ylab("Sustainability aggregated score") +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text  = element_text(size = 15))
ggsave(filename = "./_graphs/stability_issue_6countries.png", plot = tsv, device = "png", units = "in", width = 20, height = 8)

# Same issue with all countries
tsv <- edge_path %>% group_by(nIndicators, iso3c) %>%
  ggplot(aes(x = factor(nIndicators), y = SFS_index)) +
  geom_boxplot() + #facet_wrap(~iso3c, ncol = 3) +
  geom_hline(yintercept = 0, color = "red") +
  #scale_x_continuous(breaks = 4:27, labels = 4:27) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("Number of indicators") +
  ylab("Sustainability aggregated score (for all countries)") +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text  = element_text(size = 15))
ggsave(filename = "./_graphs/stability_issue_all_countries.png", plot = tsv, device = "png", units = "in", width = 12, height = 8)

## =================================================================================== ##
## Measure rank change and internal variability using backwards process results
## =================================================================================== ##

# Rank operators
# Update indicators list
textFile2_uptd <- textFile2[1:19]

rank_summary <- rep(NA, length(textFile2_uptd))
stdv_summary <- rep(NA, length(textFile2_uptd))
for(i in 1:length(textFile2_uptd)){
  
  ref_vals <- calc_sfs_index(combList = textFile2_uptd[[i]], correct_skew = "box_cox", data = all_data, fnt_type = "arithmetic")
  ref_cntr <- calc_sfs_index(combList = textFile2_uptd[[i]], correct_skew = "box_cox", data = all_data, fnt_type = "arithmetic") %>% rownames %>% sort
  
  snsr_flt <- sensitivity_results[[i]]
  snsr_flt <- snsr_flt %>% dplyr::filter(iso3c %in% ref_cntr)
  
  calc_diff <- rep(NA, length(ref_cntr))
  calc_medn <- rep(NA, length(ref_cntr))
  calc_stdv <- rep(NA, length(ref_cntr))
  for(j in 1:length(ref_cntr)){
    calc_diff[j] <- median(snsr_flt[snsr_flt$iso3c == ref_cntr[j], "SFS_index"]) - ref_vals[rownames(ref_vals)==ref_cntr[j],"SFS_index"]
    calc_medn[j] <- median(snsr_flt[snsr_flt$iso3c == ref_cntr[j], "SFS_index"])
    calc_stdv[j] <- sd(snsr_flt[snsr_flt$iso3c == ref_cntr[j], "SFS_index"])
  }
  
  rank_summary[i] <- sum(abs(rank(ref_vals[,"SFS_index"]) - rank(calc_medn)))/length(ref_cntr)
  stdv_summary[i] <- sd(calc_stdv)
  
}

gnrl_summary <- data.frame(cmbn = 4:22, rank = rank_summary, stdv = stdv_summary)
gnrl_summary <- cbind(gnrl_summary, scale(gnrl_summary[,2:3], center = T, scale = T))
colnames(gnrl_summary)[4:5] <- c("rank_scaled", "stdv_scaled")
gnrl_summary$nCountries <- dfs %>%
  dplyr::filter(nIndicators <= 22) %>%
  dplyr::group_by(nIndicators) %>%
  dplyr::summarise(MaxCount = max(nCountries)) %>%
  .$MaxCount

tsv <- gnrl_summary %>%
  ggplot(aes(factor(cmbn), rank, fill = "Rank change")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(factor(cmbn), stdv*1000, fill = "Variability", alpha = 0.8), stat = "identity") +
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Standard deviation")) +
  scale_colour_manual(values = c("blue", "red")) +
  xlab("Number of indicators") +
  ylab("Rank change") +
  guides(alpha = F) +
  theme(legend.title = element_blank()) +
  ggplot2::annotate("text", x = 1:19, y = gnrl_summary$rank + 1, label = lag(x = gnrl_summary$nCountries, n = 1) - gnrl_summary$nCountries, size = 5) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text  = element_text(size = 15),
        legend.title = element_blank(),
        legend.text  = element_text(size = 15))
ggsave(filename = "./_graphs/optimal_combination.png", plot = tsv, device = "png", units = "in", width = 12, height = 8)

## =================================================================================== ##
## Producing final map
## =================================================================================== ##

ref_vals <- calc_sfs_index(combList = textFile2[[17]], correct_skew = "box_cox", data = all_data, fnt_type = "arithmetic")
ref_vals <- calc_sfs_index(combList = textFile2[[1]], correct_skew = "box_cox", data = all_data, fnt_type = "arithmetic")
ref_vals <- calc_sfs_index(combList = textFile2[[24]], correct_skew = "box_cox", data = all_data, fnt_type = "arithmetic")

typeList <- c("static", "interactive")
type <- "static"
if(type == "static"){
  ## Static
  ref_vals$ISO3 <- rownames(ref_vals); rownames(ref_vals) <- 1:nrow(ref_vals)
  suppressMessages(pacman::p_load(sf, spData, tmap, RColorBrewer))
  
  world <- sf::st_read("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/Input_data/world_shape/all_countries_robinson_all.shp")
  world2 <- dplyr::left_join(x = world, y = ref_vals, by = "ISO3")
  
  tsv <- tm_shape(world2) +
    tm_polygons("SFS_index",
                n = 20,
                palette = brewer.pal(n = 20, name = "RdYlBu"),
                breaks = seq(0, .95, .05)) + # "RdBu", breaks = seq(0.01, 1, .01)
    tm_borders("gray20", lwd = .5) +
    tm_grid(projection = "longlat") +
    tm_layout(inner.margins = c(0, .02, .02, .02))
  tmap_save(tm = tsv, filename = "_graphs/sfs_index_map_v3.png", width = 16, height = 8, units = "in")
  tmap_save(tm = tsv, filename = "_graphs/sfs_index_map_164countries.png", width = 16, height = 8, units = "in")
  tmap_save(tm = tsv, filename = "_graphs/sfs_index_map_16countries.png", width = 16, height = 8, units = "in")
} else {
  if(type == "interactive"){
    ## Interactive
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
          hc_colorAxis(stops = colstops, min = min(indices$SFS_index), max = max((indices$SFS_index))) %>%
          hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
          hc_mapNavigation(enabled = TRUE) %>%
          hc_add_theme(thm)
      )
      
    }
    ref_vals$Country <- country_codes$country.name.en[match(rownames(ref_vals), country_codes$iso3c)]
    sfsMap(Scores = ref_vals)
  }
}

## =================================================================================== ##
## Calculating correlations
## =================================================================================== ##

if(!file.exists("./sfs_raw_indicators_plus_index.csv")){
  all_data_index <- dplyr::left_join(x = all_data, y = ref_vals, by = c("iso3c" = "ISO3"))
  rownames(all_data_index) <- rownames(all_data)
  write.csv(all_data_index, "./sfs_raw_indicators_plus_index.csv", row.names = T)
} else {
  all_data_index <- read.csv("./sfs_raw_indicators_plus_index.csv", row.names = 1)
}

if(!file.exists("./sfs_raw_indicators_plus_index_gdp.csv")){
  
  gdp_data <- read.csv("./Input_data_final/gdp_per_capita.csv")
  gdp_data <- gdp_data %>% select(Country.Code, X2016)
  names(gdp_data) <- c("iso3c", "GDP")
  
  all_data_index_gdp <- dplyr::left_join(x = all_data_index, y = gdp_data, by = "iso3c")
  rownames(all_data_index_gdp) <- rownames(all_data_index)
  write.csv(all_data_index_gdp, "./sfs_raw_indicators_plus_index_gdp.csv", row.names = T)
} else {
  all_data_index_gdp <- read.csv("./sfs_raw_indicators_plus_index_gdp.csv", row.names = 1)
}

png(height = 1200, width = 1200, pointsize = 25, file = "./_graphs/correlation_matrix_indicators_index2.png")
all_data_index %>%
  dplyr::select(Emissions.agriculture.total:Serum.retinol.deficiency, SFS_index) %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>% corrplot(type = "upper", method = "square", tl.pos = "lt")
all_data_index %>%
  dplyr::select(Emissions.agriculture.total:Serum.retinol.deficiency, SFS_index) %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>% corrplot(add = T, type = "lower", method = "number",
                                                                       diag = FALSE, tl.pos = "n", cl.pos = "n", number.cex = 0.5, number.digits = 2)
dev.off()

corList <- lapply(X = 2:28, FUN = function(i){
  
  corObj <- broom::tidy(cor.test(x = all_data_index[,i], y = all_data_index$SFS_index, method = "spearman", use = "pairwise.complete.obs"))
  tsv <- all_data_index %>%
    ggplot(aes(x = all_data_index[,i], y = SFS_index)) +
    geom_point() +
    geom_smooth(se = F) +
    xlab(names(all_data_index)[i]) +
    ylab("Sustainability aggregated score") +
    theme(legend.title = element_blank()) +
    theme_bw() +
    theme(axis.title = element_text(size = 20),
          axis.text  = element_text(size = 15),
          legend.title = element_blank(),
          legend.text  = element_text(size = 15))
  ggsave(filename = paste0("./_graphs/_relations_indicators_vs_SFS_index_updtd/relation_SFS_index_vs_", names(all_data_index)[i], ".png"),
         plot = tsv,
         device = "png",
         units = "in",
         width = 8,
         height = 8)
  return(corObj)
  
})
corList <- do.call(rbind, corList)

suppressMessages(pacman::p_load(DALEX, e1071, ceterisParibus, gbm, randomForest, xgboost))

calc_ind_relevance <- function(df = all_data_index_gdp %>% dplyr::select(SFS_index, Obesity, GDP) %>% tidyr::drop_na()){
  
  # Models
  sfs_lm_full <- lm(SFS_index ~ ., data = df)
  sfs_lm_jind <- lm(SFS_index ~ .-GDP, data = df)
  sfs_rf_full <- randomForest(SFS_index ~ ., data = df)
  sfs_rf_jind <- randomForest(SFS_index ~ .-GDP, data = df)
  sfs_sv_full <- svm(SFS_index ~ ., data = df)
  sfs_sv_jind <- svm(SFS_index ~ .-GDP, data = df)
  sfs_gb_full <- gbm(SFS_index ~ ., data = df, n.trees = 500)
  sfs_gb_jind <- gbm(SFS_index ~ .-GDP, data = df, n.trees = 500)
  
  # Explainers
  expl_list <- lapply(X = 1:nrow(df), FUN = function(i){
    expl_lm <- list(DALEX::explain(sfs_lm_full, data = df[-i,-1], y = df$SFS_index[-i]),
                    DALEX::explain(sfs_lm_jind, data = df[-i,-1], y = df$SFS_index[-i]))
    expl_rf <- list(DALEX::explain(sfs_rf_full, data = df[-i,-1], y = df$SFS_index[-i]),
                    DALEX::explain(sfs_rf_jind, data = df[-i,-1], y = df$SFS_index[-i]))
    expl_sv <- list(DALEX::explain(sfs_sv_full, data = df[-i,-1], y = df$SFS_index[-i]),
                    DALEX::explain(sfs_sv_jind, data = df[-i,-1], y = df$SFS_index[-i]))
    expl_gb_f <- DALEX::explain(sfs_gb_full, data = df[-i,-1], y = df$SFS_index[-i], predict_function = function(model, x) predict(model, x, n.trees = 500))
    expl_gb_j <- DALEX::explain(sfs_gb_jind, data = df[-i,-1], y = df$SFS_index[-i], predict_function = function(model, x) predict(model, x, n.trees = 500))
    explainers <- list(expl_lm, expl_rf, expl_sv, expl_gb_f, expl_gb_j)
    return(explainers)
  })
  
  # Model performance
  prfm_list <- lapply(X = 1:length(expl_list), FUN = function(i){
    mp_lm <- list(DALEX::model_performance(expl_list[[i]][[1]][[1]]),
                  DALEX::model_performance(expl_list[[i]][[1]][[2]]))
    mp_rf <- list(DALEX::model_performance(expl_list[[i]][[2]][[1]]),
                  DALEX::model_performance(expl_list[[i]][[2]][[2]]))
    mp_sv <- list(DALEX::model_performance(expl_list[[i]][[3]][[1]]),
                  DALEX::model_performance(expl_list[[i]][[3]][[2]]))
    mp_gb <- list(DALEX::model_performance(expl_list[[i]][[4]]),
                  DALEX::model_performance(expl_list[[i]][[5]]))
    performances <- list(mp_lm, mp_rf, mp_sv, mp_gb)
    return(performances)
  })
  
  # Feature importance
  vimp_list <- lapply(X = 1:length(prfm_list), FUN = function(i){
    vi_lm <- list(DALEX::variable_importance(expl_list[[i]][[1]][[1]], loss_function = loss_root_mean_square, type = "difference"),
                  DALEX::variable_importance(expl_list[[i]][[1]][[2]], loss_function = loss_root_mean_square, type = "difference"))
    vi_rf <- list(DALEX::variable_importance(expl_list[[i]][[2]][[1]], loss_function = loss_root_mean_square, type = "difference"),
                  DALEX::variable_importance(expl_list[[i]][[2]][[2]], loss_function = loss_root_mean_square, type = "difference"))
    vi_sv <- list(DALEX::variable_importance(expl_list[[i]][[3]][[1]], loss_function = loss_root_mean_square, type = "difference"),
                  DALEX::variable_importance(expl_list[[i]][[3]][[2]], loss_function = loss_root_mean_square, type = "difference"))
    vi_gb <- list(DALEX::variable_importance(expl_list[[i]][[4]], loss_function = loss_root_mean_square, type = "difference"),
                  DALEX::variable_importance(expl_list[[i]][[5]], loss_function = loss_root_mean_square, type = "difference"))
    importances <- list(vi_lm, vi_rf, vi_sv, vi_gb)
    return(importances)
  })
  
  for(i in 1:length(vimp_list)){
    for(j in 1:4){
      vimp_list[[i]][[j]][[1]]$method <- "Full"
      vimp_list[[i]][[j]][[2]]$method <- "No GDP"
      vimp_list[[i]][[j]][[1]]$jackniffe <- i
      vimp_list[[i]][[j]][[2]]$jackniffe <- i
    }
  }
  
  vimp_mat <- do.call(rbind, vimp_list %>% purrr::flatten() %>% purrr::flatten())
  vimp_mat %>% ggplot(aes(x = label, y = dropout_loss, fill = variable)) +
    geom_boxplot()
  
  wilcox.test(x = vimp_mat$dropout_loss[vimp_mat$variable == "City.access" & vimp_mat$label == "svm" & vimp_mat$method == "No GDP"],
              y = vimp_mat$dropout_loss[vimp_mat$variable == "City.access" & vimp_mat$label == "svm" & vimp_mat$method == "Full"],
              alternative = "two.sided")
  
}


lapply(X = 2:28, function(i){
  
  df <- all_data_index_gdp %>% dplyr::select(SFS_index, colnames(all_data_index_gdp)[i]) %>% tidyr::drop_na()
  fcor <- broom::tidy(cor.test(x = df[,2], y = df$SFS_index, method = "spearman"))
  fcor$alternative <- NULL
  fcor$method <- "Full correlation"
  df <- all_data_index_gdp %>% dplyr::select(SFS_index, GDP, colnames(all_data_index_gdp)[i]) %>% tidyr::drop_na()
  parcor <- ppcor::pcor.test(x = df[,3], y = df$SFS_index, z = df$GDP, method = "spearman")
  parcor$n <- parcor$gp <- NULL
  colnames(parcor)[ncol(parcor)] <- "method"
  parcor$method <- "Partial correlation"
  semcor <- ppcor::spcor.test(x = df[,3], y = df$SFS_index, z = df$GDP, method = "spearman")
  semcor$n <- semcor$gp <- NULL
  colnames(semcor)[ncol(semcor)] <- "method"
  semcor$method <- "Semi-partial correlation"
  results <- rbind(fcor, parcor, semcor)
  return(results)
  
})

i = 28
r_xy <- cor(df$Serum.retinol.deficiency, df$SFS_index, method = "spearman")
r_xz <- cor(df$Serum.retinol.deficiency, df$GDP, method = "spearman")
r_yz <- cor(df$SFS_index, df$GDP, method = "spearman")

(r_xy - (r_xz*r_yz))/(sqrt(1 - r_xz^2)*sqrt(1 - r_yz^2))

y_resid <- resid(lm(SFS_index ~ GDP, data = df))
x_resid <- resid(lm(Serum.retinol.deficiency ~ GDP, data = df))

cor(x_resid, y_resid, method = "spearman")


broom::tidy(summary(lm(SFS_index ~ Obesity, data = all_data_index2)))
cor.test(all_data_index2$Obesity, all_data_index2$SFS_index, method = "pearson", use = "pairwise.complete.obs")
cor.test(all_data_index$Obesity, all_data_index$SFS_index, method = "pearson", use = "pairwise.complete.obs")

all_data_index$country.name.en <- rownames(all_data_index)
library(plotly)

d <- plotly::highlight_key(all_data_index)
g1 <- ggplot(data = d, aes(x = Female.labor.force, y = SFS_index)) +
  geom_point() +
  xlab("Labor force participation rate, female") +
  ylab("SFS aggregated index")
g2 <- ggplot(data = d, aes(x = Wage.employment, y = Agr.employment)) +
  geom_point() +
  xlab("Wage employment distribution in agriculture") +
  ylab("Employment in agriculture")
g <- g1 %>%
  subplot(g2) %>%
  layout(title = "Click and drag to select points",
         dragmode = 'lasso') %>%
  highlight("plotly_selected")
suppressMessages(build <- plotly_build(g))
build$x$data[[1]]$text <- paste0('Female labor force: ', as.character(round(all_data_index$Female.labor.force, 2)), '<br>', 
                                 'SFS score: ', as.character(round(all_data_index$SFS_index, 2)), '<br>',
                                 'Country: ', as.character(all_data_index$country.name.en))
build$x$data[[2]]$text <- paste0('Wage.employment: ', as.character(round(all_data_index$Wage.employment, 2)), '<br>', 
                                 'Agr.employment: ', as.character(round(all_data_index$Agr.employment, 2)), '<br>',
                                 'Country: ', as.character(all_data_index$country.name.en))
suppressMessages(build)


## =================================================================================== ##
## Plotting ranking boxplots per country
## =================================================================================== ##

i = 17
sensitivity_results[[i]] %>%
  dplyr::group_by(Subsample, combination, nIndicators) %>%
  rank(.$SFS_index)

ttst <- sensitivity_results[[i]]
ttst2 <- ttst %>%
  dplyr::group_by(Subsample, combination, nIndicators) %>%
  dplyr::mutate(Ranking = rank(SFS_index))

ttst2 %>%
  ggplot2::ggplot(aes(x = reorder(iso3c, Ranking, FUN = median), y = Ranking)) +
  geom_boxplot() +
  coord_flip()

# ------------------------------------------------------------------------ #
# Cluster
data[,mtch]
pacman::p_load(diceR)
CC <- consensus_cluster(data[,mtch], nk = 2:10, p.item = 0.8, reps = 5,
                        algorithms = c("hc", "pam", "diana"))
co <- capture.output(str(CC))
strwrap(co, width = 80)
CC <- apply(CC, 2:4, impute_knn, data = data[,mtch], seed = 1)
CC_imputed <- impute_missing(CC, data[,mtch], nk = 4)
sum(is.na(CC))
sum(is.na(CC_imputed))
pam.4 <- CC[, , "PAM_Euclidean", "4", drop = FALSE]
cm <- consensus_matrix(pam.4)
dim(cm)
hm <- graph_heatmap(pam.4)
# ------------------------------------------------------------------------ #
