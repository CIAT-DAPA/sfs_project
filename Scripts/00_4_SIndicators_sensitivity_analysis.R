# Sensitivity analysis: SFS project
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
                                EnvStats, compiler, caretEnsemble, tabplot, moments))

## ========================================================================== ##
## Define countries to work with
## ========================================================================== ##

# Country code translation
country_codes <- read.csv("./country_codes_update_28_09_18.csv")

## ========================================================================== ##
## Load SFS indicators data
## ========================================================================== ##

all_data <- read.csv("sfs_raw_indicators.csv", row.names = 1)

## ========================================================================== ##
## Correlations
## ========================================================================== ##

# Raw data (just transforming pH indicator)
png(height = 1200, width = 1200, pointsize = 25, file = "./_graphs/correlation_matrix.png")
all_data[,-1] %>% cor(use = "pairwise.complete.obs", method = "spearman") %>% corrplot(type = "upper", method = "square", tl.pos = "lt")
all_data[,-1] %>% cor(use = "pairwise.complete.obs", method = "spearman") %>% corrplot(add = T, type = "lower", method = "number",
                                                                                       diag = FALSE, tl.pos = "n", cl.pos = "n", number.cex = 0.5, number.digits = 2)
dev.off()

# Transformed data (after transforming pH indicator and applyin Box-Cox transform for skewed indicators)
skew <- apply(X = all_data, MARGIN = 2, FUN = function(x){x %>% as.numeric %>% moments::skewness(., na.rm = T)})
all_data_transformed <- all_data
for(i in 2:length(skew)){
  if(skew[i] > 2 | skew[i] < -2){
    all_data_transformed[,i][which(all_data_transformed[,i] == 0)] <- 0.01
    optPar   <- EnvStats::boxcox(x = all_data_transformed[,i], optimize = T)
    all_data_transformed[,i] <- EnvStats::boxcoxTransform(x = all_data_transformed[,i], lambda = optPar$lambda)
  } else { all_data_transformed[,i] <- all_data_transformed[,i] }
}
png(height = 1200, width = 1200, pointsize = 25, file = "./_graphs/correlation_matrix_trns_data.png")
all_data_transformed[,-1] %>% cor(use = "pairwise.complete.obs", method = "spearman") %>% corrplot(type = "upper", method = "square", tl.pos = "lt")
all_data_transformed[,-1] %>% cor(use = "pairwise.complete.obs", method = "spearman") %>% corrplot(add = T, type = "lower", method = "number",
                                                                                       diag = FALSE, tl.pos = "n", cl.pos = "n", number.cex = 0.5, number.digits = 2)
dev.off()
rm(all_data_transformed, optPar, i, skew)

## ========================================================================== ##
## Missing data
## ========================================================================== ##

library(tabplot)
all_data2 <- all_data
names(all_data2)[2:8] <- paste0("Env_", 1:7)
names(all_data2)[9:11] <- paste0("Eco_", 1:3)
names(all_data2)[12:14] <- paste0("Soc_", 1:3)
names(all_data2)[15:ncol(all_data2)] <- paste0("FnN_", 1:14)
tab <- tabplot::tableplot(dat = all_data2[,-1], nBins = nrow(all_data2), scales = "lin", fontsize = 6)
tabplot::tableSave(tab, filename = "missing_data2.png", width = 8, height = 6, fontsize = 6, legend.lines = 6)

all_data[,-1] %>% tabplot::tableplot()

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

dfs <- readRDS("./Results/modelling_results/counts_and_indicators.rds")

# Edge of maximum number of countries: 24 possibilities
tsv <- dfs %>%
  dplyr::group_by(nIndicators) %>%
  dplyr::summarise(MaxCount = max(nCountries)) %>%
  ggplot(aes(x = nIndicators, y = MaxCount)) + geom_point(size = 4) +
  theme_bw() +
  scale_x_continuous(breaks = 4:27) +
  scale_y_continuous(breaks = seq(0, 170, 25), limits = c(0, 170)) +
  xlab("Number of indicators") +
  ylab("Number of countries with complete data") +
  theme(axis.title = element_text(size = 20),
        axis.text  = element_text(size = 15))
ggsave(filename = "./_graphs/countries_vs_indicators.png", plot = tsv, device = "png", units = "in", width = 8, height = 8)

# All possible combinations
tsv <- dfs %>%
  ggplot(aes(x = nIndicators, y = nCountries)) + geom_point(size = 4) +
  theme_bw() +
  scale_x_continuous(breaks = 4:27) +
  scale_y_continuous(breaks = seq(0, 170, 25), limits = c(0, 170)) +
  xlab("Number of indicators") +
  ylab("Number of countries with complete data") +
  theme(axis.title = element_text(size = 20),
        axis.text  = element_text(size = 15))
ggsave(filename = "./_graphs/all_possible_combinations.png", plot = tsv, device = "png", units = "in", width = 8, height = 8)

## ========================================================================== ##
## Find all possible combinations backwards
## ========================================================================== ##
# nInd <- dfs$nIndicators[dfs$maxCombinations == "Yes"]
# textFile2
# paths <- lapply(X = 1:length(nInd), function(i){
#   cat("Processing combination:", nInd[i], "...\n")
#   path_finder(data = all_data, combList = textFile2[i][[1]], id = nInd[i])
# })

## =================================================================================== ##
## Sensitivity analysis: standarizing the dataset as first step
## =================================================================================== ##
skew_methods <- c("none", "log", "box_cox")
calculateIndices2 <- function(data = all_data, correct_skew = "none", combList = textFile2[[17]], theory = "true", fnt_type = "geometric"){
  
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
  signs <- c(NA, -1, +1, -1, +1, -1, +1, -1, +1, -1, +1, +1, +1, +1, +1, -1, +1, +1, +1, -1, -1, -1, -1, +1, +1, -1, -1, -1)
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

sensitivity_results <- readRDS("./sensitivity_analysis_skewness_fltr.rds")
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
#       typeList <- c("geometric", "arithmetic")
#       typeResults <- lapply(X = 1:length(typeList), function(k){
#         
#         results <- calculateIndices2(data = all_data,
#                                      combList = textFile2[[i]],
#                                      theory = theoryList[j],
#                                      fnt_type = typeList[k])
#         results$mean_type <- typeList[k]
#         results$theory <- theoryList[j]
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
# saveRDS(sensitivity_results, "./sensitivity_analysis.rds")
# sensitivity_results <- readRDS("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/sensitivity_analysis.rds")
# sensitivity_results_fltr <- sensitivity_results %>% purrr::map(.f = function(x){
#   x_fltr <- x %>% dplyr::filter(mean_type == "arithmetic")
#   x_fltr$Approach <- x_fltr$mean_type <- x_fltr$theory <- NULL
#   return(x_fltr)
# })
# saveRDS(sensitivity_results_fltr, "./sensitivity_analysis_fltr.rds")
sensitivity_results <- readRDS("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/sensitivity_analysis_skewness_fltr.rds")
sensitivity_results[[19]]

# Measure importance of factors
sensitivity_results[[17]] %>% View()

datos <- sensitivity_results[[17]] %>% filter(iso3c == "BEL")
model <- aov(SFS_index ~ Subsample + combination + Subsample:combination, data = datos)
summary(model)
datos %>% ggplot(aes(x = combination, y = Subsample, fill = SFS_index)) + geom_raster()

## =================================================================================== ##
## Stability plot: internal variability
## =================================================================================== ##

# Este si va
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
## Instability plot: external variability
## =================================================================================== ##

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







sensitivity_results[[17]] %>%
  dplyr::filter(mean_type == "geometric" & iso3c == "VNM") %>%
  ggplot(aes(x = reorder(iso3c, SFS_index, FUN = median), y = SFS_index, fill = factor(nIndicators))) +
  geom_boxplot() +
  xlab("Country") +
  ylab("Sustainable Food Systems index")

set.seed(107)
test <- snsr_flt[complete.cases(snsr_flt),]
inTrain <- createDataPartition(y = test$SFS_index, p = .75, list = FALSE)
training <- test[inTrain,]
testing <- test[-inTrain,]
my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(training$SFS_index, 25),
  summaryFunction=twoClassSummary
)
caretEnsemble::caretEnsemble()

# Rank operators
# Update indicators list
textFile2_uptd <- textFile2[1:19]
# Calculate SFS reference index
skew_methods <- c("none", "log", "box_cox")
calc_sfs_index <- function(combList = textFile2_uptd[[17]], correct_skew = "none", data = all_data, fnt_type = "geometric"){
  
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
  signs <- c(NA, -1, +1, -1, +1, -1, +1, -1, +1, -1, +1, +1, +1, +1, +1, -1, +1, +1, +1, -1, -1, -1, -1, +1, +1, -1, -1, -1)
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

rank_summary <- rep(NA, length(textFile2_uptd))
stdv_summary <- rep(NA, length(textFile2_uptd))
for(i in 1:length(textFile2_uptd)){
  
  ref_vals <- calc_sfs_index(combList = textFile2_uptd[[i]], correct_skew = "box_cox", data = all_data, fnt_type = "arithmetic")
  ref_cntr <- calc_sfs_index(combList = textFile2_uptd[[i]], correct_skew = "box_cox", data = all_data, fnt_type = "arithmetic") %>% rownames %>% sort
  
  snsr_flt <- sensitivity_results[[i]]
  snsr_flt <- snsr_flt %>% dplyr::filter(iso3c %in% ref_cntr & skew_method == "box_cox")
  
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
gnrl_summary$nCountries <- dfs$nCountries[dfs$maxCombinations == "Yes"][1:nrow(gnrl_summary)]

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


gnrl_summary2 <- gnrl_summary %>% dplyr::select(cmbn, rank_scaled, stdv_scaled) %>% tidyr::gather(key = , value = , -cmbn)

gnrl_summary %>% filter(cmbn %in% 15:20) %>%
  ggplot(aes(x = cmbn, y = mtrc, colour = stdv, size = stdv)) +
  geom_point()


## Visualize data
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
      hc_colorAxis(stops = colstops, min = min(indices$SFS_index), max = max((indices$SFS_index))) %>%
      hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
      hc_mapNavigation(enabled = TRUE) %>%
      hc_add_theme(thm)
  )
  
}
ref_vals <- calc_sfs_index(combList = textFile2[[17]], correct_skew = "box_cox", data = all_data, fnt_type = "arithmetic")
ref_vals <- calc_sfs_index(combList = textFile2[[1]], correct_skew = "box_cox", data = all_data, fnt_type = "arithmetic")
ref_vals <- calc_sfs_index(combList = textFile2[[24]], correct_skew = "box_cox", data = all_data, fnt_type = "arithmetic")

ref_vals$Country <- country_codes$country.name.en[match(rownames(ref_vals), country_codes$iso3c)]

ref_vals %>%
  ggplot(aes(x = reorder(Country, SFS_index), y = SFS_index, fill = SFS_index)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("")

ref_vals$ISO3 <- rownames(ref_vals); rownames(ref_vals) <- 1:nrow(ref_vals)
sfsMap(Scores = ref_vals)
#############################################################################

library(sf)
library(spData)
library(spDataLarge)
library(tmap)
library(RColorBrewer)

world <- sf::st_read("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/Input_data/world_shape/all_countries_robinson_all.shp")
world2 <- dplyr::left_join(x = world, y = ref_vals, by = "ISO3")

tsv <- tm_shape(world2) +
  tm_polygons("SFS_index", palette = brewer.pal(n = 100, name = "RdBu")) +
  tm_borders("gray20", lwd = .5) +
  tm_grid(projection = "longlat") +
  tm_layout(inner.margins = c(0, .02, .02, .02))
tmap_save(tm = tsv, filename = "_graphs/sfs_index_map.png", width = 16, height = 8, units = "in")
tmap_save(tm = tsv, filename = "_graphs/sfs_index_map_164countries.png", width = 16, height = 8, units = "in")
tmap_save(tm = tsv, filename = "_graphs/sfs_index_map_17countries.png", width = 16, height = 8, units = "in")



vals <- rep(NA, length(ref_countries))
for(i in 1:length(ref_countries)){
  vals[i] <- median(filtered_df[filtered_df$iso3c == ref_countries[i], "SFS_index"]) - ref_values[rownames(ref_values)==ref_countries[i],"SFS_index"]
}
hist(vals)


# 1. Obtain reference countries
df <- sensitivity_results[[17]] %>%
  dplyr::filter(mean_type == "geometric" & nIndicators == 20)
ref_countries <- df$iso3c %>% unique %>% sort
ref_values <- df %>% dplyr::filter(mean_type == "geometric" & nIndicators == 20)
df <- sensitivity_results[[17]] %>%
  dplyr::filter(iso3c %in% ref_countries)
df[df$combination == 1, "iso3c"] %>% unique %>% length

# 2. Calculate index for reference countries
# 3. Obtain rank of reference countries
# 4. Filter all possibilities by reference countries
# 5. Obtain rank of countries for all possibilities







textFile2 <- db_new$Indicators
all_combinations2 <- lapply(X = 1:length(textFile2), FUN = function(i){
  
  theoryList <- c("true")
  theoryResults <- lapply(X = 1:length(theoryList), function(j){
    
    typeList <- c("geometric", "arithmetic")
    typeResults <- lapply(X = 1:length(typeList), function(k){
      
      results <- calculateIndices2(data = all_data,
                                   combList = textFile2[[i]],
                                   theory = theoryList[j],
                                   fnt_type = typeList[k])
      results$mean_type <- typeList[k]
      results$theory <- theoryList[j]
      results$combination <- i
      results$nIndicators <- length(textFile2[[i]])
      return(results)
      
    })
    typeResults <- do.call(rbind, typeResults)
    return(typeResults)
    
  })
  theoryResults <- do.call(rbind, theoryResults)
  return(theoryResults)
  
})
all_combinations2 <- do.call(rbind, all_combinations2)

##
all_combinations2 %>% filter(iso3c %in% c("ARG", "COL", "FRA", "USA", "CAN", "VNM")) %>%
  ggplot(aes(x = as.numeric(nIndicators), y = SFS_index, group = mean_type, fill = mean_type, colour = mean_type)) +
  geom_point() + facet_grid(theory~iso3c) +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_continuous(breaks = 4:27, labels = 4:27) +
  xlab("Number of indicators")
##
all_combinations2 %>% dplyr::filter(mean_type == "geometric" & theory == "true") %>% ggplot(aes(x = reorder(iso3c, SFS_index, FUN = median), y = SFS_index)) + geom_boxplot()




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



saveRDS(object = all_combinations2, file = "HDI_sensitivity_analysis2.rds")
all_combinations2 <- readRDS("HDI_sensitivity_analysis2.rds")

db %>% group_by(nIndicators) %>% sample()

# Scatterplot
all_combinations2 %>%
  ggplot(aes(x = Environment, y = SFS_index, group = mean_type, colour = mean_type)) +
  geom_point() +
  facet_grid(mean_type ~ theory)

# Calculate median
median_calculated <- all_combinations2 %>%
  select(SFS_index:Subsample, iso3c:combination) %>%
  group_by(iso3c, mean_type, theory, combination) %>%
  summarise(SFS_index = median(SFS_index, na.rm = T))
median_calculated %>%
  filter(iso3c %in% c("ARG", "COL", "FRA", "USA", "CAN", "VNM")) %>%
  ggplot(aes(x = as.numeric(combination), y = SFS_index, group = mean_type, fill = mean_type, colour = mean_type)) +
  geom_point() + facet_grid(theory~iso3c) +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_continuous(breaks = 1:24, labels = 4:27) +
  xlab("Number of indicators")

median_calculated %>%
  group_by(iso3c, mean_type, theory) %>%
  summarise(SFS_index = median(SFS_index, na.rm = T))

# Calculate successive differences
successiveDiff <- median_calculated %>%
  dplyr::group_by(iso3c, mean_type, theory) %>%
  tidyr::nest() %>%
  dplyr::mutate(sDiff = purrr::map(.x = .$data, .f = function(x){
    df <- data.frame(combination = x$combination, Difference = x$SFS_index - dplyr::lag(x = x$SFS_index, n = 1))
    return(df)
  }))
successiveDiff <- tidyr::unnest(successiveDiff)
successiveDiff$combination1 <- NULL
successiveDiff %>%
  filter(iso3c %in% c("ARG", "COL", "FRA", "USA", "CAN", "VNM")) %>%
  ggplot(aes(x = as.numeric(combination), y = Difference, group = mean_type, fill = mean_type, colour = mean_type)) +
  geom_point() + facet_grid(theory~iso3c) +
  geom_hline(yintercept = 0, color = "blue") +
  scale_x_continuous(breaks = 1:24, labels = 4:27) +
  xlab("Number of indicators")
successiveDiff %>%
  filter(iso3c %in% c("ARG", "COL", "FRA", "USA", "CAN", "VNM")) %>%
  ggplot(aes(x = Difference, group = mean_type, fill = mean_type, colour = mean_type)) +
  geom_density(alpha = .1) + facet_grid(theory~iso3c) +
  geom_vline(xintercept = 0, color = "blue") +
  # scale_x_continuous(breaks = 1:24, labels = 4:27) +
  xlab("Differences")





# Parallel coordinates plot
median_calculated %>%
  tidyr::spread(key = combination, value = SFS_index) %>%
  as.data.frame %>%
  dplyr::select(iso3c, mean_type, theory, 17:18) %>%
  parcoords::parcoords(., rownames = FALSE,
                       reorder = TRUE, brushMode = "1D",
                       color = list(
                         colorScale = htmlwidgets::JS('d3.scale.category10()'),
                         colorBy = "sp"))

median_calculated %>%
  tidyr::spread(key = combination, value = SFS_index) %>% View





# ================================================================ #
# Calcular rango y plotearlo
rango <- all_combinations2 %>%
  select(SFS_index:Subsample, iso3c:combination) %>%
  group_by(iso3c, mean_type, theory, combination) %>%
  summarise(Range = max(SFS_index, na.rm = T) - min(SFS_index, na.rm = T))
rango %>%
  #filter(iso3c %in% c("FRA")) %>%
  ggplot(aes(x = as.numeric(combination), y = Range, group = mean_type, fill = mean_type, colour = mean_type)) +
  geom_point() + facet_wrap(~theory)
# ================================================================ #



test <- all_combinations %>%
  dplyr::select(iso3c, theory, SFS_index, mean_type, Subsample, combination) %>%
  tidyr::spread(key = mean_type, value = SFS_index)

test <- all_combinations %>%
  dplyr::select(iso3c, theory, SFS_index, mean_type, Subsample, combination) %>%
  tidyr::spread(key = theory, value = SFS_index)

plot(test$false, test$true, pch = 20); abline(0,1)

all_combinations %>%
  filter(iso3c == "COL") %>%
  ggplot(aes(x = SFS_index, fill = mean_type, colour = mean_type)) + geom_density(alpha = 0.1)

# GLM approach
glm.fit <- all_combinations %>%
  dplyr::select(iso3c, mean_type, Subsample, theoretical, combination, SFS_index) %>%
  speedglm::speedglm(formula = SFS_index ~ ., data = ., family = binomial())
summary(glm.fit)
glm.explainer <- DALEX::explain(glm.fit, data = all_combinations, y = all_combinations$SFS_index)
glm.vd <- DALEX::variable_dropout(glm.explainer, type = "raw")
plot(glm.vd)

# Random forest approach
rf.fit <- all_combinations[complete.cases(all_combinations),] %>%
  dplyr::select(mean_type, theoretical, combination, SFS_index) %>%
  randomForest::randomForest(formula = SFS_index ~ ., data = ., ntree = 100)
importance(rf.fit)
rf.explainer <- DALEX::explain(model = rf.fit, data = all_combinations, y = all_combinations$SFS_index)
rf.vd <- DALEX::variable_dropout(explainer = rf.explainer, type = "raw")
plot(rf.vd)


all_combinations %>% ggplot(aes(x = SFS_index, fill = mean_type, colour = mean_type)) + # filter(mean_type == "arithmetic") %>% 
  geom_density(alpha = 0.1) +
  facet_grid(theory~combination) +
  theme_classic()

all_combinations2 <- all_combinations %>%
  select(SFS_index:Subsample, iso3c:combination) %>%
  group_by(iso3c, mean_type, theory, combination) %>%
  summarise(SFS_index = median(SFS_index, na.rm = T))

glm.fit <- glm(formula = SFS_index ~ ., data = all_combinations2[complete.cases(all_combinations2),], family = binomial)
anova(glm.fit, test = "Chi")


glm.col <- all_combinations2[complete.cases(all_combinations2),] %>%
  filter(iso3c == "COL") %>%
  glm(SFS_index ~ mean_type + theoretical + combination, data = ., family = binomial)
anova(glm.col, test = "Chi")

all_combinations2[complete.cases(all_combinations2),] %>%
  filter(iso3c == "COL") %>% ggplot(aes(x = combination, y = SFS_index)) + geom_point()

all_combinations2[complete.cases(all_combinations2),] %>%
  filter(iso3c == "COL") %>% ggplot(aes(x = SFS_index)) + geom_density()

all_combinations2 <- all_combinations %>%
  select(SFS_index:Subsample, iso3c:combination) %>%
  group_by(iso3c, mean_type, theory, combination) %>%
  summarise(SFS_index = median(SFS_index, na.rm = T)) %>%
  spread(key = combination, value = SFS_index)
names(all_combinations2)[4:ncol(all_combinations2)] <- paste0("comb_", names(all_combinations2)[4:ncol(all_combinations2)])
all_combinations2$theory <- as.character(all_combinations2$theory)

parcoords::parcoords(all_combinations2[,1:13], rownames = FALSE, # [,c("iso3c", paste0("comb_", 1:10))]
                     reorder = TRUE, brushMode="1D",
                     color = list(
                       colorScale = htmlwidgets::JS('d3.scale.category10()'),
                       colorBy = "sp"))

cv_data <- all_combinations %>%
  select(SFS_index:Subsample, iso3c:combination) %>%
  group_by(iso3c, mean_type, theory, combination) %>%
  summarise(cv = sd(SFS_index, na.rm = T)/mean(SFS_index, na.rm = T))

cv_data %>% ggplot(aes(x = cv, fill = mean_type, colour = mean_type)) +
  geom_density(alpha = 0.1) +
  facet_grid(theory~combination) +
  theme_classic()

cv_data %>%
  dplyr::filter(iso3c == "COL") %>%
  ggplot(aes(y = cv, x = mean_type, colour = mean_type)) +
  geom_boxplot(alpha = 0.1) +
  facet_wrap(~theory)



all_combinations2 %>%
  filter(iso3c %in% c("FRA")) %>%
  ggplot(aes(x = as.numeric(combination), y = SFS_index, group = mean_type, fill = mean_type, colour = mean_type)) +
  geom_point() + facet_wrap(~theory)


all_combinations2 %>%
  filter(iso3c %in% c("ARG", "COL", "FRA", "USA", "CAN", "VNM")) %>%
  ggplot(aes(x = as.numeric(combination), y = SFS_index, group = mean_type, fill = mean_type, colour = mean_type)) +
  geom_point() + facet_grid(theory~iso3c)


all_combinations %>%
  ggplot(aes(x = as.numeric(combination), y = SFS_index, group = mean_type, fill = mean_type, colour = mean_type)) +
  geom_point() + facet_wrap(~theory)
