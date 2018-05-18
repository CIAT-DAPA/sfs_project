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
library(pacman)
pacman::p_load(raster, rgdal, maptools, jsonlite, foreach, doParallel, XML, plspm, reshape, tidyverse, countrycode, caret,
               missMDA, missForest, treemap, viridisLite, highcharter, corrplot, cluster, factoextra, FactoMineR, gghighlight,
               EnvStats,compiler)

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
dfs %>%
  dplyr::group_by(nIndicators) %>%
  dplyr::summarise(MaxCount = max(nCountries)) %>%
  ggplot(aes(x = nIndicators, y = MaxCount)) + geom_point()

## ========================================================================== ##
## Find all possible combinations backwards
## ========================================================================== ##
nInd <- dfs$nIndicators[dfs$maxCombinations == "Yes"]
textFile2
paths <- lapply(X = 1:length(nInd), function(i){
  cat("Processing combination:", nInd[i], "...\n")
  path_finder(data = all_data, combList = textFile2[i][[1]], id = nInd[i])
})

## =================================================================================== ##
## Sensitivity analysis: standarizing each dataset for each combination of indicators
## =================================================================================== ##
calculateIndices <- function(data = all_data, combList = textFile2[[17]], theory = "true", fnt_type = "geometric"){
  
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
  
  # Updating data set
  data <- data[which(complete.cases(data[, mtch])),]
  
  # HDI approach
  HDI_approach <- function(data = data, varInd = mtch, theory = theory, fnt_type = "geometric"){
    
    rNames <- data$iso3c
    
    # Step 1. Normalization function for all indicators
    normalization <- function(x){
      y = x/max(x)
      # y = (x - min(x))/(max(x) - min(x))
      return(y)
    }
    
    # Step 2. Normalize indicadors and apply a correction for those indicators which have negative polarity
    for(m in mtch){
      data[,m] <- normalization(x = data[,m])
      if(theory == "true"){
        if(signs[m] < 0){data[,m] <- 1 - data[,m]}
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
  #HDI_approach(data = data, varInd = mtch, theoretical = T, fnt_type = "arithmetic") %>% View
  
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

all_combinations <- lapply(X = 1:length(textFile2), FUN = function(i){
  
  theoryList <- c("true", "false")
  theoryResults <- lapply(X = 1:length(theoryList), function(j){
    
    typeList <- c("geometric", "arithmetic")
    typeResults <- lapply(X = 1:length(typeList), function(k){
      
      results <- calculateIndices(data = all_data,
                                  combList = textFile2[[i]],
                                  theory = theoryList[j],
                                  fnt_type = typeList[k])
      results$mean_type <- typeList[k]
      results$theory <- theoryList[j]
      results$combination <- i
      return(results)
      
    })
    typeResults <- do.call(rbind, typeResults)
    return(typeResults)
    
  })
  theoryResults <- do.call(rbind, theoryResults)
  return(theoryResults)
  
})
all_combinations <- do.call(rbind, all_combinations)
all_combinations %>% View

saveRDS(object = all_combinations, file = "HDI_sensitivity_analysis.rds")
all_combinations <- readRDS("HDI_sensitivity_analysis.rds")


## =================================================================================== ##
## Sensitivity analysis: standarizing the dataset as first step
## =================================================================================== ##
calculateIndices2 <- function(data = all_data, combList = textFile2[[17]], theory = "true", fnt_type = "geometric"){
  
  theory <<- theory
  fnt_type <<- fnt_type
  
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
        if(signs[m] < 0){data[,m] <- 1 - data[,m]}
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

combFiles <- list.files(path = "./Best_combinations/RDSfiles", full.names = T) %>% gtools::mixedsort()
sensitivity_results <- lapply(X = 1:length(combFiles), function(i){
  
  db <- readRDS(combFiles[i])
  db$matches <- lapply(1:nrow(db), function(i){ sum(textFile2[[1]] %in% db$Indicators[[i]]) }) %>% unlist
  db <- db %>% dplyr::filter(matches == 4)
  db$nIndicators %>% table %>% names
  db$nIndicators %>% table
  nInd <- db$nIndicators %>% unique %>% sort
  smpls <- round(table(db$nIndicators)/sum(table(db$nIndicators)) * 100) + 1
  if(sum(table(db$nIndicators) > 100) > 1){
    db_new <- lapply(X = 1:length(nInd), function(j){
      db_nInd <- db %>% dplyr::filter(nIndicators == nInd[j])
      set.seed(1234)
      smpl <- sample(x = 1:nrow(db_nInd), size = smpls[j], replace = F)
      db_nInd <- db_nInd[smpl,]
      return(db_nInd)
    })
    db_new <- do.call(rbind, db_new)
  } else {
    db_new <- db; rm(db)
  }
  
  textFile2 <- db_new$Indicators
  all_combinations2 <- lapply(X = 1:length(textFile2), FUN = function(i){
    
    theoryList <- c("true", "false")
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
  
  return(all_combinations2)
})

textFile2 <- db_new$Indicators
all_combinations2 <- lapply(X = 1:length(textFile2), FUN = function(i){
  
  theoryList <- c("true", "false")
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


## Partial indices
combList <- textFile2[[17]]

# Step 1. Normalization function for all indicators
normalization <- function(x){
  y = x/max(x, na.rm = T)
  # y = (x - min(x))/(max(x) - min(x))
  return(y)
}

data <- all_data
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
HDI_approach(data = data, varInd = mtch, theory = "true", fnt_type = "arithmetic")
HDI_approach(data = data, varInd = mtch, theory = "true", fnt_type = "geometric")









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
