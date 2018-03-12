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
suppressMessages(if(!require(EnvStats)){install.packages('EnvStats'); library(EnvStats)} else {library(EnvStats)})
suppressMessages(library(compiler))

## ========================================================================== ##
## Define countries to work with
## ========================================================================== ##

# Worldwide shapefile
# countries <- rgdal::readOGR(dsn = "./Input_data/world_shape", "all_countries")
# countries$COUNTRY <- iconv(countries$COUNTRY, from = "UTF-8", to = "latin1")

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

## ========================================================================== ##
## Selecting the maximum values
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
}; rm(i, df)
textFile2 <- textFile[combID]
rm(textFile, dfs, nInd, combID)

## ========================================================================== ##
## Sensitivity analysis
## ========================================================================== ##
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

all_combinations$iso3c <- factor(all_combinations$iso3c)
all_combinations$mean_type <- factor(all_combinations$mean_type)
all_combinations$Subsample <- factor(all_combinations$Subsample)
all_combinations$theoretical <- factor(all_combinations$theoretical)
all_combinations$combination <- factor(all_combinations$combination)

test <- all_combinations %>%
  dplyr::select(iso3c, theory, SFS_index, mean_type, Subsample, combination) %>%
  tidyr::spread(key = mean_type, value = SFS_index)

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
  ggplot(aes(x = cv, fill = mean_type, colour = mean_type)) +
  geom_density(alpha = 0.1) +
  facet_wrap(~theory)


















HDI_results %>% ggplot(aes(x = Subsample, y = SFS_index, group = iso3c)) + geom_line()

HDI_results %>% group_by(iso3c) %>% summarise(cv = sd(SFS_index, na.rm = T)/mean(SFS_index, na.rm = T)) %>%
  ggplot(aes(x = cv)) + geom_density()






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
