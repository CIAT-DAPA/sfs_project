# Fitting models for sustainable food systems project
# Implemented by: H. Achicanoy, P. Alvarez & L. Lamotte
# CIAT, 2017

# R options
options(warn = -1); options(scipen = 999)
OSys <- Sys.info(); OSys <- OSys[names(OSys)=="sysname"]
if(OSys == "Linux"){
  wk_dir <- "/mnt/workspace_cluster_9/Sustainable_Food_System/Input_data/"; setwd(wk_dir); rm(wk_dir)
} else {
  if(OSys == "Windows"){
    wk_dir <- "//dapadfs/workspace_cluster_9/Sustainable_Food_System/Input_data"; setwd(wk_dir); rm(wk_dir)
  }
}; rm(OSys)

# Load packages
suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(maptools)){install.packages('maptools'); library(maptools)} else {library(maptools)})
suppressMessages(if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} else {library(dplyr)})
suppressMessages(if(!require(tidyr)){install.packages('tidyr'); library(tidyr)} else {library(tidyr)})
suppressMessages(if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)} else {library(ggplot2)})
suppressMessages(if(!require(jsonlite)){install.packages('jsonlite'); library(jsonlite)} else {library(jsonlite)})
suppressMessages(if(!require(foreach)){install.packages('foreach'); library(foreach)} else {library(foreach)})
suppressMessages(if(!require(doMC)){install.packages('doMC'); library(doMC)} else {library(doMC)})
suppressMessages(if(!require(XML)){install.packages('XML'); library(XML)} else {library(XML)})
suppressMessages(if(!require(plspm)){install.packages('plspm'); library(plspm)} else {library(plspm)})
suppressMessages(if(!require(reshape)){install.packages('reshape'); library(reshape)} else {library(reshape)})
suppressMessages(if(!require(VIM)){install.packages('VIM'); library(VIM)} else {library(VIM)})
suppressMessages(library(compiler))

# Worldwide shapefile
countries <- rgdal::readOGR(dsn = "./world_shape", "all_countries")
countries$COUNTRY <- iconv(countries$COUNTRY, from = "UTF-8", to = "latin1")

complete_data <- readRDS(file = "data_joined.RDS")

# Missing values analysis

# Include some plots to explain this

# Method 1: Imputing missing values using k nearest neighbors
complete_data1 <- VIM::kNN(data = complete_data)

# Method 2: mice package (assumption of linear regression) 3.

# Method 3: Amelia package (assumption of multivariate normality)

# Method 4: missForest package 2.

# Method 5: Hmisc package 1.

# Method 6: mi package

# Method 7: simputation package

### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
### PLS-PM: Using repeated indicators                                                                         ###
### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###

# Define path model matrix
# path matrix (inner model realtionships) Here should be the dimensions of our model
NUTR <- c(0, 0, 0, 0)
HINT <- c(0, 0, 0, 0)
FSCY <- c(0, 0, 0, 0)
SUFS <- c(1, 1, 1, 0)
sfs_path <- rbind(NUTR, HINT, FSCY, SUFS); rm(NUTR, HINT, FSCY, SUFS)
# add optional column names
colnames(sfs_path) <- rownames(sfs_path)
innerplot(sfs_path)

# List of blocks for outer model
sfs_blocks <- list(2:3, 4:5, 6:9, 2:9)
# sfs_blocks <- list(c("varnames1"), c("varnames2"), c("varnames3"))

# List of modes
sfs_modes <- rep("A", 4)

# Running the model
sfs_pls <- plspm(complete_data[complete.cases(complete_data),], sfs_path, sfs_blocks, modes = sfs_modes)
plot(sfs_pls)
pairs(sfs_pls$scores)

indices <- as.data.frame(sfs_pls$scores)
indices$iso3 <- as.character(complete_data[complete.cases(complete_data),"ISO3"])

saveRDS(object = indices, file = "../Results/sfs_index_v0.RDS")

xloads = melt(sfs_pls$crossloadings, id.vars = c("name", "block"))

gg <- ggplot(data = xloads, aes(x = name, y = value, fill = block))
gg <- gg + geom_hline(yintercept = 0, color = "gray75")
gg <- gg + geom_hline(yintercept = c(-0.5, 0.5), color = "gray70", linetype = 2)
gg <- gg + geom_bar(stat = 'identity', position = 'dodge')
gg <- gg + facet_wrap(block ~ variable)
gg <- gg + theme(axis.text.x = element_text(angle = 90), line = element_blank())
gg <- gg + ggtitle("Crossloadings")
gg

highchart(type = "map") %>%
  hc_add_series_map(map = worldgeojson, df = indices, value = "SUFS", joinBy = "iso3") %>%
  hc_colorAxis(stops = color_stops()) %>%
  hc_tooltip(useHTML = TRUE, headerFormat = "",
             pointFormat = "this is {point.name} and have {point.population} people with gni of {point.GNI}")

### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
### Clustering methodologies                                                                                  ###
### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###

data_test <- complete_data[complete.cases(complete_data),]; rownames(data_test) <- data_test$ISO3

library(cluster)
library(factoextra)

res_dist <- get_dist(data_test[,-1], stand = TRUE, method = "pearson")
fviz_dist(dist.obj = res_dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Using k-means
fviz_nbclust(data_test[,-1], kmeans, method = "gap_stat")

get_clust_tendency(scale(data_test[,-1]), n = 50,
                   gradient = list(low = "steelblue",  high = "white"))
