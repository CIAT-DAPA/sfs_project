# Fitting models for sustainable food systems project
# Implemented by: H. Achicanoy, P. Alvarez & L. Lamotte
# CIAT, 2017

# R options
g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)
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
suppressMessages(if(!require(mice)){install.packages('mice'); library(mice)} else {library(mice)})
suppressMessages(if(!require(Amelia)){install.packages('Amelia'); library(Amelia)} else {library(Amelia)})
suppressMessages(if(!require(missForest)){install.packages('missForest'); library(missForest)} else {library(missForest)})
suppressMessages(if(!require(Hmisc)){install.packages('Hmisc'); library(Hmisc)} else {library(Hmisc)})
suppressMessages(if(!require(mi)){install.packages('mi'); library(mi)} else {library(mi)})
suppressMessages(if(!require(simputation)){install.packages('simputation', dependencies = T); library(simputation)} else {library(simputation)})
suppressMessages(if(!require(highcharter)){install.packages('highcharter', dependencies = T); library(highcharter)} else {library(highcharter)})
suppressMessages(if(!require(igraph)){install.packages('igraph', dependencies = T); library(igraph)} else {library(igraph)})
suppressMessages(if(!require(networkD3)){install.packages('networkD3', dependencies = T); library(networkD3)} else {library(networkD3)})
suppressMessages(if(!require(cluster)){install.packages('cluster', dependencies = T); library(cluster)} else {library(cluster)})
suppressMessages(if(!require(factoextra)){install.packages('factoextra', dependencies = T); library(factoextra)} else {library(factoextra)})
suppressMessages(if(!require(FactoMineR)){install.packages('FactoMineR', dependencies = T); library(FactoMineR)} else {library(FactoMineR)})
suppressMessages(library(compiler))

# Load joined data
complete_data <- readRDS(file = "data_joined.RDS")

# Missing values analysis
# Percent of missing values per variable and combination
VIM::aggr(complete_data)

# Method 1: k nearest neighbors (non-parametric alternative)
complete_data1 <- VIM::kNN(data = complete_data); complete_data1 <- complete_data1[,colnames(complete_data)]

# 3. Method 2: mice (assumption of LM for continuous data and GLM for categorical)

# Method 3: Amelia package (assumption of multivariate normality and LM)

# 2. Method 4: missForest (Uses random forest)
complete_data2 <- missForest::missForest(xmis = complete_data) # Does not work. Check!!!

# 1. Method 5: Hmisc (Uses bootstrap sampling and predictive mean matching to impute missing values)
complete_data3 <- Hmisc::aregImpute(formula = ~ GHI_2000 + ChldMalnutrition + Access_median + Footprint_median + sanitation + water_sources + GDP + political_stability, data = complete_data)
# Requires a function to arrange imputed data after process

# Method 6: mi package
complete_data4 <- mi::mi(complete_data, seed = 335) # Does not work. Check!!!

# Method 7: simputation package


# PLS-PM: Using repeated indicators
# Define path model matrix (inner model)
NUTR <- c(0, 0, 0, 0)
HINT <- c(0, 0, 0, 0)
FSCY <- c(0, 0, 0, 0) # FSCY <- c(1, 1, 0, 0)
SUFS <- c(1, 1, 1, 0)
sfs_path <- rbind(NUTR, HINT, FSCY, SUFS); rm(NUTR, HINT, FSCY, SUFS)
colnames(sfs_path) <- rownames(sfs_path)
innerplot(sfs_path)

# List of blocks for outer model
sfs_blocks <- list(2:3, 4:5, 6:9, 2:9)

# List of modes
sfs_modes <- rep("A", 4)

complete_data1$Access_median <- (-complete_data1$Access_median)

# Running the model
# sfs_pls <- plspm(complete_data[complete.cases(complete_data),], sfs_path, sfs_blocks, modes = sfs_modes)
sfs_pls <- plspm(complete_data1, sfs_path, sfs_blocks, modes = sfs_modes)
outerplot(sfs_pls)

plot(sfs_pls)
pairs(sfs_pls$scores, pch = 20, cex = 2)

indices <- as.data.frame(sfs_pls$scores)
indices$iso3 <- as.character(complete_data1$ISO3)

if(!file.exists("../Results/sfs_index_knn_imputed.RDS")){
  saveRDS(object = indices, file = "../Results/sfs_index_knn_imputed.RDS") # saveRDS(object = indices, file = "../Results/sfs_index_knn_imputed_model2.RDS")
} else {
  indices <- readRDS(file = "../Results/sfs_index_knn_imputed.RDS")
}

indices[,1:(ncol(indices)-1)] <- round(indices[,1:(ncol(indices)-1)], 2)

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
             pointFormat = "{point.name} has a SFS index of {point.SUFS}")

### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
### Clustering methodologies                                                                                  ###
### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###

rownames(complete_data1) <- complete_data1$ISO3

# Calculate similarity measure
sfs_dis <- cluster::daisy(x = complete_data1[,-1], metric = c("gower"), stand = FALSE)
sfs_dis <- 1 - as.matrix(sfs_dis)

# Do cluster analysis
sfs_pca <- FactoMineR::PCA(X = complete_data1[,-1], scale.unit = T, graph = F)
sfs_hpc <- FactoMineR::HCPC(res = sfs_pca, nb.clust = -1, graph = F)
complete_data1$cluster <- sfs_hpc$data.clust$clust

# Visualize using networkD3
sfs_dis[lower.tri(sfs_dis, diag = TRUE)] <- NA
sfs_dis <- na.omit(data.frame(as.table(sfs_dis))); names(sfs_dis) <- c("from", "to", "similarity")
sfs_dis <- sfs_dis[sfs_dis$similarity >= .98,] # Filter by more than 98 degree of similarity

gD <- igraph::simplify(igraph::graph.data.frame(sfs_dis, directed = FALSE))
nodeList <- data.frame(id = c(0:(igraph::vcount(gD) - 1)), name = igraph::V(gD)$name) # because networkD3 library requires IDs to start at 0
getNodeID <- function(x){ which(x == igraph::V(gD)$name) - 1 } # to ensure that IDs start at 0
edgeList <- plyr::ddply(sfs_dis, .variables = c("from", "to", "similarity"), 
                        function (x) data.frame(fromID = getNodeID(x$from), 
                                                toID = getNodeID(x$to)))
nodeList <- cbind(nodeList, nodeDegree = igraph::degree(gD, v = igraph::V(gD), mode = "all")); rm(gD, getNodeID)
nodeList$cluster <- as.numeric(as.character(complete_data1$cluster))[match(nodeList$name, complete_data1$ISO3)]

networkD3::forceNetwork(Links = edgeList,
                        Nodes = nodeList,
                        Source = "fromID",
                        Target = "toID",
                        Value = "similarity",
                        NodeID = "name",
                        Group = "cluster",
                        opacity = 1,
                        fontSize = 15)

# Dynamic time wrapping
sfs_dtw <- TSclust::diss(as.matrix(complete_data1[,-1]), METHOD = "DTWARP", normalize = TRUE)
sfs_dtw_matrix <- as.matrix(sfs_dtw)
sfs_dtw_matrix[lower.tri(sfs_dtw_matrix, diag = TRUE)] <- NA
sfs_dtw_matrix <- na.omit(data.frame(as.table(sfs_dtw_matrix))); names(sfs_dtw_matrix) <- c("Origin", "Recipient", "Similarity")

sfs_dtw_matrix2 <- sfs_dtw_matrix

sfs_dtw_matrix2 <- sfs_dtw_matrix2[sfs_dtw_matrix2$Similarity <= 2000,]

###
gD <- igraph::simplify(igraph::graph.data.frame(sfs_dtw_matrix2, directed=FALSE))
nodeList <- data.frame(ID = c(0:(igraph::vcount(gD) - 1)), # because networkD3 library requires IDs to start at 0
                       nName = igraph::V(gD)$name)
# Map node names from the edge list to node IDs
getNodeID <- function(x){
  which(x == igraph::V(gD)$name) - 1 # to ensure that IDs start at 0
}
# And add them to the edge list 
edgeList <- plyr::ddply(sfs_dtw_matrix2, .variables = c("Origin", "Recipient", "Similarity"), 
                        function (x) data.frame(SourceID = getNodeID(x$Origin), 
                                                TargetID = getNodeID(x$Recipient)))

nodeList <- cbind(nodeList, nodeDegree=igraph::degree(gD, v = igraph::V(gD), mode = "all"))

networkD3::forceNetwork(Links = edgeList, Nodes = nodeList, Source = "SourceID",
             Target = "TargetID", Value = "Similarity", NodeID = "nName",
             Group = "nodeDegree", opacity = 0.4)

library(visNetwork)
visNetwork(nodeList, edgeList) %>%
  visOptions(highlightNearest = TRUE, 
             selectedBy = "type.label")


edges <- edgeList[,4:5]; names(edges) <- c("from", "to")
nodes <- data.frame(id = nodeList[,1])

library(visNetwork)
visNetwork(nodes, edges) %>%
  visOptions(highlightNearest = TRUE)

###

library("corrplot")
corrplot(as.matrix(sfs_dtw), is.corr = FALSE, method = "color", type = "upper")

plot(hclust(sfs_dtw, method = "ward.D2"))

library(igraph)

head(get.data.frame(as.matrix(sfs_dtw)))

res_dist <- get_dist(data_test[,-1], stand = TRUE, method = "pearson")
fviz_dist(dist.obj = res_dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Using k-means
fviz_nbclust(data_test[,-1], kmeans, method = "gap_stat")

get_clust_tendency(scale(data_test[,-1]), n = 50,
                   gradient = list(low = "steelblue",  high = "white"))
