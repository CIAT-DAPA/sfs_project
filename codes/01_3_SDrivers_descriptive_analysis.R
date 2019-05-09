# Descriptive analysis and scale adjustments: SFS project - sustainability drivers
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

# Load SFS drivers
drivers <- read.csv(paste0(data_path,"/outputs/drivers/sfs_drivers.csv"), row.names = 1)

# Function to calculate descriptive statistics
descriptive_analysis <- function(df = drivers[,-1],
                                 dataset_name = "original",
                                 outdir = paste0(data_path,"/outputs")){
  
  outdir <- paste0(outdir,"/",dataset_name,"_dscrp_stats")
  if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
  
  # Descriptive statistics
  df %>%
    psych::describe() %>%
    write.csv(file = paste0(outdir,"/dscrp_stats_",dataset_name,"_dataset.csv"), row.names = T)
  
  # Correlation analysis
  df %>%
    cor(method = "spearman", use = "pairwise.complete.obs") %>%
    write.csv(file = paste0(outdir,"/spearman_cor_",dataset_name,"_dataset.csv"), row.names = T)
  
  outfile <- paste0(outdir,"/spearman_cor_",dataset_name,"_dataset.png")
  if(!file.exists(outfile)){
    
    png(height = 8, width = 8, res = 300, units = "in", file = outfile)
    par(cex = 1)
    df %>%
      cor(method = "spearman", use = "pairwise.complete.obs") %>%
      corrplot::corrplot.mixed(lower = "number",
                               upper = "square",
                               tl.pos = "lt",
                               cl.cex = par("cex"),
                               tl.cex = par("cex"),
                               number.cex = 0.45,
                               lower.col = "black")
    dev.off()
    
  }
  
  return(cat(paste0("Done. Please check the outputs in: ",outdir,"\n")))
  
}

# Descriptive statistics for original data
descriptive_analysis(df = drivers[,-1],
                     dataset_name = "sfs_drivers_original",
                     outdir = paste0(data_path,"/outputs/drivers"))

# Correct skewness
dscrp_stats <- read.csv(paste0(data_path,"/outputs/drivers/sfs_drivers_original_dscrp_stats/dscrp_stats_sfs_drivers_original_dataset.csv"), row.names = 1)
# Apply Box-Cox transformations to correct skewness
for(i in 1:nrow(dscrp_stats)){
  j <- i + 1
  if(dscrp_stats$skew[i] > 2 | dscrp_stats$skew[i] < -2){
    drivers[,j][which(drivers[,j] == 0)] <- 0.01
    if(sum(na.omit(drivers[,j]) < 0) > 0){
      lambda.bc   <- car::boxCox(drivers[,j]~1, family = "yjPower")
      lambda.max  <- lambda.bc$x[which.max(lambda.bc$y)]
      drivers[,j] <- car::yjPower(drivers[,j], lambda.max, jacobian.adjusted = F)
      rm(lambda.max, lambda.bc)
    } else {
      optPar      <- EnvStats::boxcox(x = drivers[,j], optimize = T)
      drivers[,j] <- EnvStats::boxcoxTransform(x = drivers[,j], lambda = optPar$lambda)
    }
  } else { drivers[,j] <- drivers[,j] }
}; rm(i,j,optPar)

# Descriptive statistics for scale adjusted data
descriptive_analysis(df = drivers[,-1],
                     dataset_name = "sfs_drivers_scales_adjusted",
                     outdir = paste0(data_path,"/outputs/drivers"))

# Save scale adjusted data
if(!file.exists(paste0(data_path,"/outputs/drivers/sfs_drivers_scales_adjusted.csv"))){
  drivers %>%
    write.csv(file = paste0(data_path,"/outputs/drivers/sfs_drivers_scales_adjusted.csv"), row.names = T)
}
rm(drivers, descriptive_analysis, dscrp_stats, data_path)

## PCA for all SFS indicators vs 16 countries
# df <- all_data[complete.cases(all_data),]
# all_pca <- df[,-1] %>% FactoMineR::PCA(scale.unit = T, graph = F)
# all_pca %>% factoextra::fviz_pca_biplot(repel = TRUE) +
#   ggplot2::theme_bw()
