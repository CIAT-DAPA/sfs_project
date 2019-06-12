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
all_data <- read.csv(paste0(data_path,"/outputs/indicators/sfs_raw_indicators_scales_adjusted.csv"), row.names = 1)

# Load edge's frontier
frontier_df <- readRDS(paste0(data_path,"/outputs/indicators/edge_frontier/all_maximum_combinations_tibble.RDS"))
frontier_df_fltrd <- frontier_df %>% dplyr::filter(max == 1)
frontier_df_fltrd <- frontier_df_fltrd %>% dplyr::arrange(nIndicators); rm(frontier_df)

# Plot: All possible combinations
if(!file.exists(paste0(data_path,"/outputs/indicators/edge_frontier/all_possible_combinations.pdf"))){
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
    ggsave(filename = paste0(data_path,"/outputs/indicators/edge_frontier/all_possible_combinations.pdf"), units = "in", width = 8, height = 8)
}
# Plot: Edge's frontier
if(!file.exists(paste0(data_path,"/outputs/indicators/edge_frontier/countries_vs_indicators.pdf"))){
  frontier_df_fltrd %>%
    ggplot(aes(x = nIndicators, y = nCountries)) + geom_point(size = 4) +
    theme_bw() +
    scale_x_continuous(breaks = 4:27) +
    scale_y_continuous(breaks = seq(0, 170, 25), limits = c(0, 170)) +
    xlab("Number of indicators") +
    ylab("Number of countries with complete data") +
    theme(axis.title = element_text(size = 20),
          axis.text  = element_text(size = 15)) +
    ggsave(filename = paste0(data_path,"/outputs/indicators/edge_frontier/countries_vs_indicators.pdf"), units = "in", width = 8, height = 8)
}

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
sfs_index       <- calc_sfs_index(combList = frontier_df_fltrd$indicators_list[24][[1]], data = all_data, fnt_type = "arithmetic")

# Producing final map
final_map <- function(df = sfs_index, out_name = "sfs_index_map", format = "png"){
  out <- paste0(data_path,"/outputs/indicators/publish/",out_name,".",format)
  if(!file.exists(out)){
    df$ISO3 <- rownames(df); rownames(df) <- 1:nrow(df)
    suppressMessages(pacman::p_load(sf, spData, tmap, RColorBrewer))
    # Load worldwide shapefile with Robinson projection
    world  <- sf::st_read(paste0(data_path,"/inputs_raw/shapefiles/world_robinson/all_countries_robinson_all.shp"))
    world2 <- dplyr::left_join(x = world, y = df, by = "ISO3")
    # Create the final map
    tsv <- tmap::tm_shape(world2) +
      tmap::tm_polygons(col     = "SFS_index",
                        n       = 20,
                        palette = brewer.pal(n = 20, name = "RdYlBu"),
                        title   = "SFS index",
                        breaks  = seq(0, .95, .05)) + # "RdBu", breaks = seq(0.01, 1, .01)
      tmap::tm_borders("gray20", lwd = .5) +
      tmap::tm_grid(projection = "longlat") +
      tmap::tm_layout(inner.margins = c(0, .02, .02, .02))
    # Save the results
    tmap::tmap_save(tm = tsv, filename = out, width = 16, height = 8, units = "in")
    cat(paste0("Final map was produced. You can find it at: ",out))
  } else {
    stop("Final map already exists!")
  }
}
c("png","pdf") %>% purrr::map(function(f){final_map(df = sfs_index, out_name = "sfs_index_map", format = f)})

# Producing reference map for 16 countries
c("png","pdf") %>% purrr::map(function(f){final_map(df = calc_sfs_index(combList = frontier_df_fltrd$indicators_list[32][[1]], data = all_data, fnt_type = "arithmetic"), out_name = "sfs_index_map_17countries", format = f)})

# Producing reference map for 164 countries
c("png","pdf") %>% purrr::map(function(f){final_map(df = calc_sfs_index(combList = frontier_df_fltrd$indicators_list[1][[1]], data = all_data, fnt_type = "arithmetic"), out_name = "sfs_index_map_164countries", format = f)})

# Performing sensitivity analysis
combFiles <- list.files(path = paste0(data_path, "/outputs/indicators/edge_frontier/bckwd_combinations"), full.names = T) %>% gtools::mixedsort()
combNames <- list.files(path = paste0(data_path, "/outputs/indicators/edge_frontier/bckwd_combinations"), full.names = F) %>% gtools::mixedsort()
combNames <- combNames %>% strsplit(split = "_") %>% purrr::map(2) %>% unlist() %>% as.numeric()
if(!file.exists(paste0(data_path, "/outputs/indicators/edge_frontier/sensitivity_analysis.rds"))){
  
  sensitivity_results <- lapply(X = 1:length(combFiles), function(i){
    
    db <- readRDS(combFiles[i])
    
    # Verify if the 4 initial indicators are present
    db$matches <- lapply(1:nrow(db), function(j){ sum(frontier_df_fltrd$indicators_list[1][[1]] %in% db$Indicators[[j]]) }) %>% unlist
    db <- db %>% dplyr::filter(matches == 4)
    nInd <- db$nIndicators %>% unique %>% sort
    
    textFile2 <- db$Indicators
    all_combinations2 <- lapply(X = 1:length(textFile2), FUN = function(i){
      
      results <- calc_sfs_index(combList = textFile2[[i]], data = all_data, fnt_type = "arithmetic")
      results$combination <- i
      results$nIndicators <- length(textFile2[[i]])
      results$iso3c <- rownames(results)
      rownames(results) <- 1:nrow(results)
      return(results)
      
    })
    all_combinations2 <- do.call(rbind, all_combinations2)
    
    return(all_combinations2)
  })
  sensitivity_results <- list(sensitivity_results[[1]],
                              do.call(rbind, sensitivity_results[2:3]),
                              sensitivity_results[[4]],
                              do.call(rbind, sensitivity_results[5:7]),
                              do.call(rbind, sensitivity_results[8:10]),
                              sensitivity_results[[11]],
                              do.call(rbind, sensitivity_results[12:14]),
                              sensitivity_results[[15]],
                              sensitivity_results[[16]],
                              sensitivity_results[[17]],
                              sensitivity_results[[18]],
                              sensitivity_results[[19]],
                              sensitivity_results[[20]],
                              sensitivity_results[[21]],
                              sensitivity_results[[22]],
                              sensitivity_results[[23]],
                              sensitivity_results[[24]],
                              sensitivity_results[[25]],
                              sensitivity_results[[26]],
                              sensitivity_results[[27]])
  
  saveRDS(sensitivity_results, paste0(data_path,"/outputs/indicators/edge_frontier/sensitivity_analysis.rds"))
  
} else {
  sensitivity_results <- readRDS(paste0(data_path,"/outputs/indicators/edge_frontier/sensitivity_analysis.rds"))
}

# Plot: Instability issue
if(!file.exists(paste0(data_path,"/outputs/indicators/edge_frontier/instability_issue.png"))){
  
  edge_path <- lapply(1:length(frontier_df_fltrd$indicators_list), function(i){
    tbl <- calc_sfs_index(combList = frontier_df_fltrd$indicators_list[i][[1]], data = all_data, fnt_type = "arithmetic")
    tbl$nIndicators <- length(frontier_df_fltrd$indicators_list[i][[1]])
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
    ylab("Country food system sustainability scores") +
    theme_bw() +
    theme(axis.title = element_text(size = 20),
          axis.text  = element_text(size = 15))
  ggsave(filename = paste0(data_path,"/outputs/indicators/edge_frontier/instability_issue.png"), plot = tsv, device = "png", units = "in", width = 20, height = 8)
  
}
# Plot: Stability after backwards process
if(!file.exists(paste0(data_path,"/outputs/indicators/edge_frontier/backwards_stability.png"))){
  # Example 6 countries
  tsv <- sensitivity_results[[17]] %>% group_by(nIndicators, iso3c) %>%
    summarise(SFS_index = mean(SFS_index)) %>%
    filter(iso3c %in% c("ARG", "COL", "FRA", "USA", "CAN", "VNM")) %>%
    ggplot(aes(x = as.numeric(nIndicators), y = SFS_index)) +
    geom_point() + facet_wrap(~iso3c, ncol = 3) +
    geom_hline(yintercept = 0, color = "red") +
    scale_x_continuous(breaks = 4:27, labels = 4:27) +
    scale_y_continuous(limits = c(0, 1)) +
    xlab("Number of indicators") +
    ylab("Country food system sustainability scores") +
    theme_bw() +
    theme(axis.title = element_text(size = 20),
          axis.text  = element_text(size = 15))
  ggsave(filename = paste0(data_path,"/outputs/indicators/edge_frontier/backwards_stability.png"), plot = tsv, device = "png", units = "in", width = 16, height = 8)
}

# Selecting optimal combination of indicators
indicators_list <- frontier_df_fltrd$indicators_list[c(1,2,4,5,8,11,12,15:27)]
rank_summary <- rep(NA, length(indicators_list))
stdv_summary <- rep(NA, length(indicators_list))
for(i in 1:length(indicators_list)){
  
  ref_vals <- calc_sfs_index(combList = indicators_list[i][[1]], data = all_data, fnt_type = "arithmetic")
  ref_cntr <- calc_sfs_index(combList = indicators_list[i][[1]], data = all_data, fnt_type = "arithmetic") %>% rownames %>% sort
  
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
gnrl_summary <- data.frame(cmbn = 4:23, rank = rank_summary, stdv = stdv_summary)
gnrl_summary <- cbind(gnrl_summary, scale(gnrl_summary[,2:3]))
colnames(gnrl_summary)[4:5] <- c("rank_scaled", "stdv_scaled")
gnrl_summary$nCountries <- frontier_df_fltrd %>%
  dplyr::filter(nIndicators <= 23) %>%
  dplyr::group_by(nIndicators) %>%
  dplyr::summarise(MaxCount = max(nCountries)) %>%
  .$MaxCount
if(!file.exists(paste0(data_path,"/outputs/indicators/edge_frontier/select_best_combination.png"))){
  tsv <- gnrl_summary %>%
    ggplot(aes(x = factor(cmbn), y = rank, fill = "Rank change")) +
    geom_bar(stat = "identity") +
    geom_bar(aes(x = factor(cmbn), y = stdv*1000, fill = "Variability"), alpha = 0.8, stat = "identity") +
    scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Standard deviation")) +
    scale_colour_manual(values = c("blue", "red")) +
    xlab("Number of indicators") +
    ylab("Rank change") +
    guides(alpha = F) +
    theme(legend.title = element_blank()) +
    ggplot2::annotate("text", x = 1:20, y = gnrl_summary$rank + 1, label = lag(x = gnrl_summary$nCountries, n = 1) - gnrl_summary$nCountries, size = 5) +
    theme_bw() +
    theme(axis.title = element_text(size = 20),
          axis.text  = element_text(size = 15),
          legend.title = element_blank(),
          legend.text  = element_text(size = 15))
  ggsave(filename = paste0(data_path,"/outputs/indicators/edge_frontier/select_best_combination.png"), plot = tsv, device = "png", units = "in", width = 12, height = 8)
}
