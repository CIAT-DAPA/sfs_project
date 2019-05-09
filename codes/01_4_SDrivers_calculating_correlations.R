# Spearman correlation among SFS indices and drivers: SFS project
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

# Load SFS indices and drivers
drivers   <- read.csv(paste0(data_path,"/outputs/drivers/sfs_drivers.csv"), row.names = 1)
sfs_index <- read.csv(paste0(data_path,"/outputs/indicators/sfs_final_index.csv"), row.names = 1)

# Drivers list
drivers_list <- names(drivers)[-1]
drivers_lbls <- read.csv(paste0(data_path,"/inputs_raw/drivers_labels.csv"), stringsAsFactors = F)

# Merge indices and drivers
sfs_df <- dplyr::left_join(x = sfs_index, y = drivers, by = "iso3c")
rownames(sfs_df) <- rownames(sfs_index); rm(sfs_index, drivers)

calc_correlation <- function(df = sfs_df, driver = drivers_list[1]){
  
  cat(paste0("1. Calculating correlation with outliers for: ", driver, "\n"))
  cor1 <- broom::tidy(cor.test(x = df[,driver], y = df[,"SFS_index"], method = "spearman", use = "pairwise.complete.obs"))
  
  cat(paste0("2. Identifying bivariate outliers for: ", driver, " and removing them\n"))
  df_fltr <- df %>% dplyr::select("iso3c", driver, "SFS_index") %>% tidyr::drop_na()
  countryNames <- df_fltr$iso3c %>% as.character
  
  # Try different outliers detection methods
  O3s  <- df_fltr[,c(driver,"SFS_index")] %>% OutliersO3::O3prep(method = c("HDo", "PCS", "BAC", "adjOut", "DDC", "MCD"), tols = .1, boxplotLimits = 6)
  O3s1 <- OutliersO3::O3plotM(O3s, caseNames = countryNames)
  otlr <- O3s1$outsTable
  if(length(grep(pattern = "0", x = otlr$Combination)) > 0){
    otlr <- otlr[-grep(pattern = "0", x = otlr$Combination),]
  }
  df_fltr_updt <- df_fltr[-unique(as.numeric(otlr$Case)),]
  
  cat(paste0("3. Calculating correlation removing outliers for: ", driver, "\n"))
  cor2 <- broom::tidy(cor.test(x = df_fltr_updt[,driver], y = df_fltr_updt[,"SFS_index"], method = "spearman", use = "pairwise.complete.obs"))
  
  gcor         <- rbind(cor1, cor2); rm(cor1, cor2)
  gcor$driver  <- driver
  gcor$rmvOtlr <- c(0, length(unique(as.numeric(otlr$Case))))
  
  return(gcor)
  
}
drivers_correlation <- drivers_list %>% purrr::map(function(x) calc_correlation(df = sfs_df, driver = x))
drivers_correlation <- dplyr::bind_rows(drivers_correlation)

# Version 1.0
calc_best_curve <- function(df = sfs_df, driver = drivers_list[1], label = drivers_lbls$label[1]){
  
  df_fltr <- df %>% dplyr::select("iso3c", driver, "SFS_index") %>% tidyr::drop_na()
  names(df_fltr)[2:3] <- c("x","y")
  
  # Graph with outliers
  gp1 <- df_fltr %>%
    ggplot2::ggplot(aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = T, method = "gam", formula = y ~ s(x)) +
    ggplot2::xlab(label) +
    ggplot2::ylab("Country food system sustainability scores") +
    ggplot2::ylim(.23, .75) +
    ggplot2::theme(legend.title = element_blank()) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title   = element_text(size = 20),
                   axis.text    = element_text(size = 15),
                   legend.title = element_blank(),
                   legend.text  = element_text(size = 15))
  outfile <- paste0(data_path,"/drivers_graphs/correlations/SFS_index_vs_",driver,".png")
  if(!file.exists(outfile)){
    ggplot2::ggsave(filename = outfile, plot = gp1, device = "png", units = "in", width = 8, height = 8)
  }
  
  countryNames <- df_fltr$iso3c %>% as.character
  
  cat(paste0("2. Identifying bivariate outliers for: ", driver, " and removing them\n"))
  # Try different outliers detection methods
  O3s  <- df_fltr[,c("x","y")] %>% OutliersO3::O3prep(method = c("HDo", "PCS", "BAC", "adjOut", "DDC", "MCD"), tols = .1, boxplotLimits = 6)
  O3s1 <- OutliersO3::O3plotM(O3s, caseNames = countryNames)
  otlr <- O3s1$outsTable
  if(length(grep(pattern = "0", x = otlr$Combination)) > 0){
    otlr <- otlr[-grep(pattern = "0", x = otlr$Combination),]
  }
  outliers_id  <- data.frame(iso3c     = otlr$Name %>% unique,
                             x         = df_fltr$x[otlr$Case %>% unique],
                             SFS_index = df_fltr$y[otlr$Case %>% unique])
  names(outliers_id)[2] <- driver
  df_fltr_updt <- df_fltr[-unique(as.numeric(otlr$Case)),]
  
  gp2 <- df_fltr_updt %>%
    ggplot2::ggplot(aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = T, method = "gam", formula = y ~ s(x)) +
    ggplot2::xlab(label) +
    ggplot2::ylab("Country food system sustainability scores") +
    ggplot2::ylim(.23, .75) +
    ggplot2::theme(legend.title = element_blank()) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title   = element_text(size = 20),
                   axis.text    = element_text(size = 15),
                   legend.title = element_blank(),
                   legend.text  = element_text(size = 15))
  outfile <- paste0(data_path,"/drivers_graphs/correlations/SFS_index_vs_",driver,"_removing_outliers.png")
  if(!file.exists(outfile)){
    ggplot2::ggsave(filename = outfile, plot = gp2, device = "png", units = "in", width = 8, height = 8)
  }
  
  smmry <- tibble::tibble(driver   = driver,
                          graph1   = gp1 %>% list,
                          outliers = outliers_id %>% list,
                          graph2   = gp2 %>% list)
  
  return(smmry)
  
}
drivers_best_curve <- 1:length(drivers_list) %>% purrr::map(function(i) calc_best_curve(df     = sfs_df,
                                                                                        driver = drivers_list[i],
                                                                                        label  = drivers_lbls$label[i]))
drivers_best_curve <- dplyr::bind_rows(drivers_best_curve)

# Version 2.0: GAM curves
calc_best_curve <- function(df = sfs_df, driver = drivers_list[1], label = drivers_lbls$label[1], remove_min = T){
  
  df_fltr <- df %>% dplyr::select("iso3c", driver, "SFS_index") %>% tidyr::drop_na()
  names(df_fltr)[2:3] <- c("x","y")
  
  jcknf_smpl <- lapply(1:nrow(df_fltr), function(i){
    df_flt    <- df_fltr[-i,]
    fit       <- mgcv::gam(y ~ s(x), data = df_flt, method = "REML")
    df_flt$y  <- fit$fitted.values
    df_flt$id <- i
    return(df_flt)
  })
  jcknf_smpl <- do.call(rbind, jcknf_smpl)
  
  # Graph with outliers
  gp1 <- jcknf_smpl %>%
    ggplot2::ggplot(aes(x, y, group = id)) +
    ggplot2::geom_line(alpha = I(1/sqrt(nrow(df_fltr))), colour = "dodgerblue2") +
    ggplot2::geom_point(data = df_fltr %>% mutate(id = 0), aes(x, y)) +
    ggplot2::xlab(label) +
    ggplot2::ylab("Country food system sustainability scores") +
    ggplot2::ylim(.23, .75) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = element_blank()) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title   = element_text(size = 15),
                   axis.text    = element_text(size = 13),
                   legend.title = element_blank(),
                   legend.text  = element_text(size = 13))
  # outfile <- paste0(data_path,"/drivers_graphs/correlations/SFS_index_vs_",driver,".png")
  # if(!file.exists(outfile)){
  #   ggplot2::ggsave(filename = outfile, plot = gp1, device = "png", units = "in", width = 8, height = 8)
  # }
  
  countryNames <- df_fltr$iso3c %>% as.character
  
  cat(paste0("2. Identifying bivariate outliers for: ", driver, " and removing them\n"))
  # Try different outliers detection methods
  O3s  <- df_fltr[,c("x","y")] %>% OutliersO3::O3prep(method = c("HDo", "PCS", "BAC", "adjOut", "DDC", "MCD"), tols = .1, boxplotLimits = 6)
  O3s1 <- OutliersO3::O3plotM(O3s, caseNames = countryNames)
  otlr <- O3s1$outsTable
  if(length(grep(pattern = "0", x = otlr$Combination)) > 0){
    otlr <- otlr[-grep(pattern = "0", x = otlr$Combination),]
  }
  outliers_id  <- data.frame(iso3c     = otlr$Name %>% unique,
                             x         = df_fltr$x[otlr$Case %>% unique],
                             SFS_index = df_fltr$y[otlr$Case %>% unique])
  names(outliers_id)[2] <- driver
  df_fltr_updt <- df_fltr[-unique(as.numeric(otlr$Case)),]
  if(remove_min){
    df_fltr_updt <- df_fltr_updt[-which.min(df_fltr_updt$x),]
  }
  
  jcknf_smpl <- lapply(1:nrow(df_fltr_updt), function(i){
    df_flt    <- df_fltr_updt[-i,]
    fit       <- mgcv::gam(y ~ s(x), data = df_flt, method = "REML")
    df_flt$y  <- fit$fitted.values
    df_flt$id <- i
    return(df_flt)
  })
  jcknf_smpl <- do.call(rbind, jcknf_smpl)
  
  gp2 <- jcknf_smpl %>%
    ggplot2::ggplot(aes(x, y, group = id)) +
    ggplot2::geom_line(alpha = I(1/sqrt(nrow(df_fltr_updt))), colour = "dodgerblue2") +
    ggplot2::geom_point(data = df_fltr_updt %>% mutate(id = 0), aes(x, y)) +
    ggplot2::xlab(label) +
    ggplot2::ylab("Country food system sustainability scores") +
    ggplot2::ylim(.23, .75) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = element_blank()) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title   = element_text(size = 15),
                   axis.text    = element_text(size = 13),
                   legend.title = element_blank(),
                   legend.text  = element_text(size = 13))
  outfile <- paste0(data_path,"/drivers_graphs/correlations/SFS_index_vs_",driver,"_removing_outliers.png")
  if(!file.exists(outfile)){
    ggplot2::ggsave(filename = outfile, plot = gp2, device = "png", units = "in", width = 8, height = 8)
  }
  
  smmry <- tibble::tibble(driver   = driver,
                          graph1   = gp1 %>% list,
                          outliers = outliers_id %>% list,
                          graph2   = gp2 %>% list)
  
  return(smmry)
  
}
drivers_best_curve <- 1:length(drivers_list) %>% purrr::map(function(i) calc_best_curve(df     = sfs_df,
                                                                                        driver = drivers_list[i],
                                                                                        label  = drivers_lbls$label[i],
                                                                                        remove_min = F))
drivers_best_curve <- dplyr::bind_rows(drivers_best_curve)

# Version 2.1: Polynomial curves
calc_best_curve_poly <- function(df = sfs_df, driver = drivers_list[1], label = drivers_lbls$label[1], remove_min = T){
  
  df_fltr <- df %>% dplyr::select("iso3c", driver, "SFS_index") %>% tidyr::drop_na()
  names(df_fltr)[2:3] <- c("x","y")
  
  jcknf_smpl <- lapply(1:nrow(df_fltr), function(i){
    df_flt    <- df_fltr[-i,]
    fit       <- lm(y ~ poly(x, 2), data = df_flt)
    df_flt$y  <- fit$fitted.values
    df_flt$id <- i
    return(df_flt)
  })
  jcknf_smpl <- do.call(rbind, jcknf_smpl)
  
  # Graph with outliers
  gp1 <- jcknf_smpl %>%
    ggplot2::ggplot(aes(x, y, group = id)) +
    ggplot2::geom_line(alpha = I(1/sqrt(nrow(df_fltr))), colour = "dodgerblue2") +
    ggplot2::geom_point(data = df_fltr %>% mutate(id = 0), aes(x, y)) +
    ggplot2::xlab(label) +
    ggplot2::ylab("Country food system sustainability scores") +
    ggplot2::ylim(.23, .75) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = element_blank()) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title   = element_text(size = 15),
                   axis.text    = element_text(size = 13),
                   legend.title = element_blank(),
                   legend.text  = element_text(size = 13))
  # outfile <- paste0(data_path,"/drivers_graphs/correlations/SFS_index_vs_",driver,"_poly.png")
  # if(!file.exists(outfile)){
  #   ggplot2::ggsave(filename = outfile, plot = gp1, device = "png", units = "in", width = 8, height = 8)
  # }
  
  countryNames <- df_fltr$iso3c %>% as.character
  
  cat(paste0("2. Identifying bivariate outliers for: ", driver, " and removing them\n"))
  # Try different outliers detection methods
  O3s  <- df_fltr[,c("x","y")] %>% OutliersO3::O3prep(method = c("HDo", "PCS", "BAC", "adjOut", "DDC", "MCD"), tols = .1, boxplotLimits = 6)
  O3s1 <- OutliersO3::O3plotM(O3s, caseNames = countryNames)
  otlr <- O3s1$outsTable
  if(length(grep(pattern = "0", x = otlr$Combination)) > 0){
    otlr <- otlr[-grep(pattern = "0", x = otlr$Combination),]
  }
  outliers_id  <- data.frame(iso3c     = otlr$Name %>% unique,
                             x         = df_fltr$x[otlr$Case %>% unique],
                             SFS_index = df_fltr$y[otlr$Case %>% unique])
  names(outliers_id)[2] <- driver
  df_fltr_updt <- df_fltr[-unique(as.numeric(otlr$Case)),]
  if(remove_min){
    df_fltr_updt <- df_fltr_updt[-which.min(df_fltr_updt$x),]
  }
  
  jcknf_smpl <- lapply(1:nrow(df_fltr_updt), function(i){
    df_flt    <- df_fltr_updt[-i,]
    fit       <- lm(y ~ poly(x, 2), data = df_flt)
    df_flt$y  <- fit$fitted.values
    df_flt$id <- i
    return(df_flt)
  })
  jcknf_smpl <- do.call(rbind, jcknf_smpl)
  
  gp2 <- jcknf_smpl %>%
    ggplot2::ggplot(aes(x, y, group = id)) +
    ggplot2::geom_line(alpha = I(1/sqrt(nrow(df_fltr_updt))), colour = "dodgerblue2") +
    ggplot2::geom_point(data = df_fltr_updt %>% mutate(id = 0), aes(x, y)) +
    ggplot2::xlab(label) +
    ggplot2::ylab("Country food system sustainability scores") +
    ggplot2::ylim(.23, .75) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = element_blank()) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title   = element_text(size = 15),
                   axis.text    = element_text(size = 13),
                   legend.title = element_blank(),
                   legend.text  = element_text(size = 13))
  outfile <- paste0(data_path,"/drivers_graphs/correlations/SFS_index_vs_",driver,"_removing_outliers_poly.png")
  if(!file.exists(outfile)){
    ggplot2::ggsave(filename = outfile, plot = gp2, device = "png", units = "in", width = 8, height = 8)
  }
  
  smmry <- tibble::tibble(driver   = driver,
                          graph1   = gp1 %>% list,
                          outliers = outliers_id %>% list,
                          graph2   = gp2 %>% list)
  
  return(smmry)
  
}

# Special cases
# GAM fit
drivers_best_curve_g <- c(7,13,22) %>% purrr::map(function(i) calc_best_curve(df         = sfs_df,
                                                                              driver     = drivers_list[i],
                                                                              label      = drivers_lbls$label[i],
                                                                              remove_min = T))
# Polynomial fit
drivers_best_curve_p <- c(7,13,22) %>% purrr::map(function(i) calc_best_curve_poly(df         = sfs_df,
                                                                                   driver     = drivers_list[i],
                                                                                   label      = drivers_lbls$label[i],
                                                                                   remove_min = T))

drivers_best_curve_p <- c(2,3,4) %>% purrr::map(function(i) calc_best_curve_poly(df         = sfs_df,
                                                                                 driver     = drivers_list[i],
                                                                                 label      = drivers_lbls$label[i],
                                                                                 remove_min = F))
