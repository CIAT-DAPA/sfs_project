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
  
  mine_1  <- minerva::mine(x = df[,driver], y = df[,"SFS_index"], use = "pairwise.complete.obs")
  mic_1   <- mine_1$MIC
  micr2_1 <- mine_1$`MIC-R2`
  
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
  
  mine_2  <- minerva::mine(x = df_fltr_updt[,driver], y = df_fltr_updt[,"SFS_index"], use = "pairwise.complete.obs")
  mic_2   <- mine_2$MIC
  micr2_2 <- mine_2$`MIC-R2`
  
  gcor         <- rbind(cor1, cor2); rm(cor1, cor2)
  gcor$driver  <- driver
  gcor$rmvOtlr <- c(0, length(unique(as.numeric(otlr$Case))))
  gcor$MIC     <- c(mic_1, mic_2)
  gcor$MIC_R2  <- c(micr2_1, micr2_2)
  
  return(gcor)
  
}
drivers_correlation <- drivers_list %>% purrr::map(function(x) calc_correlation(df = sfs_df, driver = x))
drivers_correlation <- dplyr::bind_rows(drivers_correlation)

# Fitting best curve function
calc_best_curve <- function(df         = sfs_df,
                            driver     = drivers_list[1],
                            label      = drivers_lbls$label[1],
                            ft         = c("poly", "gam"),
                            order      = 2,
                            remove_min = T,
                            remove_obs = c("start", "end", "both", NA),
                            number_obs = 3)
{
  
  df_fltr             <- df %>% dplyr::select("iso3c", driver, "SFS_index") %>% tidyr::drop_na()
  names(df_fltr)[2:3] <- c("x","y")
  countryNames        <- df_fltr$iso3c %>% as.character
  
  cat(paste0(">>> 1. Identifying bivariate outliers for: ", driver, " and removing them ...\n"))
  
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
  df_fltr_updt <- df_fltr_updt %>% dplyr::arrange(x)
  if(is.na(remove_obs)){
    
    jcknf_smpl <- lapply(1:nrow(df_fltr_updt), function(i){
      df_flt    <- df_fltr_updt[-i,]
      if(ft == "poly"){
        fit <- lm(y ~ poly(x, order), data = df_flt)
      } else {
        if(ft == "gam"){
          fit <- mgcv::gam(y ~ s(x), data = df_flt, method = "REML")
        }
      }
      df_flt$y  <- fit$fitted.values
      df_flt$id <- i
      return(df_flt)
    })
    
  } else {
    if(remove_obs == "start"){
      
      jcknf_smpl <- lapply((number_obs+1):nrow(df_fltr_updt), function(i){
        df_flt    <- df_fltr_updt[-c(1:number_obs,i),]
        if(ft == "poly"){
          fit <- lm(y ~ poly(x, order), data = df_flt)
        } else {
          if(ft == "gam"){
            fit <- mgcv::gam(y ~ s(x), data = df_flt, method = "REML")
          }
        }
        df_flt$y  <- fit$fitted.values
        df_prd    <- df_fltr_updt[1:number_obs,]
        df_prd$y  <- predict(fit, df_fltr_updt[1:number_obs,])
        df_flt    <- rbind(df_flt, df_prd)
        df_flt$id <- i
        return(df_flt)
      })
      
    } else {
      if(remove_obs == "end"){
        
        jcknf_smpl <- lapply(1:(nrow(df_fltr_updt)-number_obs), function(i){
          df_flt    <- df_fltr_updt[-c(i,(nrow(df_fltr_updt)-number_obs+1):nrow(df_fltr_updt)),]
          if(ft == "poly"){
            fit <- lm(y ~ poly(x, order), data = df_flt)
          } else {
            if(ft == "gam"){
              fit <- mgcv::gam(y ~ s(x), data = df_flt, method = "REML")
            }
          }
          df_flt$y  <- fit$fitted.values
          df_prd    <- df_fltr_updt[(nrow(df_fltr_updt)-number_obs+1):nrow(df_fltr_updt),]
          df_prd$y  <- predict(fit, df_fltr_updt[(nrow(df_fltr_updt)-number_obs+1):nrow(df_fltr_updt),])
          df_flt    <- rbind(df_flt, df_prd)
          df_flt$id <- i
          return(df_flt)
        })
        
      } else {
        if(remove_obs == "both"){
          
          jcknf_smpl <- lapply((number_obs+1):(nrow(df_fltr_updt)-number_obs), function(i){
            df_flt    <- df_fltr_updt[-c(1:number_obs,i,(nrow(df_fltr_updt)-number_obs+1):nrow(df_fltr_updt)),]
            if(ft == "poly"){
              fit <- lm(y ~ poly(x, order), data = df_flt)
            } else {
              if(ft == "gam"){
                fit <- mgcv::gam(y ~ s(x), data = df_flt, method = "REML")
              }
            }
            df_flt$y  <- fit$fitted.values
            df_prd    <- df_fltr_updt[c(1:number_obs,(nrow(df_fltr_updt)-number_obs+1):nrow(df_fltr_updt)),]
            df_prd$y  <- predict(fit, df_fltr_updt[c(1:number_obs,(nrow(df_fltr_updt)-number_obs+1):nrow(df_fltr_updt)),])
            df_flt    <- rbind(df_flt, df_prd)
            df_flt$id <- i
            return(df_flt)
          })
          
        }
      }
    }
  }
  jcknf_smpl <- do.call(rbind, jcknf_smpl)
  
  gp2 <- jcknf_smpl %>%
    ggplot2::ggplot(aes(x, y, group = id)) +
    ggplot2::geom_line(alpha = 0.5, colour = "dodgerblue2") +
    ggplot2::geom_point(data = df_fltr_updt %>% mutate(id = 0), aes(x, y)) +
    ggplot2::xlab(label) +
    ggplot2::ylab("Country food system sustainability scores") +
    ggplot2::ylim(.20, .80) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = element_blank()) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title   = element_text(size = 15),
                   axis.text    = element_text(size = 13),
                   legend.title = element_blank(),
                   legend.text  = element_text(size = 13))
  outfile <- paste0(data_path,"/drivers_graphs/correlations/v4/SFS_index_vs_",driver,"_removing_outliers_",ft,".png")
  if(!file.exists(outfile)){
    ggplot2::ggsave(filename = outfile, plot = gp2, device = "png", units = "in", width = 8, height = 8)
  }
  
  smmry <- tibble::tibble(driver   = driver,
                          outliers = outliers_id %>% list,
                          graph2   = gp2 %>% list)
  
  return(smmry)
  
}

# Change in merchandise and services trade
calc_best_curve(df         = sfs_df,
                driver     = drivers_list[22],
                label      = drivers_lbls$label[22],
                ft         = "gam",
                order      = NA,
                remove_min = T,
                remove_obs = NA,
                number_obs = NA)

# Change in annual population growth
calc_best_curve(df         = sfs_df,
                driver     = drivers_list[2],
                label      = drivers_lbls$label[2],
                ft         = "poly",
                order      = 2,
                remove_min = F,
                remove_obs = NA,
                number_obs = NA)

# Change over time in annual GDP
calc_best_curve(df         = sfs_df,
                driver     = drivers_list[3],
                label      = drivers_lbls$label[3],
                ft         = "poly",
                order      = 2,
                remove_min = F,
                remove_obs = NA,
                number_obs = NA)

# Change over time in agricultural area
calc_best_curve(df         = sfs_df,
                driver     = drivers_list[13],
                label      = drivers_lbls$label[13],
                ft         = "poly",
                order      = 2,
                remove_min = T,
                remove_obs = NA,
                number_obs = NA)

# Change over time in mobile cellular subscriptions
calc_best_curve(df         = sfs_df,
                driver     = drivers_list[15],
                label      = drivers_lbls$label[15],
                ft         = "gam",
                order      = 2,
                remove_min = F,
                remove_obs = NA,
                number_obs = NA)

# Change over time in female employment in services
calc_best_curve(df         = sfs_df,
                driver     = drivers_list[7],
                label      = drivers_lbls$label[7],
                ft         = "poly",
                order      = 2,
                remove_min = T,
                remove_obs = "start",
                number_obs = 3)

# Change in urban population
calc_best_curve(df         = sfs_df,
                driver     = drivers_list[4],
                label      = drivers_lbls$label[4],
                ft         = "poly",
                order      = 2,
                remove_min = F,
                remove_obs = "end",
                number_obs = 5)

# GDP per capita
gdp <- read.csv(paste0(data_path,"/inputs_raw/gdp_per_capita_2010_us_dollars.csv"))
colnames(gdp) <- gsub("X", "Y", colnames(gdp))

dplyr::left_join(sfs_df, data.frame(iso3c = gdp$iso3c, gdp = gdp$Y2017), by = "iso3c")

just_points <- function(df = df, driver = "gdp", label = label){
  
  df_fltr             <- df %>% dplyr::select("iso3c", driver, "SFS_index") %>% tidyr::drop_na()
  names(df_fltr)[2:3] <- c("x","y")
  
  gp2 <- df_fltr %>%
    ggplot2::ggplot(aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::xlab(label) +
    ggplot2::ylab("Country food system sustainability scores") +
    ggplot2::ylim(.20, .80) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = element_blank()) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title   = element_text(size = 15),
                   axis.text    = element_text(size = 13),
                   legend.title = element_blank(),
                   legend.text  = element_text(size = 13))
  outfile <- paste0(data_path,"/drivers_graphs/correlations/v4/SFS_index_vs_",driver,".png")
  if(!file.exists(outfile)){
    ggplot2::ggsave(filename = outfile, plot = gp2, device = "png", units = "in", width = 8, height = 8)
  }
}

just_points(df         = dplyr::left_join(sfs_df, data.frame(iso3c = gdp$iso3c, gdp = gdp$Y2017), by = "iso3c"),
            driver     = "gdp",
            label      = "GDP per capita 2017 (constant 2010 US$)")
just_points(df         = dplyr::left_join(sfs_df, data.frame(iso3c = gdp$iso3c, gdp = gdp %>% dplyr::select(Y2004:Y2015) %>% apply(., 1, median, na.rm = T)), by = "iso3c"),
            driver     = "gdp",
            label      = "GDP per capita 2004-2015 (constant 2010 US$)")

