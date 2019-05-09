# Processing Growing concerns for food safety (Google trends data): SFS project - drivers
# Implemented by: H. Achicanoy & P. Alvarez
# http://www.loc.gov/standards/iso639-2/php/code_list.php
# CIAT, 2019

# R options
g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)

OSys <- Sys.info()[1]
# Organizar esta parte
wk_dir   <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9/Sustainable_Food_System/Drivers", "Windows" = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Drivers")
setwd(wk_dir); rm(wk_dir, OSys)

# Load packages
library(pacman)
pacman::p_load(gtrendsR, tidyverse, future.apply, countrycode)

# Define data directory
data_path <- "D:/ToBackup/sustainable_food_systems/sfs_repo/data"

gcfs_data <- "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Drivers/growing_concerns_food_safety.rds"
gcfs_data <- "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Drivers/Google_trends_complete_italian_corrected.rds"

if(!file.exists(gcfs_data)){
  
  # Load country codes list
  country_codes     <- read.csv(paste0(data_path,"/inputs_raw/country_codes.csv"))
  country_languages <- read.csv(paste0(data_path,"/inputs_raw/country_languages.csv"))
  country_languages <- dplyr::left_join(country_languages, country_codes %>% dplyr::select(iso3c, iso2c), by = "iso3c")
  
  # Key-terms for searching
  key_terms <- list(english    = c("unsafe food", "food quality control", "traceability"),
                    spanish    = c("inseguridad alimenticia", "control de calidad de alimentos", "trazabilidad"),
                    french     = c("aliments dangereux", "contrôle de la qualité des produits alimentaires", "traçabilité"),
                    portuguese = c("alimentos não seguros", "controle de qualidade de alimentos", "rastreabilidade"),
                    italian    = c("cibo insicuro", "controllo qualità del cibo", "tracciabilità"),
                    german     = c("risikobehaftete lebensmittel", "lebensmittelqualitätskontrolle", "rückverfolgbarkeit"),
                    dutch      = c("voedselonveiligheid", "voedselkwaliteit controle", "traceerbaarheid"),
                    russian    = c("небезопасная еда", "контроль качества продуктов питания", "прослеживаемость"))
  
  country_languages$key_terms <- NA
  for(i in 1:nrow(country_languages)){
    country_languages$key_terms[i] <- list(key_terms[[which(names(key_terms) == country_languages$Language[i])]])
  }; rm(i, key_terms)
  
  # Basic function to download from Google trends
  get_gtrends <- function(code = "US", key = "healthy diet", language = "en"){
    
    df <- gtrendsR::gtrends(keyword           = key,
                            geo               = code,
                            gprop             = "web",
                            hl                = language,
                            low_search_volume = T,
                            time              = "all")
    df <- df$interest_over_time
    return(df)
    
  }
  # plot(gtrends(geo = "US", keyword = "healthy diet", hl = "en", low_search_volume = T))
  
  # Download Growing concerns for food safety data
  gcfs_trends <- lapply(1:nrow(country_languages), function(i){
    
    terms <- lapply(1:length(country_languages$key_terms[[i]]), function(j){
      
      tryCatch(expr = {
        tbl_gt <- get_gtrends(code     = country_languages$iso2c[i] %>% as.character,
                              key      = country_languages$key_terms[[i]][j],
                              language = country_languages$code[i] %>% as.character)
      },
      error = function(e){
        cat("Download process failed in combination:", i,"\n")
        return("Continue ...\n")
      })
      if(!is.null(tbl_gt)){
        tbl_gt$language <- country_languages$Language[i] %>% as.character
        return(tbl_gt)
      } else {
        tbl_gt <- data.frame(date     = NA,
                             hits     = NA,
                             keyword  = country_languages$key_terms[[i]][j],
                             geo      = country_languages$iso2c[i] %>% as.character,
                             gprop    = "web",
                             category = 0,
                             language = country_languages$Language[i] %>% as.character)
        return(tbl_gt)
      }
      
    })
    terms <- do.call(rbind, terms)
    return(terms)
  })
  gcfs_trends <- gcfs_trends %>% purrr::map(function(x){
    if(is.numeric(x$date)){
      x$date <- as.Date(x$date, origin = "1970-01-01")
    }; return(x)
  })
  gcfs_trends <- do.call(rbind, gcfs_trends)
  gcfs_trends <- gcfs_trends[complete.cases(gcfs_trends),]; rownames(gcfs_trends) <- 1:nrow(gcfs_trends)
  saveRDS(gcfs_trends, "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Drivers/growing_concerns_food_safety.rds")
  
} else {
  gcfs_trends <- readRDS(gcfs_data)
}

gcfs_trends <- readRDS("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Drivers/growing_concerns_food_safety.rds")

traza <- key_terms %>% purrr::map(., function(x){x[3]}) %>% unlist

gcfs_trends2 <- gcfs_trends
gcfs_trends <- gcfs_trends[!(gcfs_trends$keyword %in% traza),]

cList <- gcfs_trends$geo %>% as.character %>% unique
ch_food_safety_cncr <- lapply(1:length(cList), function(i){
  
  # 2008-2009
  # 2016-2017
  
  # Individual time series since 2007-01-01
  indv_ts <- gcfs_trends %>%
    dplyr::filter(geo == cList[i] & date >= "2007-01-01") %>%
    split(.$keyword)
  
  # Calculating moving average
  mv_av_ts <- indv_ts %>%
    purrr::discard(., .p = function(x){nrow(x) <= 0}) %>%
    map(function(X){
      TS <- X$hits %>%
        as.numeric %>%
        na.omit %>%
        ts(.,
           start     = c(lubridate::year(X$date)[1],
                         lubridate::month(X$date)[1]),
           end       = c(lubridate::year(X$date)[length(X$date)],
                         lubridate::month(X$date)[length(X$date)]),
           frequency = 12)
      trend_ts <- forecast::ma(TS, order = 12, centre = T)
      # plot(TS); lines(trend_ts)
      return(trend_ts)
    })
  
  # Calculating aggregate number of hits for healthy diet, junk food and organic food terms
  slope_ts <- mv_av_ts %>%
    purrr::map(as.numeric) %>%
    purrr::reduce(`+`) %>%
    ts(start = c(2007, 1), end = c(2018, 7), frequency = 12) %>%
    na.omit %>%
    trend::sens.slope(.)
  
  # Return slope of Google trends
  return(slope_ts$estimates)
  
}) %>% unlist

food_safety_concern <- dplyr::left_join(x  = country_languages %>% dplyr::select(iso2c, iso3c),
                                        y  = data.frame(iso2c = cList, ch_food_safety_cncr = ch_food_safety_cncr),
                                        by = "iso2c") %>%
  dplyr::select(iso3c, ch_food_safety_cncr)
rm(cList, ch_food_safety_cncr)
write.csv(food_safety_concern, "D:/food_safety_concern.csv", row.names = F)

sfs_index <- read.csv(paste0(data_path,"/outputs/indicators/sfs_final_index.csv"),row.names = 1)
xy <- dplyr::left_join(sfs_index, food_safety_concern, by = "iso3c")

xy %>%
  ggplot(aes(x = ch_food_safety_cncr, y = SFS_index)) +
  geom_point() +
  geom_vline(xintercept = 0, colour = "red")
cor(xy$ch_food_safety_cncr, xy$SFS_index, method = "spearman", use = "pairwise.complete.obs")

write.csv(food_safety_concern, "./drivers_CB/Databases_modified/Demand_Consumer/Final/Change_google_trends.csv", row.names = F)

# ----------------------------------------------- #
# Test with one country
# ----------------------------------------------- #

# Plotting time series
# allGTrends %>%
#   filter(geo == "AR") %>%
#   ggplot(aes(x = date, y = hits %>% as.numeric, colour = keyword)) +
#   geom_line()
# 
# # Filtering and splitting time series
# ts_country <- allGTrends %>%
#   filter(geo == "AR" & date >= "2007-01-01") %>%
#   split(.$keyword)
# 
# # Calculating moving average per time serie
# ma_country <- ts_country %>%
#   map(function(X){
#     TS <- X$hits %>%
#       as.numeric %>%
#       na.omit %>%
#       ts(.,
#          start     = c(lubridate::year(X$date)[1],
#                        lubridate::month(X$date)[1]),
#          end       = c(lubridate::year(X$date)[length(X$date)],
#                        lubridate::month(X$date)[length(X$date)]),
#          frequency = 12)
#     trend_ts <- forecast::ma(TS, order = 12, centre = T)
#     plot(TS)
#     lines(trend_ts)
#     return(trend_ts)
#   })
# 
# plot(ma_country[[1]], ylim = c(0, 200), ylab = "Hits") # Black
# lines(ma_country[[2]], col = 2) # Red
# lines(ma_country[[3]], col = 4) # Blue
# lines(ma_country[[4]], col = 3) # Green
# 
# total <- ma_country[[1]] %>% as.numeric + ma_country[[2]] %>% as.numeric + ma_country[[3]] %>% as.numeric
# ts_total <- ts(total, start = c(2007, 1), end = c(2018, 7), frequency = 12)
# 
# lines(ts_total, col = 5)
# trend::sens.slope(na.omit(ts_total))
