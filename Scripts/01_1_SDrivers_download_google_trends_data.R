# Download Google Trends data: SFS project - drivers
# Implemented by: H. Achicanoy & P. Alvarez
# http://www.loc.gov/standards/iso639-2/php/code_list.php
# CIAT, 2018

# R options
g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)

OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
wk_dir   <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9/Sustainable_Food_System/Drivers", "Windows" = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Drivers")
setwd(wk_dir); rm(wk_dir, OSysPath, OSys)

# Load packages
library(pacman)
pacman::p_load(gtrendsR, tidyverse, future.apply, countrycode)

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

countries_languages <- read.csv("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/sfs_index_languages.csv")
countries_languages <- left_join(x = countries_languages, y = country_codes %>% select(iso3c, iso2c), by = "iso3c")
rm(country_codes)
countries_languages <- countries_languages %>% as.tibble()

## ========================================================================== ##
## Key-terms for searching
## ========================================================================== ##

key_terms <- list(english    = c("healthy diet", "junk food", "obesity", "organic food"),
                  spanish    = c("dieta saludable", "comida chatarra", "obesidad", "alimentos organicos"),
                  french     = c("alimentation saine", "malbouffe", "obesité", "aliments biologiques"),
                  portuguese = c("dieta saudável", "comida não saudável", "obesidade", "comida orgânica"),
                  # vietnamise = c("Khẩu phần ăn lành mạnh", "Đồ ăn vặt", "Béo phì", "Thực phẩm hữu cơ"),
                  # mandarin   = c("健康的饮食", "垃圾食品", "肥胖", "有机食品"),
                  # bahasa     = c("diet sihat", "makanan rigan", "obesity", "makanan organic"),
                  italian    = c("dieta sana", "cibo spazzatura", "obesita", "cibo organico"),
                  german     = c("gesunde ernährung", "junk food", "Übergewicht", "bio lebensmittel"),
                  dutch      = c("gezond voedsel", "junk food", "obesitas", "biologisch voedsel"))

key_terms <- list(english    = c("healthy diet", "junk food", "organic food"),
                  spanish    = c("dieta saludable", "comida chatarra", "alimentos organicos"),
                  french     = c("alimentation saine", "malbouffe", "aliments biologiques"),
                  portuguese = c("dieta saudável", "comida não saudável", "comida orgânica"),
                  italian    = c("dieta sana", "cibo spazzatura", "cibo organico"),
                  german     = c("gesunde ernährung", "junk food", "bio lebensmittel"),
                  dutch      = c("gezond voedsel", "junk food", "biologisch voedsel"))

countries_languages$key_terms <- NA
for(i in 1:nrow(countries_languages)){
  countries_languages$key_terms[i] <- list(key_terms[[which(names(key_terms) == countries_languages$Language[i])]])
}; rm(i)

## ========================================================================== ##
## Basic function to extract time series for each search
## ========================================================================== ##

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

## ========================================================================== ##
## Some examples
## ========================================================================== ##

plot(gtrends(geo = "US", keyword = "healthy diet", hl = "en", low_search_volume = T))
plot(gtrends(geo = "CO", keyword = "dieta saludable", hl = "es", low_search_volume = T))
plot(gtrends(geo = "FR", keyword = "alimentation saine", hl = "fr", low_search_volume = T))
plot(gtrends(geo = "NL", keyword = "gezond voedsel", hl = "nl", low_search_volume = T))
plot(gtrends(geo = "DE", keyword = "gesunde ernährung", hl = "de", low_search_volume = T))
plot(gtrends(geo = "MY", keyword = "diet sihat", hl = "id", low_search_volume = T))
plot(gtrends(geo = "VN", keyword = "Khẩu phần ăn lành mạnh", hl = "vi", low_search_volume = T))

# Calculating annual time series
test <- get_gtrends(code = "US", key = "healthy diet", language = "en")
test$Year <- lubridate::year(test$date)
test$Month <- lubridate::month(test$date)
test2 <- test %>% group_by(Year) %>% summarise(Median = median(hits, na.rm = T))
test2 %>% ggplot(aes(x = Year, y = Median)) + geom_line() + theme_bw()

## ========================================================================== ##
# Download Google Trends data
## ========================================================================== ##

allGTrends <- lapply(1:nrow(countries_languages), function(i){
  
  terms <- lapply(1:length(countries_languages$key_terms[[i]]), function(j){
    
    tryCatch(expr = {
      tbl_gt <- get_gtrends(code     = countries_languages$iso2c[i] %>% as.character,
                            key      = countries_languages$key_terms[[i]][j],
                            language = countries_languages$code[i] %>% as.character)
    },
    error = function(e){
      cat("Download process failed in combination:", i,"\n")
      return("Continue ...\n")
    })
    if(!is.null(tbl_gt)){
      tbl_gt$language <- countries_languages$Language[i] %>% as.character
      return(tbl_gt)
    } else {
      tbl_gt <- data.frame(date     = NA,
                           hits     = NA,
                           keyword  = countries_languages$key_terms[[i]][j],
                           geo      = countries_languages$iso2c[i] %>% as.character,
                           gprop    = "web",
                           category = 0,
                           language = countries_languages$Language[i] %>% as.character)
      return(tbl_gt)
    }
    
  })
  terms <- do.call(rbind, terms)
  return(terms)
})
allGTrends <- allGTrends %>% purrr::map(function(x){
  if(is.numeric(x$date)){
    x$date <- as.Date(x$date, origin = "1970-01-01")
  }; return(x)
})
allGTrends <- do.call(rbind, allGTrends)
allGTrends <- allGTrends[complete.cases(allGTrends),]; rownames(allGTrends) <- 1:nrow(allGTrends)
saveRDS(allGTrends, paste0("./drivers_CB/Databases_modified/Demand_Consumer/Final/Google_trends_complete_ts.rds"))

allGTrends <- readRDS("./drivers_CB/Databases_modified/Demand_Consumer/Final/Google_trends_complete_ts.rds")

cList <- allGTrends$geo %>% as.character %>% unique
ch_diet_health_attn <- lapply(1:length(cList), function(i){
  
  # Individual time series since 2007-01-01
  indv_ts <- allGTrends %>%
    filter(geo == cList[i] & date >= "2007-01-01") %>%
    split(.$keyword)
  
  # Calculating moving average
  mv_av_ts <- indv_ts %>%
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

gTrends <- left_join(x = countries_languages %>%
                       select(iso2c, iso3c),
                     y = data.frame(iso2c = cList, ch_diet_health_attn = ch_diet_health_attn),
                     by = "iso2c") %>%
  select(iso3c, ch_diet_health_attn)
rm(cList, ch_diet_health_attn)
write.csv(gTrends, "./drivers_CB/Databases_modified/Demand_Consumer/Final/Change_google_trends.csv", row.names = F)

# ----------------------------------------------- #
# Test with one country
# ----------------------------------------------- #

# Plotting time series
allGTrends %>%
  filter(geo == "AR") %>%
  ggplot(aes(x = date, y = hits %>% as.numeric, colour = keyword)) +
  geom_line()

# Filtering and splitting time series
ts_country <- allGTrends %>%
  filter(geo == "AR" & date >= "2007-01-01") %>%
  split(.$keyword)

# Calculating moving average per time serie
ma_country <- ts_country %>%
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
    plot(TS)
    lines(trend_ts)
    return(trend_ts)
  })

plot(ma_country[[1]], ylim = c(0, 200), ylab = "Hits") # Black
lines(ma_country[[2]], col = 2) # Red
lines(ma_country[[3]], col = 4) # Blue
lines(ma_country[[4]], col = 3) # Green

total <- ma_country[[1]] %>% as.numeric + ma_country[[2]] %>% as.numeric + ma_country[[3]] %>% as.numeric
ts_total <- ts(total, start = c(2007, 1), end = c(2018, 7), frequency = 12)

lines(ts_total, col = 5)
trend::sens.slope(na.omit(ts_total))
