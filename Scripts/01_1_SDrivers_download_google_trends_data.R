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
saveRDS(allGTrends, paste0("./drivers_CB/Databases_modified/Demand_Consumer/Final/Google_trends_data.rds"))

allGTrends <- readRDS("./drivers_CB/Databases_modified/Demand_Consumer/Final/Google_trends_data.rds")

# traditional version
allGTrends2 <- allGTrends
allGTrends2 <- allGTrends2[complete.cases(allGTrends2),]

allGTrends3 <- allGTrends2 %>%
  select(keyword, geo) %>%
  unique %>%
  as.tibble

allGTrends3 <- allGTrends3 %>% dplyr::mutate(slope = lapply(1:nrow(allGTrends3), function(i){
  
  db_flt <- allGTrends %>% filter(keyword == allGTrends3$keyword[i] & geo == allGTrends3$geo[i])
  TS <- db_flt$hits %>%
    as.numeric %>%
    na.omit %>%
    ts(.,
       start     = c(lubridate::year(db_flt$date)[1],
                     lubridate::month(db_flt$date)[1]),
       end       = c(lubridate::year(db_flt$date)[length(db_flt$date)],
                     lubridate::month(db_flt$date)[length(db_flt$date)]),
       frequency = 12)
  
  slope <- trend::sens.slope(x = TS)
  slope <- slope$estimates
  
  return(slope)
  
}) %>% unlist)
rm(allGTrends2)

gTrends <- left_join(x = countries_languages %>% select(iso2c, iso3c), y = allGTrends3, by = c("iso2c" = "geo")) %>%
  select(iso3c, keyword, slope)
gTrends$keyword <- gTrends$keyword %>% factor
levels(gTrends$keyword) <- c("healthy diet", "organic food", "organic food", "organic food", "junk food",
                             "junk food", "organic food", "healthy diet", "healthy diet", "healthy diet",
                             "healthy diet", "healthy diet", "healthy diet", "junk food", "junk food",
                             "obesity", "obesity", "obesity", "obesity", "obesity", "obesity",
                             "organic food", "obesity")
gTrends <- gTrends %>% tidyr::spread(key = keyword, value = slope)
gTrends$`<NA>` <- NULL
gTrends[is.na(gTrends)] <- 0

gTrends[,-1] %>% cor(method = "spearman") %>% corrplot::corrplot(method = "square")
gTrends[,-1] %>% FactoMineR::PCA(scale.unit = T, graph = T)

write.csv(gTrends, "./drivers_CB/Databases_modified/Demand_Consumer/Final/Change_google_trends.csv", row.names = F)
