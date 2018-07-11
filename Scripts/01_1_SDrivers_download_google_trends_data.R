
# Load packages
library(gtrendsR)
library(tidyverse)
library(future.apply)
library(countrycode)

# List of countries ISO2 code
data(countries)
ccode <- countries$country_code %>% unique %>% na.omit %>% as.character

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

countries_languages$key_terms <- NA
for(i in 1:nrow(countries_languages)){
  countries_languages$key_terms[i] <- list(key_terms[[which(names(key_terms) == countries_languages$Language[i])]])
}; rm(i)

# http://www.loc.gov/standards/iso639-2/php/code_list.php

# List of keyterms for searching
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

# iconv(countries$COUNTRY, from = "UTF-8", to = "latin1")

# List of languages
# lngg_list <- c("en", "es", "fr", "pt", "vi", "zh", "id", "it", "de", "nl")

# Basic function to extract time series for each search
get_gtrends <- function(code = "US", key = "healthy diet", language = "en"){
  
  df <- gtrendsR::gtrends(keyword = key,
                          geo = code,
                          gprop = "web",
                          hl = language,
                          low_search_volume = T)
  df <- df$interest_over_time
  return(df)
  
}

# Explore some results
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

# Create all possible combinations
combinations <- lapply(1:length(key_terms), function(i){
  tbl <- expand.grid(ccode, key_terms[[i]], lngg_list) %>% as.data.frame
  return(tbl)
})
combinations <- do.call(rbind, combinations)
combinations$Var1 <- as.character(combinations$Var1)
combinations$Var2 <- as.character(combinations$Var2)
combinations$Var3 <- as.character(combinations$Var3)

# Download data from all possible combinations
allGTrends <- lapply(1:nrow(countries_languages), function(i){
  
  tryCatch(expr = {
    tbl_gt <- get_gtrends(code     = countries_languages$iso2c[i] %>% as.character,
                          key      = countries_languages$key_terms[[i]],
                          language = countries_languages$code[i] %>% as.character)
  },
  error = function(e){
    cat("Download process failed in combination:", i,"\n")
    return("Continue ...\n")
  })
  if(exists("tbl_gt")){
    tbl_gt$language <- countries_languages$Language[i] %>% as.character
    return(tbl_gt)
  } else {
    tbl_gt <- data.frame(date = NA,
                         hits = NA,
                         keyword = NA,
                         geo = countries_languages$iso2c[i] %>% as.character,
                         gprop = "web",
                         category = 0,
                         language = countries_languages$Language[i] %>% as.character)
    return(tbl_gt)
  }
  
})










allGTrends <- future_lapply(1:nrow(combinations), function(i){
  
  tryCatch(expr = {
    tbl_gt <- get_gtrends(code = combinations[i,1], key = combinations[i,2], language = combinations[i,3])
  },
  error = function(e){
    cat("Download process failed in combination:", i,"\n")
    return("Continue ...\n")
  })
  if(exists("tbl_gt")){
    tbl_gt$language <- combinations[i,3]
    return(tbl_gt)
  } else {
    tbl_gt <- data.frame(date = NA, hits = NA, keyword = combinations[i,2], geo = combinations[i,1], gprop = "web", category = 0, language = combinations[i,3])
    return(tbl_gt)
  }
  
})


#############################################################################
#############################################################################
#############################################################################
#############################################################################




countryList <- lapply(1:length(ccode), function(i){ # Country
  
  termsList <- lapply(1:length(key_terms), function(j){ # Language
    
    keyList <- lapply(1:length(key_terms[[j]]), function(k){
      
      lngList <- lapply(1:length(lngg_list), function(l){
        
        tryCatch(expr = {
          df <- get_gtrends(code = ccode[i], key = key_terms[[j]][k], language = lngg_list[l])
          df$language <- lngg_list[k]
        })
        if(exists("df")){
          return(df)
        } else {
          df <- data.frame(date = NA, hits = NA, keyword = key_terms[j], geo = ccode[i], gprop = "web", category = 0, language = lngg_list[k])
          return(df)
        }
        
      })
      lngList <- do.call(rbind, lngList)
      return(lngList)
    })
    keyList <- do.call(rbind, keyList)
    return(keyList)
  })
  termsList <- do.call(rbind, termsList)
  return(termsList)
})
countryList <- do.call(rbind, countryList)














wrld_info <- lapply(X = 1:length(ccode), function(i){
  
  key_info <- lapply(1:length(key_terms), function(j){
    
    language_info <- lapply(1:length(lngg_list), function(k){
      
      tryCatch(expr = {
        df <- get_gtrends(code = ccode[i], key = key_terms[j], language = lngg_list[k])
        df$language <- lngg_list[k]
      })
      if(exists("df")){
        return(df)
      } else {
        df <- data.frame(date = NA, hits = NA, keyword = key_terms[j], geo = ccode[i], gprop = "web", category = 0, language = lngg_list[k])
      }
      
    })
    language_info <- do.call(rbind, language_info)
    return(language_info)
    
  })
  key_info <- do.call(rbind, key_info)
  return(key_info)
  
})




gt.df <- gtrends(keyword = c("健康的饮食", "dieta saludable", "healthy diet", "régime équilibrét"), 
                        geo = "USA", gprop = "web", time = "2004-01-01 2017-12-31")[[1]]

gtrends(keyword = "healthy diet", geo = "FR", gprop = "web", hl = "fr", time = "2004-01-01 2017-12-31")$interest_over_time
gtrends(keyword = "healthy diet", geo = "US", gprop = "web", hl = "fr", time = "2004-01-01 2017-12-31")$interest_over_time
us_info <- gtrends(keyword = "healthy diet", geo = "US", gprop = "web", hl = "en-US", time = "2004-01-01 2017-12-31")$interest_over_time
us_info %>% ggplot(aes(x = date, y = hits)) + geom_line()
us_info %>% ggplot(aes(x = hits)) + geom_histogram(bins = 15)
summary(us_info)
uk_info <- gtrends(keyword = "healthy diet", geo = "GB", gprop = "web", hl = "en-US", time = "2004-01-01 2017-12-31")$interest_over_time
uk_info %>% ggplot(aes(x = date, y = hits)) + geom_line()
uk_info %>% ggplot(aes(x = hits)) + geom_histogram(bins = 15)
summary(uk_info)

# problem with the languages in this package
google.trends = gtrends(c("healthy diet"), 
                        geo =c('US','CO','DE','FR' ), gprop = "web", hl = "fr", time = "2004-01-01 2016-01-01")[[1]]

gt2 = dcast(google.trends, date ~ keyword + geo, value.var = "hits")
rownames(gt2) = gt2$date

par(mfrow=c(3,2))

for(i in 2:ncol(gt2)){
  plot(gt2$date,gt2[,i],type='l',xlab=names(gt2)[1],ylab=names(gt2)[i])
}


quantile(...)
sum(table(dist2$DTWarp[quantile(...)])) > 10



