# Processing and integrating data: SFS project auxiliar script
# Implemented by: H. Achicanoy & P. Alvarez
# CIAT, 2017

## ========================================================================== ##
## Parallelization
# library(doParallel)
# 
# #the following line will create a local 4-node snow cluster
# workers = makeCluster(4, type = "SOCK")
# registerDoParallel(workers)
# 
# foreach(i=1:4) %dopar% Sys.getpid()
# stopCluster(workers)
## ========================================================================== ##

## ========================================================================== ##
## Country codes
# # Country codes FAO
# if(!file.exists('./Results/_fixed_data/FAO_ISO3_codes.RDS')){
#   iso3FAO <- readHTMLTable('http://www.fao.org/countryprofiles/iso3list/en/')
#   iso3FAO <- iso3FAO$`NULL`
#   saveRDS(object = iso3FAO, file = './Results/_fixed_data/FAO_ISO3_codes.RDS')
# } else {
#   iso3FAO <- readRDS(file = './Results/_fixed_data/FAO_ISO3_codes.RDS')
#   iso3FAO$`Short name` <- as.character(iso3FAO$`Short name`); iso3FAO$`Short name`[which(iso3FAO$`Short name` == "Côte d'Ivoire")] <- "Ivory Coast"; iso3FAO$`Short name` <- as.factor(iso3FAO$`Short name`)
# }

# # Country codes World Bank
# if(!file.exists('./Results/_fixed_data/WBK_ISO3_codes.RDS')){
#   iso3WBK <- readHTMLTable(getURL('https://wits.worldbank.org/wits/wits/witshelp/content/codes/country_codes.htm', .opts = list(ssl.verifypeer = FALSE)))
#   iso3WBK <- iso3WBK$`NULL`
#   iso3WBK$V3 <- NULL
#   colnames(iso3WBK) <- c("Country", "ISO3")
#   iso3WBK <- iso3WBK[-1,]; rownames(iso3WBK) <- 1:nrow(iso3WBK)
#   saveRDS(object = iso3WBK, file = './Results/_fixed_data/WBK_ISO3_codes.RDS')
# } else {
#   iso3WBK <- readRDS(file = './Results/_fixed_data/WBK_ISO3_codes.RDS')
#   iso3WBK$Country <- as.character(iso3WBK$Country); iso3WBK$Country[which(iso3WBK$Country == "Côte d'Ivoire")] <- "Ivory Coast"; iso3WBK$Country <- as.factor(iso3WBK$Country)
# }
## ========================================================================== ##

## ========================================================================== ##
## Validate number of matching countries
# emission2 <- emission %>% select(Country) %>% unique
# dplyr::inner_join(x = country_codes, y = emission %>% select(Country) %>% unique, by = c("country.name.en" = "Country")) %>% dim
# emission2$Country[which(is.na(match(emission2$Country, country_codes$country.name.en)))]
#
# GBI2 <- GBI %>% select(Country) %>% unique
# GBI2$Country[which(is.na(match(GBI2$Country, country_codes$country.name.en)))]
## ========================================================================== ##

