# Stunting and obesity analysis
# H. Achicanoy
# CIAT, 2018

# R options
g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)

OSys <- Sys.info()[1]
OSysPath <- switch(OSys, "Linux" = "/mnt", "Windows" = "//dapadfs")
wk_dir   <- switch(OSys, "Linux" = "/mnt/workspace_cluster_9/Sustainable_Food_System/SFS_indicators", "Windows" = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators")
setwd(wk_dir); rm(wk_dir, OSysPath, OSys)

# Load packages
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(raster, rgdal, maptools, jsonlite, foreach, doParallel, XML, plspm, reshape, tidyverse, countrycode, caret,
                                missMDA, missForest, treemap, viridisLite, highcharter, corrplot, cluster, factoextra, FactoMineR, gghighlight,
                                EnvStats, compiler, caretEnsemble, tabplot, moments))

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


## ========================================================================== ##
# Load stunting and obesity time series
## ========================================================================== ##

#### Stunting

stunting <- read_csv(file = "./Input_data_final/Food_Nutrition/Stunting.csv", col_names = T, skip = 1)
names(stunting)[4] <- "Stunting"
stunting <- stunting %>% select(Country, Year, Stunting)
stunting$Country <- as.character(stunting$Country)
stunting$Country[which(stunting$Country == "Bolivia (Plurinational State of)")] <- "Bolivia"
stunting$Country[which(stunting$Country == "Côte d'Ivoire")] <- "Ivory Coast"
stunting$Country[which(stunting$Country == "Czechia")] <- "Czech Republic"
stunting$Country[which(stunting$Country == "Guinea-Bissau")] <- "Guinea Bissau"
stunting$Country[which(stunting$Country == "The former Yugoslav republic of Macedonia")] <- "The former Yugoslav Republic of Macedonia"
stunting$Country[which(stunting$Country == "United Kingdom of Great Britain and Northern Ireland")] <- "United Kingdom"
stunting$Country[which(stunting$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
stunting <- stunting %>% filter(Year >= 2000)
stunting$Year <- as.numeric(as.character(stunting$Year))

stunting <- stunting %>% spread(key = Year, value = Stunting)

# Function to calculate extreme differences in a time serie
calc_extrm_diff <- function(db){
  
  dff_vct <- rep(NA, nrow(db))
  for(i in 1:nrow(db)){
    
    # Identify extreme values in an individual time serie
    rng <- which(!is.na(db[i, 2:ncol(db)])) %>% range
    
    if(rng[1] != rng[2]){
      dff_vct[i] <- db[i, rng[2]+1] - db[i, rng[1]+1]
    } else {
      dff_vct[i] <- NA
    }
  }
  return(dff_vct %>% unlist)
}

# Calculate change over time in stunting
stunting$chg_stunting <- calc_extrm_diff(db = stunting)

stunting <- dplyr::inner_join(x = country_codes, y = stunting %>% dplyr::select(Country, chg_stunting), by = c("country.name.en" = "Country"))
stunting <- stunting %>% dplyr::select(country.name.en, iso3c, chg_stunting)

#### Obesity

obesity <- read_csv(file = "./Input_data_final/Food_Nutrition/Obesity.csv", col_names = T, skip = 2)
names(obesity)[1] <- "Country"
names(obesity)[2:ncol(obesity)] <- gsub(pattern = "_1", replacement = "", x = names(obesity)[2:ncol(obesity)])
obesity <- obesity[-1,]
names(obesity)[2:ncol(obesity)] <- paste0(names(obesity)[2:ncol(obesity)], "-", obesity[1,2:ncol(obesity)])
obesity <- obesity[-1,]
obesity <- obesity %>% gather(year_sex, Value, -Country) %>% separate(year_sex, c("Year", "Sex"))
obesity$Value <- gsub(pattern = " \\[*.*?\\]", replacement = "", x = obesity$Value) %>% as.character %>% as.numeric
aux <- obesity %>% group_by(Country, Year) %>% summarise(Value = mean(Value, na.rm = T))
aux$Sex <- "Average"
obesity <- rbind(obesity %>% as.data.frame, aux %>% as.data.frame); rm(aux)

obesity <- obesity %>% filter(Sex == "Average")
obesity$Country <- as.character(obesity$Country)
obesity$Year <- as.numeric(as.character(obesity$Year))
obesity$Sex <- NULL
colnames(obesity)[3] <- "Obesity"
obesity$Country[which(obesity$Country == "Bolivia (Plurinational State of)")] <- "Bolivia"
obesity$Country[which(obesity$Country == "Côte d'Ivoire")] <- "Ivory Coast"
obesity$Country[which(obesity$Country == "Czechia")] <- "Czech Republic"
obesity$Country[which(obesity$Country == "Guinea-Bissau")] <- "Guinea Bissau"
obesity$Country[which(obesity$Country == "Sudan (former)")] <- "Sudan"
obesity$Country[which(obesity$Country == "The former Yugoslav republic of Macedonia")] <- "The former Yugoslav Republic of Macedonia"
obesity$Country[which(obesity$Country == "United Kingdom of Great Britain and Northern Ireland")] <- "United Kingdom"
obesity$Country[which(obesity$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
obesity <- obesity %>% filter(Year >= 2000)
obesity <- obesity[!(obesity$Country == "Sudan" & is.na(obesity$Obesity)),]

obesity <- obesity %>% spread(key = Year, value = Obesity)
obesity <- obesity %>% tidyr::drop_na()
rownames(obesity) <- 1:nrow(obesity)

# Calculate change over time in obesity
obesity$chg_obesity <- calc_extrm_diff(db = obesity)

obesity <- dplyr::inner_join(x = country_codes, y = obesity %>% dplyr::select(Country, chg_obesity), by = c("country.name.en" = "Country"))
obesity <- obesity %>% dplyr::select(country.name.en, iso3c, chg_obesity)




###########################
# Level of obesity
obesity <- read_csv(file = "./Input_data_final/Food_Nutrition/Obesity.csv", col_names = T, skip = 2)
names(obesity)[1] <- "Country"
names(obesity)[2:ncol(obesity)] <- gsub(pattern = "_1", replacement = "", x = names(obesity)[2:ncol(obesity)])
obesity <- obesity[-1,]
names(obesity)[2:ncol(obesity)] <- paste0(names(obesity)[2:ncol(obesity)], "-", obesity[1,2:ncol(obesity)])
obesity <- obesity[-1,]
obesity <- obesity %>% gather(year_sex, Value, -Country) %>% separate(year_sex, c("Year", "Sex"))
obesity$Value <- gsub(pattern = " \\[*.*?\\]", replacement = "", x = obesity$Value) %>% as.character %>% as.numeric
aux <- obesity %>% group_by(Country, Year) %>% summarise(Value = mean(Value, na.rm = T))
aux$Sex <- "Average"
obesity <- rbind(obesity %>% as.data.frame, aux %>% as.data.frame); rm(aux)

obesity <- obesity %>% filter(Sex == "Average")
obesity$Country <- as.character(obesity$Country)
obesity$Year <- as.numeric(as.character(obesity$Year))
obesity$Sex <- NULL
colnames(obesity)[3] <- "Obesity"
obesity$Country[which(obesity$Country == "Bolivia (Plurinational State of)")] <- "Bolivia"
obesity$Country[which(obesity$Country == "Côte d'Ivoire")] <- "Ivory Coast"
obesity$Country[which(obesity$Country == "Czechia")] <- "Czech Republic"
obesity$Country[which(obesity$Country == "Guinea-Bissau")] <- "Guinea Bissau"
obesity$Country[which(obesity$Country == "Sudan (former)")] <- "Sudan"
obesity$Country[which(obesity$Country == "The former Yugoslav republic of Macedonia")] <- "The former Yugoslav Republic of Macedonia"
obesity$Country[which(obesity$Country == "United Kingdom of Great Britain and Northern Ireland")] <- "United Kingdom"
obesity$Country[which(obesity$Country == "Venezuela (Bolivarian Republic of)")] <- "Venezuela"
obesity <- obesity %>% filter(Year >= 2000)
obesity <- obesity[!(obesity$Country == "Sudan" & is.na(obesity$Obesity)),]

# obesity %>% ggplot(aes(x = Year, y = Obesity, group = Country)) + geom_line(alpha = .2) + theme_bw()

yearsList <- obesity$Year %>% unique %>% sort
obesityList <- lapply(1:length(yearsList), function(i){
  df <- obesity %>% filter(Year == yearsList[i])
  df <- dplyr::inner_join(x = country_codes, y = df, by = c("country.name.en" = "Country"))
  return(df)
})
lapply(obesityList, dim)

recentObesity <- obesityList[[length(obesityList)]]
recentObesity <- recentObesity %>% dplyr::select(country.name.en, iso3c, Obesity)
recentObesity <- recentObesity[complete.cases(recentObesity),]; rownames(recentObesity) <- 1:nrow(recentObesity)
rm(obesity, obesityList)
###########################




# Load drivers dataset
if(file.exists("../Drivers/demand_consumer.csv")){demand_consumer <- read.csv("../Drivers/demand_consumer.csv", row.names = 1)}
if(file.exists("../Drivers/production_supply.csv")){production_supply <- read.csv("../Drivers/production_supply.csv", row.names = 1)}
if(file.exists("../Drivers/trade_distribution.csv")){trade_distribution <- read.csv("../Drivers/trade_distribution.csv", row.names = 1)}

drivers <- dplyr::left_join(x = country_codes %>% dplyr::select(iso3c), y = demand_consumer, by = "iso3c")
drivers <- dplyr::left_join(x = drivers, y = production_supply, by = "iso3c")
drivers <- dplyr::left_join(x = drivers, y = trade_distribution, by = "iso3c")
drivers <- drivers[-which(is.na(drivers$iso3c)),]
drivers <- drivers[-which(apply(X = drivers[,2:ncol(drivers)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 19),]

drivers <- dplyr::right_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = drivers, by = "iso3c")
rownames(drivers) <- drivers$country.name.en; drivers$country.name.en <- NULL
rm(demand_consumer, production_supply, trade_distribution)

drvrs_stntng <- dplyr::left_join(x = drivers, y = stunting %>% dplyr::select(iso3c, chg_stunting), by = "iso3c")
drvrs_stntng[,-1] %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
  corrplot::corrplot(method = "square")

png(height = 1200, width = 1200, pointsize = 25, file = "./_graphs/correlation_matrix_stunting.png")
drvrs_stntng[,-1] %>% cor(use = "pairwise.complete.obs", method = "spearman") %>% corrplot(type = "upper", method = "square", tl.pos = "lt")
drvrs_stntng[,-1] %>% cor(use = "pairwise.complete.obs", method = "spearman") %>% corrplot(add = T, type = "lower", method = "number", col = "black",
                                                                                       diag = FALSE, tl.pos = "n", cl.pos = "n", number.cex = 0.5, number.digits = 1)
dev.off()


drvrs_stntng[,-1] %>%
  cor(use = "pairwise.complete.obs", method = "spearman")

drvrs_stntng %>%
  select(chg_serv_trd, chg_stunting) %>%
  drop_na() %>%
  .[-which.max(.$chg_serv_trd),] %>%
  ggplot(aes(x = chg_serv_trd, y = chg_stunting)) +
  geom_point() +
  geom_smooth()

drvrs_obesity <- dplyr::left_join(x = drivers, y = obesity %>% dplyr::select(iso3c, chg_obesity), by = "iso3c")
drvrs_obesity[,-1] %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
  corrplot::corrplot(method = "square")

png(height = 1200, width = 1200, pointsize = 25, file = "./_graphs/correlation_matrix_obesity_drivers.png")
drvrs_obesity[,-1] %>% cor(use = "pairwise.complete.obs", method = "spearman") %>% corrplot(type = "upper", method = "square", tl.pos = "lt")
drvrs_obesity[,-1] %>% cor(use = "pairwise.complete.obs", method = "spearman") %>% corrplot(add = T, type = "lower", method = "number",
                                                                                                 diag = FALSE, tl.pos = "n", cl.pos = "n", number.cex = 0.5, number.digits = 2)
dev.off()

mid <- mean(drvrs_obesity$chg_pop_growth, na.rm = T)
drvrs_obesity %>%
  select(chg_serv_trd, chg_obesity, chg_pop_growth, chg_employers) %>%
  drop_na() %>%
  .[-which.max(.$chg_serv_trd),] %>%
  ggplot(aes(x = log(chg_serv_trd), y = chg_obesity, size = chg_employers, colour = chg_pop_growth)) +
  scale_color_gradient2(midpoint=mid, low="blue", mid="green",
                        high="red", space ="Lab" ) +
  geom_point()

sum(!is.na(drvrs_obesity$chg_employers))

# Look the main correlated drivers and those two indicators
# Based on a correlation scheme choose the three drivers for doing the plot Chris wants


drvrs_lvl_obesity <- dplyr::left_join(x = drivers, y = recentObesity %>% dplyr::select(iso3c, Obesity), by = "iso3c")


mid <- mean(drvrs_lvl_obesity$chg_road_infr, na.rm = T)
drvrs_lvl_obesity %>%
  select(chg_serv_trd, Obesity, chg_road_infr, chg_urban_pop) %>%
  drop_na() %>%
  .[-which.max(.$chg_serv_trd),] %>%
  ggplot(aes(x = log(chg_serv_trd), y = Obesity, size = chg_urban_pop, colour = chg_road_infr)) +
  scale_color_gradient2(midpoint=mid, low="blue", mid="green",
                        high="red", space ="Lab" ) +
  geom_point()

drvrs_lvl_obesity[,-1] %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
  corrplot::corrplot(method = "square")

png(height = 1200, width = 1200, pointsize = 25, file = "./_graphs/correlation_matrix_level_obesity_drivers.png")
drvrs_lvl_obesity[,-1] %>% cor(use = "pairwise.complete.obs", method = "spearman") %>% corrplot(type = "upper", method = "square", tl.pos = "lt")
drvrs_lvl_obesity[,-1] %>% cor(use = "pairwise.complete.obs", method = "spearman") %>% corrplot(add = T, type = "lower", method = "number",
                                                                                            diag = FALSE, tl.pos = "n", cl.pos = "n", number.cex = 0.5, number.digits = 2)
dev.off()

hist(drvrs_lvl_obesity$chg_road_infr)
