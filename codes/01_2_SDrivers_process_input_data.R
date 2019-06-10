# Processing and integrating data: SFS project - drivers
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

# Function for creating tables per dimension and gather all data in one table
generate_drivers_tables <- function(data_path = data_path){
  
  cat(">>> This function generates or loads created drivers input tables <<<\n")
  cat("    1. Verify if intermediate target files exists: ...\n")
  cat("    Environment, Economic, Social, and Food and nutrition\n")
  
  listed_fls <- list.files(paste0(data_path,"/dimensions/drivers"), pattern = "*.csv$", full.names = T)
  
  if(length(listed_fls) < 3){
    
    cat("    1.1. Some intermediate target files does not exist: creating them ...\n")
    
    # Load country codes list
    country_codes <- read.csv(paste0(data_path,"/inputs_raw/country_codes.csv"))
    
    # Demand consumer
    if(!file.exists(paste0(data_path, "/dimensions/drivers/demand_consumer.csv"))){
      
      ## 1. Growing attention paid to diet and health
      ## Measure: Healthy diet, junk food, obesity, organic food
      ## Units: Search importance (0-100)
      ## Years: 2012:2016
      ## Countries with data: 68
      
      diet_health_attention <- readxl::read_excel(paste0(data_path,"/inputs_raw/drivers/sfs_demand_consumer_raw_drivers.xlsx"), sheet = "diet_health_attention")
      diet_health_attention$chg_diet_health_attn <- diet_health_attention$chg_diet_health_attn %>% as.character() %>% as.numeric
      diet_health_attention <- dplyr::left_join(x = country_codes %>% dplyr::select(iso3c, country.name.en), y = diet_health_attention, by = "iso3c")
      diet_health_attention <- diet_health_attention %>% dplyr::select(country.name.en, iso3c, chg_diet_health_attn)
      diet_health_attention <- diet_health_attention %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/demand_consumer/diet_health_attention")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/diet_health_attention_chg_over_time.png"))){
        diet_health_attention %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_diet_health_attn), y = chg_diet_health_attn)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time of interest in aggregate of\nhealthy diet, junk food, and organic food") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/diet_health_attention_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 2. Population growth ### change average over last 3 years
      ## Measure: Population growth
      ## Units: (annual %)
      ## Years: 2015
      ## Countries with data: 214
      
      pop_growth <- readxl::read_excel(paste0(data_path,"/inputs_raw/drivers/sfs_demand_consumer_raw_drivers.xlsx"), sheet = "pop_annual_growth")
      pop_growth$Country <- NULL
      names(pop_growth)[-1] <- paste0("Y", names(pop_growth)[-1])
      
      pop_growth$chg_pop_growth <- pop_growth %>% dplyr::select(Y2004:Y2015) %>% apply(., 1, median, na.rm = T)
      pop_growth <- pop_growth %>% dplyr::select(iso3c, chg_pop_growth)
      pop_growth <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = pop_growth, by = "iso3c")
      pop_growth <- pop_growth %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/demand_consumer/population_growth")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/population_growth_chg_over_time.png"))){
        pop_growth %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_pop_growth), y = chg_pop_growth)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Population annual growth (%)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/population_growth_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 3. GDP growth  ### change average over last 3 years
      ## Measure: Raise in consumers' income
      ## Units: (annual %)
      ## Years: 2015
      ## Countries with data: 192
      
      gdp_growth <- readxl::read_excel(paste0(data_path,"/inputs_raw/drivers/sfs_demand_consumer_raw_drivers.xlsx"), sheet = "gdp_annual_growth")
      gdp_growth$Country <- NULL
      names(gdp_growth)[-1] <- paste0("Y", 2000:2016)
      
      gdp_growth$chg_gdp_growth <- gdp_growth %>% dplyr::select(Y2004:Y2015) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      gdp_growth <- gdp_growth %>% dplyr::select(iso3c, chg_gdp_growth)
      gdp_growth <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = gdp_growth, by = "iso3c")
      gdp_growth <- gdp_growth %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/demand_consumer/gdp_growth")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/gdp_growth_chg_over_time.png"))){
        gdp_growth %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_gdp_growth), y = chg_gdp_growth)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("GDP annual growth (%)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/gdp_growth_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 4. Urban population ### change over time 10 years
      ## Measure: Urbanization
      ## Units: (% of total)
      ## Years: 2005:2015
      ## Countries with data: 213
      
      urban_pop <- readxl::read_excel(paste0(data_path,"/inputs_raw/drivers/sfs_demand_consumer_raw_drivers.xlsx"), sheet = "urban_pop")
      urban_pop$Country <- NULL
      names(urban_pop)[-1] <- paste0("Y", 1960:2016)
      
      base                    <- urban_pop %>% dplyr::select(Y2004:Y2006) %>% apply(., 1, median, na.rm = T)
      recent                  <- urban_pop %>% dplyr::select(Y2014:Y2016) %>% apply(., 1, median, na.rm = T)
      urban_pop$chg_urban_pop <- (recent - base); rm(recent, base)
      urban_pop               <- urban_pop %>% dplyr::select(iso3c, chg_urban_pop)
      urban_pop <- dplyr::left_join(x = country_codes %>% select(country.name.en, iso3c), y = urban_pop, by = "iso3c")
      urban_pop <- urban_pop %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/demand_consumer/urban_population")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/urban_population_chg_over_time.png"))){
        urban_pop %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_urban_pop), y = chg_urban_pop)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time in urban population\n(% of total)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/urban_population_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 5. Employers, female ### change over time 10 years
      ## Measure: Woman involvement
      ## Units: (%)
      ## Years: 2008:2015
      ## Countries with data: 78
      
      employers <- readxl::read_excel(paste0(data_path,"/inputs_raw/drivers/sfs_demand_consumer_raw_drivers.xlsx"), sheet = "female_self_employment")
      employers$Country <- NULL
      names(employers)[-1] <- paste0("Y", names(employers)[-1])
      
      base                    <- employers %>% dplyr::select(Y2007:Y2009) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      recent                  <- employers %>% dplyr::select(Y2014:Y2016) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      employers$chg_employers <- (recent - base); rm(recent, base)
      employers               <- employers %>% dplyr::select(iso3c, chg_employers)
      employers <- dplyr::left_join(x = country_codes %>% select(country.name.en, iso3c), y = employers, by = "iso3c")
      employers <- employers %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/demand_consumer/female_self_employment")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/female_self_employment_chg_over_time.png"))){
        employers %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_employers), y = chg_employers)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time in female self-employment\n(% of female employment)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust = 0.95, vjust = 0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/female_self_employment_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 6. Employment in industry, female ### change over time 10 years
      ## Measure: Woman involvement
      ## Units: (%)
      ## Years: 2005:2015
      ## Countries with data: 91
      
      empl_industry <- readxl::read_excel(paste0(data_path,"/inputs_raw/drivers/sfs_demand_consumer_raw_drivers.xlsx"), sheet = "female_employ_industry")
      empl_industry$Country <- NULL
      names(empl_industry)[-1] <- paste0("Y", names(empl_industry)[-1])
      
      base                            <- empl_industry %>% dplyr::select(Y2004:Y2006) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      recent                          <- empl_industry %>% dplyr::select(Y2014:Y2016) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      empl_industry$chg_empl_industry <- (recent - base); rm(recent, base)
      empl_industry                   <- empl_industry %>% dplyr::select(iso3c, chg_empl_industry)
      empl_industry <- dplyr::left_join(x = country_codes %>% select(country.name.en, iso3c), y = empl_industry, by = "iso3c")
      empl_industry <- empl_industry %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/demand_consumer/female_employment_industry")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/female_employment_industry_chg_over_time.png"))){
        empl_industry %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_empl_industry), y = chg_empl_industry)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time in female employment in industry\n(% of female employment)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust = 0.95, vjust = 0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/female_employment_industry_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 7. Employment services, female ### change over time 10 years
      ## Measure: Woman involvement
      ## Units: (%)
      ## Years: 2005:2015
      ## Countries with data: 92
      
      empl_services <- readxl::read_excel(paste0(data_path,"/inputs_raw/drivers/sfs_demand_consumer_raw_drivers.xlsx"), sheet = "female_employ_services")
      empl_services$Country <- NULL
      names(empl_services)[-1] <- paste0("Y", names(empl_services)[-1])
      
      base                            <- empl_services %>% dplyr::select(Y2004:Y2006) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      recent                          <- empl_services %>% dplyr::select(Y2014:Y2016) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      empl_services$chg_empl_services <- (recent - base); rm(recent, base)
      empl_services                   <- empl_services %>% dplyr::select(iso3c, chg_empl_services)
      empl_services <- dplyr::left_join(x = country_codes %>% select(country.name.en, iso3c), y = empl_services, by = "iso3c")
      empl_services <- empl_services %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/demand_consumer/female_employment_services")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/female_employment_services_chg_over_time.png"))){
        empl_services %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_empl_services), y = chg_empl_services)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time in female employment services\n(% of female employment)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust = 0.95, vjust = 0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/female_employment_services_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      demand_drivers <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = diet_health_attention, by = c("country.name.en", "iso3c"))
      demand_drivers <- dplyr::left_join(x = demand_drivers, y = pop_growth, by = c("country.name.en", "iso3c"))
      demand_drivers <- dplyr::left_join(x = demand_drivers, y = gdp_growth, by = c("country.name.en", "iso3c"))
      demand_drivers <- dplyr::left_join(x = demand_drivers, y = urban_pop, by = c("country.name.en", "iso3c"))
      demand_drivers <- dplyr::left_join(x = demand_drivers, y = employers, by = c("country.name.en", "iso3c"))
      demand_drivers <- dplyr::left_join(x = demand_drivers, y = empl_industry, by = c("country.name.en", "iso3c"))
      demand_drivers <- dplyr::left_join(x = demand_drivers, y = empl_services, by = c("country.name.en", "iso3c"))
      
      demand_drivers <- demand_drivers[-which(apply(X = demand_drivers[,3:ncol(demand_drivers)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 7),]
      rownames(demand_drivers) <- demand_drivers$country.name.en
      demand_drivers$country.name.en <- NULL
      
      write.csv(x = demand_drivers, file = paste0(data_path,"/dimensions/drivers/demand_consumer.csv"), row.names = T)
      rm(diet_health_attention, empl_industry, empl_services, employers, gdp_growth, pop_growth, urban_pop, outdir)
      
    } else {
      demand_drivers <- read.csv(paste0(data_path, "/dimensions/drivers/demand_consumer.csv"), row.names = 1)
      if(!file.exists(paste0(data_path,"/descriptive_graphs/drivers/demand_consumer/demand_consumer_pca.png"))){
        df <- demand_drivers[complete.cases(demand_drivers),]
        env_pca <- df[,-1] %>% FactoMineR::PCA(scale.unit = T, graph = F)
        env_pca %>% factoextra::fviz_pca_biplot(repel = TRUE) +
          ggplot2::theme_bw() +
          ggplot2::ggsave(filename = paste0(data_path,"/descriptive_graphs/drivers/demand_consumer/demand_consumer_pca.png"), units = "in", width = 12, height = 8)
      }
    }
    
    # Production supply
    if(!file.exists(paste0(data_path, "/dimensions/drivers/production_supply.csv"))){
      
      ## 1. Annual temperature per country ## trend all years
      ## Units: (Celsius)
      ## Years: 1991:2015
      ## Countries with data: 227
      
      temp <- readxl::read_excel(paste0(data_path, "/inputs_raw/drivers/sfs_production_supply_raw_drivers.xlsx"), sheet = "annual_temperature")
      temp <- temp %>% tidyr::spread(key = Year, value = tas)
      temp$chg_hist_temp <- apply(temp, 1, function(x){
        y <- as.numeric(x[-1])
        TS <- ts(data = y, start = 1991, end = 2015, frequency = 1)
        slope <- trend::sens.slope(x = TS)
        return(slope$estimates)
      })
      names(temp)[1] <- "iso3c"
      temp <- temp %>% dplyr::select(iso3c, chg_hist_temp)
      temp <- dplyr::left_join(x = country_codes %>% select(country.name.en, iso3c), y = temp, by = "iso3c")
      temp <- temp %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/production_supply/annual_temperature")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/annual_temperature_chg_over_time.png"))){
        temp %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_hist_temp), y = chg_hist_temp)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time in mean temperature") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/annual_temperature_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 2. Annual precipitation per country ## trend all years
      ## Units: (mm)
      ## Years: 1991:2015
      ## Countries with data: 205
      
      prec <- readxl::read_excel(paste0(data_path, "/inputs_raw/drivers/sfs_production_supply_raw_drivers.xlsx"), sheet = "annual_rainfall")
      prec <- prec %>% tidyr::spread(key = Year, value = pr)
      prec$chg_hist_prec <- apply(prec, 1, function(x){
        y <- as.numeric(x[-1])
        TS <- ts(data = y, start = 1991, end = 2015, frequency = 1)
        slope <- trend::sens.slope(x = TS)
        return(slope$estimates)
      })
      names(prec)[1] <- "iso3c"
      prec <- prec %>% dplyr::select(iso3c, chg_hist_prec)
      prec <- dplyr::left_join(x = country_codes %>% select(country.name.en, iso3c), y = prec, by = "iso3c")
      prec <- prec %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/production_supply/annual_precipitation")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/annual_precipitation_chg_over_time.png"))){
        prec %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_hist_prec), y = chg_hist_prec)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time in total precipitation (mm)") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/annual_precipitation_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 3. Annual variability in temperature per country ## trend all years
      ## Units: (Celsius)
      ## Years: 1991:2015
      ## Countries with data: 227
      
      sd_temp <- readxl::read_excel(paste0(data_path, "/inputs_raw/drivers/sfs_production_supply_raw_drivers.xlsx"), sheet = "annual_variability_temp")
      sd_temp <- sd_temp %>% tidyr::spread(key = Year, value = tas)
      sd_temp$chg_sd_temp <- apply(sd_temp, 1, function(x){
        y <- as.numeric(x[-1])
        TS <- ts(data = y, start = 1991, end = 2015, frequency = 1)
        slope <- trend::sens.slope(x = TS)
        return(slope$estimates)
      })
      names(sd_temp)[1] <- "iso3c"
      sd_temp <- sd_temp %>% dplyr::select(iso3c, chg_sd_temp)
      sd_temp <- dplyr::left_join(x = country_codes %>% select(country.name.en, iso3c), y = sd_temp, by = "iso3c")
      sd_temp <- sd_temp %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/production_supply/annual_variability_temperature")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/annual_variability_temperature_chg_over_time.png"))){
        sd_temp %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_sd_temp), y = chg_sd_temp)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time in temperature variability") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/annual_variability_temperature_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 4. Annual variability in precipitation per country ## trend all years
      ## Units: (Celsius)
      ## Years: 1991:2015
      ## Countries with data: 205
      
      sd_prec <- readxl::read_excel(paste0(data_path, "/inputs_raw/drivers/sfs_production_supply_raw_drivers.xlsx"), sheet = "annual_variability_rain")
      sd_prec <- sd_prec %>% tidyr::spread(key = Year, value = pr)
      sd_prec$chg_sd_prec <- apply(sd_prec, 1, function(x){
        y <- as.numeric(x[-1])
        TS <- ts(data = y, start = 1991, end = 2015, frequency = 1)
        slope <- trend::sens.slope(x = TS)
        return(slope$estimates)
      })
      names(sd_prec)[1] <- "iso3c"
      sd_prec <- sd_prec %>% dplyr::select(iso3c, chg_sd_prec)
      sd_prec <- dplyr::left_join(x = country_codes %>% select(country.name.en, iso3c), y = sd_prec, by = "iso3c")
      sd_prec <- sd_prec %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/production_supply/annual_variability_precipitation")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/annual_variability_precipitation_chg_over_time.png"))){
        sd_prec %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_sd_prec), y = chg_sd_prec)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time in precipitation variability") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/annual_variability_precipitation_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 5. Cereal yield ## average last 10 years
      ## Measure:
      ## Units: (kh/ha)
      ## Years: 2005:2015
      ## Countries with data: 179
      
      cereal_yld <- readxl::read_excel(paste0(data_path, "/inputs_raw/drivers/sfs_production_supply_raw_drivers.xlsx"), sheet = "cereal_yield")
      cereal_yld$Country <- NULL
      names(cereal_yld)[-1] <- paste0("Y", 1961:2016)
      
      base                      <- cereal_yld %>% dplyr::select(Y2004:Y2006) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      recent                    <- cereal_yld %>% dplyr::select(Y2014:Y2016) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      cereal_yld$chg_cereal_yld <- (recent - base); rm(recent, base)
      cereal_yld                <- cereal_yld %>% dplyr::select(iso3c, chg_cereal_yld)
      cereal_yld <- dplyr::left_join(x = country_codes %>% select(country.name.en, iso3c), y = cereal_yld, by = "iso3c")
      cereal_yld <- cereal_yld %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/production_supply/cereal_yield")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/cereal_yield_chg_over_time.png"))){
        cereal_yld %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_cereal_yld), y = chg_cereal_yld)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time in cereal yield\n(kg per hectare)") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/cereal_yield_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 6. Agricultural land
      ## Measure: 
      ## Original name: 
      ## Units: ()
      ## Years: 
      ## Countries with data: 206
      
      ag_land <- readxl::read_excel(paste0(data_path, "/inputs_raw/drivers/sfs_production_supply_raw_drivers.xlsx"), sheet = "agricultural_land")
      ag_land$Country <- NULL
      names(ag_land)[-1] <- paste0("Y", 1960:2015)
      
      base                <- ag_land %>% dplyr::select(Y2003:Y2005) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      recent              <- ag_land %>% dplyr::select(Y2013:Y2015) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      ag_land$chg_ag_land <- (recent - base); rm(recent, base)
      ag_land             <- ag_land %>% dplyr::select(iso3c, chg_ag_land)
      ag_land <- dplyr::left_join(x = country_codes %>% select(iso3c, country.name.en), y = ag_land, by = "iso3c")
      ag_land <- ag_land %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/production_supply/agricultural_land")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/agricultural_land_chg_over_time.png"))){
        ag_land %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_ag_land), y = chg_ag_land)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time in agricultural area\n(% of land area)") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/agricultural_land_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 7. Access to infrastructure and information
      ## Measure: 
      ## Units: (m/km)
      ## Years: 
      ## Countries with data: 246
      
      access_infr <- readxl::read_excel(paste0(data_path, "/inputs_raw/drivers/sfs_production_supply_raw_drivers.xlsx"), sheet = "access_infrastructure_info")
      names(access_infr)[2] <- "chg_access_infr"
      access_infr <- dplyr::left_join(x = country_codes %>% select(country.name.en, iso3c), y = access_infr, by = c("country.name.en" = "Country"))
      access_infr <- access_infr %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/production_supply/access_infrastructure_info")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/access_infrastructure_info_chg_over_time.png"))){
        access_infr %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_access_infr), y = chg_access_infr)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change in improved access to\ninfrastructure and information") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/access_infrastructure_info_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 8. Mobile cellular subscriptions (per 100 people)
      ## Measure: 
      ## Units: ()
      ## Years: 
      ## Countries with data: 201
      
      mobile <- readxl::read_excel(paste0(data_path, "/inputs_raw/drivers/sfs_production_supply_raw_drivers.xlsx"), sheet = "cellphone_subscriptions")
      mobile$Country <- NULL
      names(mobile)[-1] <- paste0("Y", colnames(mobile)[-1])
      
      base              <- mobile %>% dplyr::select(Y2004:Y2006) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T); base[which(base == 0)] <- 0.2841842
      recent            <- mobile %>% dplyr::select(Y2014:Y2016) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      mobile$chg_mobile <- (recent - base); rm(recent, base)
      mobile            <- mobile %>% dplyr::select(iso3c, chg_mobile)
      mobile <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = mobile, by = "iso3c")
      mobile <- mobile %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/production_supply/mobile_subscriptions")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/mobile_subscriptions_chg_over_time.png"))){
        access_infr %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_access_infr), y = chg_access_infr)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time in mobile cellular\nsubscriptions (per 100 people)") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/mobile_subscriptions_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 9. Land degradation
      ## Measure: 
      ## Units: degrees
      ## Years: 1991
      ## Countries with data: 180
      
      soil_data <- readxl::read_excel(paste0(data_path, "/inputs_raw/drivers/sfs_production_supply_raw_drivers.xlsx"), sheet = "soil")
      soil_data <- soil_data %>% dplyr::select(Country,Element,Value) %>% tidyr::spread(key = Element, value = Value)
      names(soil_data)[2:3] <- c("land_degradation","soil_erosion")
      soil_data <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = soil_data, by = c("country.name.en" = "Country"))
      
      land_degradation <- soil_data %>% select(country.name.en, iso3c, land_degradation) %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/production_supply/land_degradation")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/land_degradation.png"))){
        land_degradation %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, land_degradation), y = land_degradation)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Land degradation (GLASOD degrees)") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust = 0.95, vjust = 0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/land_degradation.png"), units = "in", width = 20, height = 8)
      }
      
      ## 10. Soil erosion
      ## Measure: 
      ## Units: degrees
      ## Years: 1991
      ## Countries with data: 143
      
      soil_erosion <- soil_data %>% select(country.name.en, iso3c, soil_erosion) %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/production_supply/soil_erosion")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/soil_erosion.png"))){
        soil_erosion %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, soil_erosion), y = soil_erosion)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Soil erosion (GLASOD degrees)") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust = 0.95, vjust = 0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/soil_erosion.png"), units = "in", width = 20, height = 8)
      }
      
      ## 11. Cereal crop yield vs fertilizer application
      ## Measure: 
      ## Units: ()
      ## Years: 2003:2012
      ## Countries with data: 149
      
      yield_fertil    <- readxl::read_excel(paste0(data_path, "/inputs_raw/drivers/sfs_production_supply_raw_drivers.xlsx"), sheet = "cereal_yield_vs_fertilizer")
      yield_fertil$id <- yield_fertil %>% dplyr::group_indices(iso3c)
      
      yield_fertil_base <- yield_fertil %>% dplyr::filter(Year >= 2002, Year <= 2004) %>% dplyr::group_by(iso3c, id) %>% dplyr::summarise(mdn_Fert_appl_base = median(Fertilizer_application, na.rm = T),
                                                                                                                                          mdn_Cerl_yld_base  = median(Cereal_yield, na.rm = T))
      yield_fertil_recent <- yield_fertil %>% dplyr::filter(Year >= 2011, Year <= 2013) %>% dplyr::group_by(iso3c, id) %>% dplyr::summarise(mdn_Fert_appl_recent = median(Fertilizer_application, na.rm = T),
                                                                                                                                            mdn_Cerl_yld_recent  = median(Cereal_yield, na.rm = T))
      
      yield_fertil_all <- dplyr::inner_join(x = yield_fertil_base, y = yield_fertil_recent, by = c("iso3c","id"))
      yield_fertil_all$Fertilizer_application_change <- (yield_fertil_all$mdn_Fert_appl_recent - yield_fertil_all$mdn_Fert_appl_base) #/yield_fertil_all$mean_Fertilizer_application_base
      yield_fertil_all$Cereal_yield_change <- (yield_fertil_all$mdn_Cerl_yld_recent - yield_fertil_all$mdn_Cerl_yld_base) #/yield_fertil_all$mean_Cereal_yield_base
      yield_fertil_all$chg_yield_fertil  <- yield_fertil_all$Cereal_yield_change / yield_fertil_all$Fertilizer_application_change 
      
      chg_yield_fert <- yield_fertil_all %>% dplyr::select(iso3c, chg_yield_fertil)
      chg_yield_fert <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = chg_yield_fert, by = "iso3c")
      chg_yield_fert <- chg_yield_fert %>% tidyr::drop_na()
      rm(yield_fertil, yield_fertil_base, yield_fertil_recent, yield_fertil_all)
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/production_supply/cereal_yield_vs_fertilizer")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/cereal_yield_vs_fertilizer_chg_over_time.png"))){
        chg_yield_fert %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_yield_fertil), y = chg_yield_fertil)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time in ratio of cereal\ncrop yield and fertilizer application") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/cereal_yield_vs_fertilizer_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 12. Fertilizer consumption (kg/ha of arable land)
      ## Measure: 
      ## Units: ()
      ## Years: 2003:2013
      ## Countries with data: 152
      
      fertil_consump <- readxl::read_excel(paste0(data_path, "/inputs_raw/drivers/sfs_production_supply_raw_drivers.xlsx"), sheet = "fertilizer_consumption")
      fertil_consump$Country <- NULL
      colnames(fertil_consump)[-1] <- paste0("Y", names(fertil_consump)[-1])
      
      base                              <- fertil_consump %>% dplyr::select(Y2002:Y2003) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      recent                            <- fertil_consump %>% dplyr::select(Y2012:Y2014) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      fertil_consump$chg_fertil_consump <- (recent - base); rm(recent, base)
      fertil_consump                    <- fertil_consump %>% dplyr::select(iso3c, chg_fertil_consump)
      fertil_consump <- dplyr::left_join(x = country_codes %>% select(country.name.en, iso3c), y = fertil_consump, by = "iso3c")
      fertil_consump <- fertil_consump %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/production_supply/fertilizer_consumption")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/fertilizer_consumption_chg_over_time.png"))){
        fertil_consump %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_fertil_consump), y = chg_fertil_consump)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time in fertilizer consumption\n(kg/ha of arable land)") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust = 0.95, vjust = 0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/fertilizer_consumption_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      production_drivers <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = temp, by = c("country.name.en", "iso3c"))
      production_drivers <- dplyr::left_join(x = production_drivers, y = prec, by = c("country.name.en", "iso3c"))
      production_drivers <- dplyr::left_join(x = production_drivers, y = sd_temp, by = c("country.name.en", "iso3c"))
      production_drivers <- dplyr::left_join(x = production_drivers, y = sd_prec, by = c("country.name.en", "iso3c"))
      production_drivers <- dplyr::left_join(x = production_drivers, y = cereal_yld, by = c("country.name.en", "iso3c"))
      production_drivers <- dplyr::left_join(x = production_drivers, y = ag_land, by = c("country.name.en", "iso3c"))
      production_drivers <- dplyr::left_join(x = production_drivers, y = access_infr, by = c("country.name.en", "iso3c"))
      production_drivers <- dplyr::left_join(x = production_drivers, y = mobile, by = c("country.name.en", "iso3c"))
      production_drivers <- dplyr::left_join(x = production_drivers, y = land_degradation, by = c("country.name.en", "iso3c"))
      production_drivers <- dplyr::left_join(x = production_drivers, y = soil_erosion, by = c("country.name.en", "iso3c"))
      production_drivers <- dplyr::left_join(x = production_drivers, y = chg_yield_fert, by = c("country.name.en", "iso3c"))
      production_drivers <- dplyr::left_join(x = production_drivers, y = fertil_consump, by = c("country.name.en", "iso3c"))
      
      production_drivers <- production_drivers[-which(apply(X = production_drivers[,3:ncol(production_drivers)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 12),]
      rownames(production_drivers) <- production_drivers$country.name.en
      production_drivers$country.name.en <- NULL
      
      write.csv(x = production_drivers, file = paste0(data_path, "/dimensions/drivers/production_supply.csv"), row.names = T)
      rm(list = setdiff(ls(), c("production_drivers", "country_codes", "data_path")))
      
    } else {
      production_drivers <- read.csv(paste0(data_path, "/dimensions/drivers/production_supply.csv"), row.names = 1)
      if(!file.exists(paste0(data_path,"/descriptive_graphs/drivers/production_supply/production_supply_pca.png"))){
        df <- production_drivers[complete.cases(production_drivers),]
        eco_pca <- df[,-1] %>% FactoMineR::PCA(scale.unit = T, graph = F)
        eco_pca %>% factoextra::fviz_pca_biplot(repel = TRUE) +
          ggplot2::theme_bw() +
          ggplot2::ggsave(filename = paste0(data_path,"/descriptive_graphs/drivers/production_supply/production_supply_pca.png"), units = "in", width = 12, height = 8)
      }
    }
    
    # Trade distribution
    if(!file.exists(paste0(data_path, "/dimensions/drivers/trade_distribution.csv"))){
      
      ## 1. Food exports (% of merchandise exports)
      ## Measure: 
      ## Units:
      ## Years: 2005:2015
      ## Countries with data: 144
      
      food_export <- readxl::read_excel(paste0(data_path,"/inputs_raw/drivers/sfs_trade_distribution_raw_drivers.xlsx"), sheet = "food_exports")
      food_export$Country <- NULL
      names(food_export)[-1] <- paste0("Y", names(food_export)[-1])
      
      base                        <- food_export %>% dplyr::select(Y2004:Y2006) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      recent                      <- food_export %>% dplyr::select(Y2014:Y2016) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      food_export$chg_food_export <- (recent - base); rm(recent, base)
      food_export                 <- food_export %>% dplyr::select(iso3c, chg_food_export)
      food_export <- dplyr::left_join(x = country_codes %>% select(country.name.en, iso3c), y = food_export, by = "iso3c")
      food_export <- food_export %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/trade_distribution/food_exports")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/food_exports_chg_over_time.png"))){
        food_export %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_food_export), y = chg_food_export)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Food exports\n(% of merchandise exports)") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust = 0.95, vjust = 0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/food_exports_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 2. Change over time in foreign direct investment (US$ dollars per capita)
      ## Measure: 
      ## Units:
      ## Years: 2004:2014
      ## Countries with data: 186
      
      foreign_invest            <- readxl::read_excel(paste0(data_path,"/inputs_raw/drivers/sfs_trade_distribution_raw_drivers.xlsx"), sheet = "foreign_investment")
      foreign_invest$Country    <- NULL
      names(foreign_invest)[-1] <- paste0("Y", names(foreign_invest)[-1])
      
      population            <- readxl::read_excel(paste0(data_path,"/inputs_raw/drivers/sfs_trade_distribution_raw_drivers.xlsx"), sheet = "pop_total_annual")
      population$Country    <- NULL
      names(population)[-1] <- paste0("Y", names(population)[-1])
      population            <- population %>% dplyr::select(iso3c, Y1970:Y2017)
      
      # Foreign investments
      base   <- foreign_invest %>% dplyr::select(Y2004:Y2006) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      recent <- foreign_invest %>% dplyr::select(Y2014:Y2016) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      FDI    <- data.frame(iso3c = foreign_invest$iso3c, base = base, recent = recent)
      base   <- population %>% dplyr::select(Y2004:Y2006) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      recent <- population %>% dplyr::select(Y2014:Y2016) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      POP    <- data.frame(iso3c = population$iso3c, base = base, recent = recent)
      ALL    <- dplyr::inner_join(x = FDI, y = POP, by = "iso3c"); rm(base, recent, FDI, POP)
      base   <- ALL$base.x/ALL$base.y
      recent <- ALL$recent.x/ALL$recent.y
      foreign_invest <- data.frame(iso3c = ALL$iso3c, chg_foreign_invest = (recent - base)); rm(recent, base)
      foreign_invest <- dplyr::left_join(x = country_codes %>% select(country.name.en, iso3c), y = foreign_invest, by = "iso3c")
      foreign_invest <- foreign_invest %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/trade_distribution/foreign_investment")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/foreign_investment_chg_over_time.png"))){
        foreign_invest %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_foreign_invest), y = chg_foreign_invest)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time in foreign direct\ninvestment (US$ dollars per capita)") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust = 0.95, vjust = 0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/foreign_investment_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 3. Change over time in services trade (US$ dollars per capita)
      ## Measure: 
      ## Units: 
      ## Years: 2004:2014
      ## Countries with data: 189
      
      serv_exp            <- readxl::read_excel(paste0(data_path,"/inputs_raw/drivers/sfs_trade_distribution_raw_drivers.xlsx"), sheet = "service_exports")
      serv_exp$Country    <- NULL
      names(serv_exp)[-1] <- paste0("Y", names(serv_exp)[-1])
      
      serv_imp            <- readxl::read_excel(paste0(data_path,"/inputs_raw/drivers/sfs_trade_distribution_raw_drivers.xlsx"), sheet = "service_imports")
      serv_imp$Country    <- NULL
      names(serv_imp)[-1] <- paste0("Y", names(serv_imp)[-1])
      
      mrch_exp            <- readxl::read_excel(paste0(data_path,"/inputs_raw/drivers/sfs_trade_distribution_raw_drivers.xlsx"), sheet = "merchandise_exports")
      mrch_exp$Country    <- NULL
      names(mrch_exp)[-1] <- paste0("Y", names(mrch_exp)[-1])
      
      mrch_imp            <- readxl::read_excel(paste0(data_path,"/inputs_raw/drivers/sfs_trade_distribution_raw_drivers.xlsx"), sheet = "merchandise_imports")
      mrch_imp$Country    <- NULL
      names(mrch_imp)[-1] <- paste0("Y", names(mrch_imp)[-1])
      
      serv_trd <- data.frame(iso3c = serv_exp$iso3c,
                             ifelse(is.na(as.matrix(serv_exp[,-1])),
                                    ifelse(is.na(as.matrix(serv_imp[,-1])),
                                           NA,
                                           as.matrix(serv_imp[,-1])),
                                    ifelse(is.na(as.matrix(serv_imp[,-1])),
                                           as.matrix(serv_exp[,-1]),
                                           as.matrix(serv_exp[,-1]) + as.matrix(serv_imp[,-1])))
      )
      rm(serv_exp, serv_imp)
      mrch_trd <- data.frame(iso3c = mrch_exp$iso3c,
                             ifelse(is.na(as.matrix(mrch_exp[,-1])),
                                    ifelse(is.na(as.matrix(mrch_imp[,-1])),
                                           NA,
                                           as.matrix(mrch_imp[,-1])),
                                    ifelse(is.na(as.matrix(mrch_imp[,-1])),
                                           as.matrix(mrch_exp[,-1]),
                                           as.matrix(mrch_exp[,-1]) + as.matrix(mrch_imp[,-1])))
      )
      rm(mrch_exp, mrch_imp)
      srmc_trd <- data.frame(iso3c = serv_trd$iso3c,
                             ifelse(is.na(as.matrix(serv_trd[,-1])),
                                    ifelse(is.na(as.matrix(mrch_trd[,-1])),
                                           NA,
                                           as.matrix(mrch_trd[,-1])),
                                    ifelse(is.na(as.matrix(mrch_trd[,-1])),
                                           as.matrix(serv_trd[,-1]),
                                           as.matrix(serv_trd[,-1]) + as.matrix(mrch_trd[,-1])))
      )
      rm(serv_trd, mrch_trd)
      srmc_trd <- srmc_trd %>% dplyr::select(iso3c, Y1970:Y2017)
      
      population <- readxl::read_excel(paste0(data_path,"/inputs_raw/drivers/sfs_trade_distribution_raw_drivers.xlsx"), sheet = "pop_total_annual")
      population$Country <- NULL
      names(population)[-1] <- paste0("Y", names(population)[-1])
      population <- population %>% dplyr::select(iso3c, Y1970:Y2017)
      
      base     <- srmc_trd %>% dplyr::select(Y2004:Y2006) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      recent   <- srmc_trd %>% dplyr::select(Y2014:Y2016) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      SERV     <- data.frame(iso3c = srmc_trd$iso3c, base = base, recent = recent)
      base     <- population %>% dplyr::select(Y2004:Y2006) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      recent   <- population %>% dplyr::select(Y2014:Y2016) %>% apply(., 2, as.numeric) %>% apply(., 1, median, na.rm = T)
      POP      <- data.frame(iso3c = population$iso3c, base = base, recent = recent)
      ALL      <- dplyr::inner_join(x = SERV, y = POP, by = "iso3c"); rm(base, recent, SERV, POP)
      base     <- ALL$base.x/ALL$base.y
      recent   <- ALL$recent.x/ALL$recent.y
      srmc_trd <- data.frame(iso3c = ALL$iso3c, chg_serv_trd = (recent - base)); rm(recent, base)
      
      srmc_trd <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = srmc_trd, by = "iso3c")
      srmc_trd <- srmc_trd %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/trade_distribution/services_trade")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/services_trade_chg_over_time.png"))){
        srmc_trd %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_serv_trd), y = chg_serv_trd)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time in merchandise and services\ntrade (US$ dollars per capita)") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust = 0.95, vjust = 0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/services_trade_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      ## 4. Change over time of interest in aggregate of unsafe food, food quality control, and traceability
      ## Measure: unsafe food, food quality control, and traceability
      ## Units: Search importance (0-100)
      ## Years: 2012:2016
      ## Countries with data: 69
      
      food_safety_concern <- readxl::read_excel(paste0(data_path,"/inputs_raw/drivers/sfs_trade_distribution_raw_drivers.xlsx"), sheet = "food_safety_concern")
      food_safety_concern$chg_food_safety_cncr <- food_safety_concern$chg_food_safety_cncr %>% as.character() %>% as.numeric
      food_safety_concern <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = food_safety_concern, by = "iso3c")
      food_safety_concern <- food_safety_concern %>% dplyr::select(country.name.en, iso3c, chg_food_safety_cncr)
      food_safety_concern <- food_safety_concern %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/drivers/trade_distribution/food_safety_concern")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/food_safety_concern_chg_over_time.png"))){
        food_safety_concern %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, chg_food_safety_cncr), y = chg_food_safety_cncr)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Change over time of interest in aggregate of\nunsafe food, food quality control,\nand traceability") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/food_safety_concern_chg_over_time.png"), units = "in", width = 20, height = 8)
      }
      
      trade_drivers <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = food_export, by = c("country.name.en", "iso3c"))
      trade_drivers <- dplyr::left_join(x = trade_drivers, y = foreign_invest, by = c("country.name.en", "iso3c"))
      trade_drivers <- dplyr::left_join(x = trade_drivers, y = srmc_trd, by = c("country.name.en", "iso3c"))
      trade_drivers <- dplyr::left_join(x = trade_drivers, y = food_safety_concern, by = c("country.name.en", "iso3c"))
      
      trade_drivers <- trade_drivers[-which(apply(X = trade_drivers[,3:ncol(trade_drivers)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 4),]
      rownames(trade_drivers) <- trade_drivers$country.name.en
      trade_drivers$country.name.en <- NULL
      
      write.csv(x = trade_drivers, file = paste0(data_path,"/dimensions/drivers/trade_distribution.csv"), row.names = T)
      rm(list = setdiff(ls(), c("trade_drivers", "country_codes", "data_path")))
      
    } else {
      trade_drivers <- read.csv(paste0(data_path, "/dimensions/drivers/trade_distribution.csv"), row.names = 1)
      if(!file.exists(paste0(data_path,"/descriptive_graphs/drivers/trade_distribution/trade_distribution_pca.png"))){
        df <- trade_drivers[complete.cases(trade_drivers),]
        eco_pca <- df[,-1] %>% FactoMineR::PCA(scale.unit = T, graph = F)
        eco_pca %>% factoextra::fviz_pca_biplot(repel = TRUE) +
          ggplot2::theme_bw() +
          ggplot2::ggsave(filename = paste0(data_path,"/descriptive_graphs/drivers/trade_distribution/trade_distribution_pca.png"), units = "in", width = 12, height = 8)
      }
    }
    
  } else {
    
    cat("    1.2. Intermediate target files already exists: loading them ...\n")
    
    # Load country codes list
    country_codes <- read.csv(paste0(data_path, "/inputs_raw/country_codes.csv"))
    
    Dimensions <- c("demand_consumer", "production_supply", "trade_distribution")
    grep2      <- Vectorize(grep, vectorize.args = "pattern") %>% unlist
    listed_fls <- listed_fls[grep2(Dimensions, listed_fls)]; rm(grep2)
    loaded_tbs <- listed_fls %>% purrr::map(., function(.){read.csv(., row.names = 1)}); rm(listed_fls)
    names(loaded_tbs) <- Dimensions; rm(Dimensions)
    
    drivers <- dplyr::left_join(x = country_codes %>% dplyr::select(iso3c), y = loaded_tbs[["demand_consumer"]], by = "iso3c")
    drivers <- dplyr::left_join(x = drivers, y = loaded_tbs[["production_supply"]], by = "iso3c")
    drivers <- dplyr::left_join(x = drivers, y = loaded_tbs[["trade_distribution"]], by = "iso3c")
    drivers <- drivers[-which(apply(drivers[,2:ncol(drivers)], 1, function(x) sum(is.na(x))) == 21),]
    
    drivers <- dplyr::right_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = drivers, by = "iso3c")
    rownames(drivers) <- drivers$country.name.en; drivers$country.name.en <- NULL
    rm(loaded_tbs, country_codes)
    if(!file.exists(paste0(data_path,"/outputs/drivers/sfs_drivers.csv"))){
      write.csv(x = drivers, file = paste0(data_path,"/outputs/drivers/sfs_drivers.csv"), row.names = T)
    }
    
  }
  
  return(cat("SFS drivers compiled successfully!\n"))
  
}
generate_drivers_tables(data_path = data_path)
rm(generate_drivers_tables, data_path)
