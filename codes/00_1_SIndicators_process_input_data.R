# Processing and integrating data: SFS project - sustainability indicators
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
generate_indicators_tables <- function(data_path = data_path){
  
  cat(">>> This function generates or loads created input tables <<<\n")
  cat("    1. Verify if intermediate target files exists: ...\n")
  cat("    Environment, Economic, Social, and Food and nutrition\n")
  
  listed_fls <- list.files(paste0(data_path,"/dimensions/indicators"), pattern = "*.csv$", full.names = T)
  
  if(length(listed_fls) < 4){
    
    cat("    1.1. Some intermediate target files does not exist: creating them ...\n")
    
    # Load country codes list
    country_codes <- read.csv(paste0(data_path,"/inputs_raw/country_codes.csv"))
    
    # Environment
    if(!file.exists(paste0(data_path, "/dimensions/indicators/environmental_dimension.csv"))){
      
      ## 1. GHG emissions in total agriculture
      ## Measure: Air quality
      ## Units: gigagrams
      ## Years: 1990-2010 (Selected period: 2000-2010)
      ## Countries with data: 222, Selected year: 2010
      
      emission <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_environmental_raw_indicators.xlsx"), sheet = "emission")
      emission <- emission %>% dplyr::select(Country, Year, Value) %>% dplyr::group_by(Country, Year) %>% dplyr::summarise(Value = sum(Value, na.rm = T))
      emission <- emission %>% dplyr::filter(Year >= 2000)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/environmental/ghg_emissions")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/ghg_emissions.png"))){
        emission %>%
          gghighlight::gghighlight_line(aes(x = Year, y = Value, group = Country), predicate = max(Value) > 30E4) +
          ggplot2::scale_x_continuous(breaks = seq(2000,2010,2)) +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("GHG emissions in total agriculture\n(Gigagrams/year)") +
          ggplot2::theme(axis.text = element_text(size = 15),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/ghg_emissions.png"), units = "in", width = 10, height = 8)
      }
      
      emission <- emission %>% tidyr::spread(Year, Value)
      emission <- emission[,c("Country",
                              apply(X = emission, 2, function(x){sum(!is.na(x))}) %>% tail(., n = 1) %>% names())]
      names(emission)[2] <- "Emissions.agriculture.total"
      emission <- dplyr::inner_join(x = country_codes, y = emission, by = c("country.name.en" = "Country"))
      emission <- emission %>% dplyr::select(country.name.en, iso3c, Emissions.agriculture.total)
      emission <- emission %>% tidyr::drop_na()
      emission$Emissions.agriculture.total <- emission$Emissions.agriculture.total %>% round(1)
      
      if(!file.exists(paste0(outdir,"/ghg_emissions_rcnt.png"))){
        emission %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Emissions.agriculture.total), y = Emissions.agriculture.total)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("GHG emissions in total agriculture\n(Gigagrams/year)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/ghg_emissions_rcnt.png"), units = "in", width = 20, height = 8)
      }
      
      ## 2. Water pH
      ## Measure: Water quality
      ## Units: pH units
      ## Years: 1965-2016 (Selected period: 1965-2016)
      ## Countries with data: 74, Selected year: median of 1965-2016
      
      pH <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_environmental_raw_indicators.xlsx"), sheet = "water_pH")
      pH <- pH %>% dplyr::select(ISO3, Value) %>% dplyr::group_by(ISO3) %>% dplyr::summarise(pH = median(Value, na.rm = T))
      pH <- dplyr::inner_join(x = country_codes, y = pH, by = c("iso3c" = "ISO3"))
      pH <- pH %>% dplyr::select(country.name.en, iso3c, pH)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/environmental/water_pH")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/water_pH_median.png"))){
        pH %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, pH), y = pH)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Water pH\n(pH units)") +
          ggplot2::theme(axis.text = element_text(size = 13),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/water_pH_median.png"), units = "in", width = 12, height = 8)
      }
      ## Not yet. It needs to be done later
      ## pH$pH <- abs(pH$pH - 7)
      
      ## 3. Water withdrawal
      ## Measure: Water use
      ## Units: %
      ## Years: 1988-2016 (Selected period: 2000-2016)
      ## Countries with data: 174, Selected year: mean of 2000-2016
      
      water <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_environmental_raw_indicators.xlsx"), sheet = "water_withdrawal")
      water$Item <- NULL
      water_aux <- lapply(1:nrow(water), function(i){
        df <- data.frame(Country = water$Country[i],
                         Year = c(water$Year1[i], water$Year2[i], water$Year3[i], water$Year4[i], water$Year5[i], water$Year6[i]),
                         Water.withdrawal = c(water$`1988-1992`[i], water$`1993-1997`[i], water$`1998-2002`[i], water$`2003-2007`[i], water$`2008-2012`[i], water$`2013-2017`[i]))
        return(df)
      })
      water <- do.call(rbind, water_aux); rm(water_aux)
      water <- water %>% tidyr::drop_na()
      rownames(water) <- 1:nrow(water)
      water <- water %>% filter(Year >= 2000)
      water$Country <- water$Country %>% as.character
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/environmental/water_withdrawal")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/water_withdrawal.png"))){
        water %>%
          gghighlight::gghighlight_line(aes(x = Year, y = Water.withdrawal, group = Country), predicate = max(Water.withdrawal) > 90) +
          ggplot2::scale_x_continuous(breaks = seq(2000,2016,2)) +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Agricultural water withdrawal\nas percentage of total renewable water (%)") +
          ggplot2::theme(axis.text = element_text(size = 15),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/water_withdrawal.png"), units = "in", width = 10, height = 8)
      }
      
      water <- water %>% tidyr::spread(key = Year, value = Water.withdrawal)
      water$Water.withdrawal <- apply(water[,-1],1,function(x){median(x, na.rm = T)}) %>% round(1)
      water <- water %>% dplyr::select(Country, Water.withdrawal)
      water <- dplyr::inner_join(x = country_codes, y = water, by = c("country.name.en" = "Country"))
      water <- water %>% dplyr::select(country.name.en, iso3c, Water.withdrawal)
      
      if(!file.exists(paste0(outdir,"/water_withdrawal_median.png"))){
        water %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Water.withdrawal), y = Water.withdrawal)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Agricultural water withdrawal\nas percentage of total renewable water (%)") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/water_withdrawal_median.png"), units = "in", width = 20, height = 8)
      }
      
      ## 4. Soil carbon content
      ## Measure: Soil and land quality
      ## Units: %
      ## Years: 2008 (Selected period: 2008)
      ## Countries with data: 202, Selected year: 2008
      
      carbon_soil <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_environmental_raw_indicators.xlsx"), sheet = "soil_carbon")
      carbon_soil <- carbon_soil %>% dplyr::select(Country, Value)
      colnames(carbon_soil)[2] <- "Soil.carbon.content"
      carbon_soil$Country <- as.character(carbon_soil$Country)
      carbon_soil <- dplyr::inner_join(x = country_codes, y = carbon_soil, by = c("country.name.en" = "Country"))
      carbon_soil <- carbon_soil %>% dplyr::select(country.name.en, iso3c, Soil.carbon.content)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/environmental/soil_carbon")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/soil_carbon_rcnt.png"))){
        carbon_soil %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Soil.carbon.content), y = Soil.carbon.content)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Soil carbon (Average carbon content\nin the topsoil as percentage in weight) (%)") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/soil_carbon_rcnt.png"), units = "in", width = 20, height = 8)
      }
      
      ## 5. Arable land
      ## Measure: Soil and land use
      ## Units: %
      ## Years: 1961-2014 (Selected period: 2000-2014)
      ## Countries with data: 217, Selected year: 2014
      
      arable_land <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_environmental_raw_indicators.xlsx"), sheet = "arable_land")
      arable_land <- arable_land %>% dplyr::select(Country, Year, Value)
      arable_land <- arable_land %>% dplyr::filter(Year >= 2000)
      arable_land$Country <- as.character(arable_land$Country)
      colnames(arable_land)[3] <- "Arable.land"
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/environmental/arable_land")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/arable_land.png"))){
        arable_land %>%
          gghighlight::gghighlight_line(aes(x = Year, y = Arable.land, group = Country), predicate = max(Arable.land) > 90) +
          ggplot2::scale_x_continuous(breaks = seq(2000,2014,2)) +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Agricultural land as percentage\nof arable land (%)") +
          ggplot2::theme(axis.text = element_text(size = 15),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/arable_land.png"), units = "in", width = 10, height = 8)
      }
      
      arable_land <- arable_land %>% tidyr::spread(Year, Arable.land)
      arable_land <- arable_land[,c("Country",
                              apply(X = arable_land, 2, function(x){sum(!is.na(x))}) %>% tail(., n = 1) %>% names())]
      names(arable_land)[2] <- "Arable.land"
      arable_land <- dplyr::inner_join(x = country_codes, y = arable_land, by = c("country.name.en" = "Country"))
      arable_land <- arable_land %>% dplyr::select(country.name.en, iso3c, Arable.land)
      arable_land <- arable_land %>% tidyr::drop_na()
      arable_land$Arable.land <- arable_land$Arable.land %>% round(1)
      
      if(!file.exists(paste0(outdir,"/arable_land_rcnt.png"))){
        arable_land %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Arable.land), y = Arable.land)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Agricultural land as percentage\nof arable land (%)") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/arable_land_rcnt.png"), units = "in", width = 20, height = 8)
      }
      
      ## 6. GEF benefits for biodiversity index
      ## Measure: Biodiversity wildlife (plants, animals)
      ## Units: %
      ## Years: 2008 (Selected period: 2008)
      ## Countries with data: 192, Selected year: 2008
      
      GBI <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_environmental_raw_indicators.xlsx"), sheet = "gef_benefits_for_biodiversity")
      GBI$Rank <- GBI$Year <- NULL
      names(GBI)[2] <- "GEF.benefits.biodiversity"
      GBI$Country <- as.character(GBI$Country)
      GBI <- dplyr::inner_join(x = country_codes, y = GBI, by = c("country.name.en" = "Country"))
      GBI <- GBI %>% dplyr::select(country.name.en, iso3c, GEF.benefits.biodiversity)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/environmental/GBI")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/GBI_rcnt.png"))){
        GBI %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, GEF.benefits.biodiversity), y = GEF.benefits.biodiversity)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Benefits of biodiversity index\n0 = no biodiversity potential to 100 = maximum") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/GBI_rcnt.png"), units = "in", width = 20, height = 8)
      }
      
      ## 6. Crop diversity
      ## Measure: Nutrition and diet
      ## Units: Shannon diversity index
      ## Years: 2009:2011
      ## Countries with data: 177
      
      crop_diversity <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_environmental_raw_indicators.xlsx"), sheet = "crop_diversity")
      crop_diversity <- crop_diversity %>% dplyr::filter(Index == "Shannon" & Measurement == "Calories") %>% dplyr::select(Country, Year, Diversity)
      names(crop_diversity)[3] <- "Crop.diversity"
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/environmental/crop_diversity")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/crop_diversity.png"))){
        crop_diversity %>%
          gghighlight::gghighlight_line(aes(x = Year, y = Crop.diversity, group = Country), predicate = max(Crop.diversity) > 2.8) +
          ggplot2::scale_x_continuous(breaks = 2009:2011) +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Crop diversity\nCalories diversity measured by Shannon index") +
          ggplot2::theme(axis.text = element_text(size = 15),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/crop_diversity.png"), units = "in", width = 10, height = 8)
      }
      
      crop_diversity <- crop_diversity %>% tidyr::spread(key = Year, value = Crop.diversity)
      crop_diversity$Crop.diversity <- apply(crop_diversity[,-1],1,function(x){median(x, na.rm = T)})
      crop_diversity <- crop_diversity %>% dplyr::select(Country, Crop.diversity)
      crop_diversity$Country <- crop_diversity$Country %>% as.character
      crop_diversity <- dplyr::inner_join(x = country_codes, y = crop_diversity, by = c("country.name.en" = "Country"))
      crop_diversity <- crop_diversity %>% dplyr::select(country.name.en, iso3c, Crop.diversity)
      
      if(!file.exists(paste0(outdir,"/crop_diversity_median.png"))){
        crop_diversity %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Crop.diversity), y = Crop.diversity)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Crop diversity\nCalories diversity measured by Shannon index") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/crop_diversity_median.png"), units = "in", width = 20, height = 8)
      }
      
      ## 8. Energy used in agriculture and forestry
      ## Measure: energy use
      ## Units: %
      ## Years: 1971-2009 (Selected period: 2000-2009)
      ## Countries with data: 113, Selected year: median of 2000-2009
      
      energy <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_environmental_raw_indicators.xlsx"), sheet = "energy")
      energy <- energy %>% dplyr::select(Country, Year, Value)
      energy$Country <- as.character(energy$Country)
      colnames(energy)[3] <- "Energy.agriculture"
      energy <- energy %>% dplyr::filter(Year >= 2000)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/environmental/agriculture_energy")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/agriculture_energy.png"))){
        energy %>%
          gghighlight::gghighlight_line(aes(x = Year, y = Energy.agriculture, group = Country), predicate = max(Energy.agriculture) > 15) +
          ggplot2::scale_x_continuous(breaks = seq(2000,2010,2)) +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Agriculture and forestry energy use\nas percentage of total (%)") +
          ggplot2::theme(axis.text = element_text(size = 15),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/agriculture_energy.png"), units = "in", width = 10, height = 8)
      }
      
      energy <- energy %>% dplyr::group_by(Country) %>% dplyr::summarise(Energy.agriculture = median(Energy.agriculture, na.rm = T))
      energy <- dplyr::inner_join(x = country_codes, y = energy, by = c("country.name.en" = "Country"))
      energy <- energy %>% dplyr::select(country.name.en, iso3c, Energy.agriculture)
      energy <- energy %>% tidyr::drop_na()
      energy$Energy.agriculture <- energy$Energy.agriculture %>% round(1)
      
      if(!file.exists(paste0(outdir,"/agriculture_energy_median.png"))){
        energy %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Energy.agriculture), y = Energy.agriculture)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Agriculture and forestry energy use\nas percentage of total (%)") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/agriculture_energy_median.png"), units = "in", width = 20, height = 8)
      }
      
      # Consolidate Environmental indicators
      environmentDim <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = emission, by = c("country.name.en", "iso3c"))
      environmentDim <- dplyr::left_join(x = environmentDim, y = pH, by = c("country.name.en", "iso3c"))
      environmentDim <- dplyr::left_join(x = environmentDim, y = water, by = c("country.name.en", "iso3c"))
      environmentDim <- dplyr::left_join(x = environmentDim, y = carbon_soil, by = c("country.name.en", "iso3c"))
      environmentDim <- dplyr::left_join(x = environmentDim, y = arable_land, by = c("country.name.en", "iso3c"))
      environmentDim <- dplyr::left_join(x = environmentDim, y = GBI, by = c("country.name.en", "iso3c"))
      environmentDim <- dplyr::left_join(x = environmentDim, y = crop_diversity, by = c("country.name.en", "iso3c"))
      environmentDim <- dplyr::left_join(x = environmentDim, y = energy, by = c("country.name.en", "iso3c"))
      environmentDim <- environmentDim[-which(apply(X = environmentDim[,3:ncol(environmentDim)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 8),]
      rownames(environmentDim) <- environmentDim$country.name.en
      environmentDim$country.name.en <- NULL
      
      write.csv(x = environmentDim, file = paste0(data_path,"/dimensions/indicators/environmental_dimension.csv"), row.names = T)
      rm(emission, pH, water, carbon_soil, arable_land, GBI, crop_diversity, energy, outdir)
      
    } else {
      environmentDim <- read.csv(paste0(data_path, "/dimensions/indicators/environmental_dimension.csv"), row.names = 1)
      if(!file.exists(paste0(data_path,"/descriptive_graphs/indicators/environmental/environmental_pca.png"))){
        df <- environmentDim[complete.cases(environmentDim),]
        env_pca <- df[,-1] %>% FactoMineR::PCA(scale.unit = T, graph = F)
        env_pca %>% factoextra::fviz_pca_biplot(repel = TRUE) +
          ggplot2::theme_bw() +
          ggplot2::ggsave(filename = paste0(data_path,"/descriptive_graphs/indicators/environmental/environmental_pca.png"), units = "in", width = 12, height = 8)
      }
    }
    
    # Economic
    if(!file.exists(paste0(data_path, "/dimensions/indicators/economic_dimension.csv"))){
      
      ## 1. Agriculture value-added per worker
      ## Measure: Financial performance
      ## Units: Constant 2010 US$
      ## Years: 1980-2016 (Selected period: 2006-2016)
      ## Countries with data: 181, Selected year: median of 2006-2016
      
      AgValueAdded <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_economic_raw_indicators.xlsx"), sheet = "agvalue_added")
      AgValueAdded$`Indicator Name` <- AgValueAdded$`Indicator Code` <- NULL
      names(AgValueAdded)[1:2] <- c("Country", "iso3c")
      AgValueAdded <- AgValueAdded %>% tidyr::gather(key = Year, value = AgValueAdded, 3:ncol(AgValueAdded))
      AgValueAdded$Year <- AgValueAdded$Year %>% as.character %>% as.numeric
      AgValueAdded <- AgValueAdded %>% dplyr::filter(Year >= 2000)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/economic/agricultural_value_added")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/agricultural_value_added.png"))){
        AgValueAdded %>%
          gghighlight::gghighlight_line(aes(x = Year, y = AgValueAdded, group = Country), predicate = max(AgValueAdded) > 90000) +
          ggplot2::scale_x_continuous(breaks = seq(2000,2016,2)) +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Agriculture value-added per worker\n(constant 2010 US$)") +
          ggplot2::theme(axis.text = element_text(size = 15),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/agricultural_value_added.png"), units = "in", width = 10, height = 8)
      }
      
      AgValueAdded <- AgValueAdded %>% tidyr::drop_na()
      AgValueAdded <- AgValueAdded %>% dplyr::filter(Year >= 2006)
      AgValueAdded <- AgValueAdded %>% dplyr::group_by(iso3c) %>% dplyr::summarise(AgValueAdded = median(AgValueAdded, na.rm = T))
      AgValueAdded <- dplyr::inner_join(x = country_codes, y = AgValueAdded, by = "iso3c")
      AgValueAdded <- AgValueAdded %>% dplyr::select(country.name.en, iso3c, AgValueAdded)
      AgValueAdded <- AgValueAdded %>% tidyr::drop_na()
      AgValueAdded$AgValueAdded <- AgValueAdded$AgValueAdded %>% round(1)
      
      if(!file.exists(paste0(outdir,"/agricultural_value_added_median.png"))){
        AgValueAdded %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, AgValueAdded), y = AgValueAdded)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Agriculture value-added per worker\n(constant 2010 US$)") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/agricultural_value_added_median.png"), units = "in", width = 20, height = 8)
      }
      
      ## 2. Agriculture under-employment
      ## Measure: Employment rate
      ## Units: %
      ## Years: 1991-2014 (Selected period: 2000-2014)
      ## Countries with data: 75, Selected year: median of 2007-2014
      
      employment <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_economic_raw_indicators.xlsx"), sheet = "underemployment")
      employment <- employment %>% dplyr::select(Country, Y1952:Y2014F)
      employment <- employment %>% tidyr::gather(Year, Value, -Country)
      employment <- unique(employment)
      employment <- employment[setdiff(1:nrow(employment), grep(pattern = "F$", x = employment$Year)),]
      employment$Value <- employment$Value %>% as.character %>% as.numeric
      employment <- employment %>% dplyr::group_by(Country, Year) %>% dplyr::summarise(Value = median(Value, na.rm = T))
      employment$Year <- gsub(pattern = "Y", replacement = "", x = employment$Year) %>% as.character %>% as.numeric
      employment <- employment %>% tidyr::drop_na()
      rownames(employment) <- 1:nrow(employment)
      names(employment)[3] <- "Time.underemployment"
      employment <- employment %>% dplyr::filter(Year >= 2000)
      employment$Country <- as.character(employment$Country)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/economic/time_underemployment")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/time_underemployment.png"))){
        employment %>%
          gghighlight::gghighlight_line(aes(x = Year, y = Time.underemployment, group = Country), predicate = max(Time.underemployment) > 50) +
          ggplot2::scale_x_continuous(breaks = seq(2000,2014,2)) +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Time-related underemployment\ndistribution, agriculture (%)") +
          ggplot2::theme(axis.text = element_text(size = 15),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/time_underemployment.png"), units = "in", width = 10, height = 8)
      }
      
      employment <- employment %>% tidyr::spread(Year, Time.underemployment)
      employment$Time.underemployment <- apply(employment[,which(names(employment) == "2007"):ncol(employment)], 1, function(x){median(x, na.rm = T)})
      employment <- employment %>% select(Country, Time.underemployment)
      employment <- dplyr::inner_join(x = country_codes, y = employment, by = c("country.name.en" = "Country"))
      employment <- employment %>% dplyr::select(country.name.en, iso3c, Time.underemployment) %>% tidyr::drop_na()
      employment$Time.underemployment <- employment$Time.underemployment %>% round(1)
      
      if(!file.exists(paste0(outdir,"/time_underemployment_median.png"))){
        employment %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Time.underemployment), y = Time.underemployment)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Time-related underemployment\ndistribution, agriculture (%)") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/time_underemployment_median.png"), units = "in", width = 20, height = 8)
      }
      
      ## 3. GINI index for agricultural land
      ## Measure: Economic distribution
      ## Units: GINI index units
      ## Years: 2014 (Selected period: 2014)
      ## Countries with data: 86, Selected year: 2014
      
      gini_agr_land_dist <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_economic_raw_indicators.xlsx"), sheet = "gini_agrLand")
      gini_agr_land_dist <- dplyr::inner_join(x = country_codes, y = gini_agr_land_dist, by = "iso3c")
      gini_agr_land_dist <- gini_agr_land_dist %>% dplyr::select(country.name.en, iso3c, GINI.agr.land.distribution)
      gini_agr_land_dist <- gini_agr_land_dist[complete.cases(gini_agr_land_dist),]; rownames(gini_agr_land_dist) <- 1:nrow(gini_agr_land_dist)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/economic/gini_agricultural_land")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/gini_agricultural_land_rcnt.png"))){
        gini_agr_land_dist %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, GINI.agr.land.distribution), y = GINI.agr.land.distribution)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Gini index for land distribution & tendency") +
          ggplot2::theme(axis.text = element_text(size = 10),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/gini_agricultural_land_rcnt.png"), units = "in", width = 20, height = 8)
      }
      
      # Consolidate Economic indicators
      economicDim <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = AgValueAdded, by = c("country.name.en", "iso3c"))
      economicDim <- dplyr::left_join(x = economicDim, y = employment, by = c("country.name.en", "iso3c"))
      economicDim <- dplyr::left_join(x = economicDim, y = gini_agr_land_dist, by = c("country.name.en", "iso3c"))
      
      economicDim <- economicDim[-which(apply(X = economicDim[,3:ncol(economicDim)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 3),]
      rownames(economicDim) <- economicDim$country.name.en
      economicDim$country.name.en <- NULL
      
      write.csv(x = economicDim, file = paste0(data_path, "/dimensions/indicators/economic_dimension.csv"), row.names = T)
      rm(AgValueAdded, employment, gini_agr_land_dist)
      
    } else {
      economicDim <- read.csv(paste0(data_path, "/dimensions/indicators/economic_dimension.csv"), row.names = 1)
      if(!file.exists(paste0(data_path,"/descriptive_graphs/indicators/economic/economic_pca.png"))){
        df <- economicDim[complete.cases(economicDim),]
        eco_pca <- df[,-1] %>% FactoMineR::PCA(scale.unit = T, graph = F)
        eco_pca %>% factoextra::fviz_pca_biplot(repel = TRUE) +
          ggplot2::theme_bw() +
          ggplot2::ggsave(filename = paste0(data_path,"/descriptive_graphs/indicators/economic/economic_pca.png"), units = "in", width = 12, height = 8)
      }
    }
    
    # Social
    if(!file.exists(paste0(data_path, "/dimensions/indicators/social_dimension.csv"))){
      
      ## 1. Employment in agriculture female
      ## Measure: Gender/Equity
      ## Units: %
      ## Years: 1990-2016 (Selected period: 2000-2016)
      ## Countries with data: 184, Selected year: 2016
      
      FemaleLaborForce <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_social_raw_indicators.xlsx"), sheet = "female_laborforce")
      FemaleLaborForce$`Indicator Name` <- FemaleLaborForce$`Indicator Code` <- NULL
      names(FemaleLaborForce)[1:2] <- c("Country", "iso3c")
      FemaleLaborForce <- FemaleLaborForce %>% tidyr::gather(Year, Female.labor.force, 3:ncol(FemaleLaborForce))
      FemaleLaborForce <- FemaleLaborForce %>% dplyr::filter(Year >= 2000)
      FemaleLaborForce$Year <- FemaleLaborForce$Year %>% as.numeric()
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/social/female_labor_force")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/female_labor_force.png"))){
        FemaleLaborForce %>%
          gghighlight::gghighlight_line(aes(x = Year, y = Female.labor.force, group = Country), predicate = max(Female.labor.force) > 80) +
          ggplot2::scale_x_continuous(breaks = seq(2000,2016,2)) +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Labor force participation rate, female\n(% of female population ages 15+)") +
          ggplot2::theme(axis.text = element_text(size = 15),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/female_labor_force.png"), units = "in", width = 10, height = 8)
      }
      
      FemaleLaborForce <- FemaleLaborForce %>%
        dplyr::select(iso3c, Year, Female.labor.force) %>%
        tidyr::spread(Year, Female.labor.force)
      
      FemaleLaborForce <- FemaleLaborForce[,c("iso3c",
                                              apply(X = FemaleLaborForce, 2, function(x){sum(!is.na(x))}) %>% tail(., n = 1) %>% names())]
      names(FemaleLaborForce)[2] <- "Female.labor.force"
      FemaleLaborForce <- dplyr::inner_join(x = country_codes, y = FemaleLaborForce, by = "iso3c")
      FemaleLaborForce <- FemaleLaborForce %>% dplyr::select(country.name.en, iso3c, Female.labor.force)
      FemaleLaborForce <- FemaleLaborForce %>% tidyr::drop_na()
      FemaleLaborForce$Female.labor.force <- FemaleLaborForce$Female.labor.force %>% round(1)
      
      if(!file.exists(paste0(outdir,"/female_labor_force_rcnt.png"))){
        FemaleLaborForce %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Female.labor.force), y = Female.labor.force)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Labor force participation rate, female\n(% of female population ages 15+)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/female_labor_force_rcnt.png"), units = "in", width = 20, height = 8)
      }
      
      ## 2. Predominant fair trade organizations and producers
      ## Measure: Inclusion
      ## Units: Categorical variable: (1: Fairtrade Producer Network; 2: Fairtrade Organization; 3: Both)
      ## Years: 2016? (Selected period: 2016)
      ## Countries with data: 160, Selected year: 2016
      
      fairtrade <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_social_raw_indicators.xlsx"), sheet = "fair_trade")
      names(fairtrade)[2]     <- "Fairtrade.ctg"
      fairtrade$Country       <- as.character(fairtrade$Country)
      fairtrade$Fairtrade.ctg <- as.character(fairtrade$Fairtrade.ctg)
      fairtrade <- dplyr::inner_join(x = country_codes, y = fairtrade, by = c("country.name.en" = "Country"))
      fairtrade <- fairtrade %>% dplyr::select(country.name.en, iso3c, Fairtrade.ctg)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/social/fairtrade")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/fairtrade_rcnt.png"))){
        
        tdy <- broom::tidy(wrld_simpl, "ISO3")
        dfm <- dplyr::left_join(tdy, fairtrade, by = c("id" = "iso3c"))
        
        ggplot2::ggplot() +
          ggplot2::geom_polygon(data = dfm, aes(x = long, y = lat, group = group, fill = Fairtrade.ctg), colour = "grey") +
          #ggplot2::scale_fill_distiller(palette = "Spectral") +
          ggplot2::coord_equal() +
          ggplot2::xlab("Longitude") +
          ggplot2::ylab("Latitude") +
          ggplot2::labs(fill = "Fairtrade category") +
          # ggplot2::theme(axis.text = element_text(size = 9.5),
          #                axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
          #                axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/fairtrade_rcnt.png"), units = "in", width = 12, height = 8)
      }
      
      # Normalization based on relative frequency
      freq_aux <- table(fairtrade$Fairtrade.ctg)/nrow(fairtrade)
      fairtrade$Fairtrade.ctg[which(fairtrade$Fairtrade.ctg == "Fairtrade Marketing Organization")] <- freq_aux[1]
      fairtrade$Fairtrade.ctg[which(fairtrade$Fairtrade.ctg == "National Fairtrade Organization")] <- freq_aux[2]
      fairtrade$Fairtrade.ctg[which(fairtrade$Fairtrade.ctg == "Producer Network")] <- freq_aux[3]
      fairtrade$Fairtrade.ctg[which(fairtrade$Fairtrade.ctg == "Producer Network and Fairtrade Organization")] <- freq_aux[4]
      fairtrade$Fairtrade.ctg <- as.numeric(fairtrade$Fairtrade.ctg); rm(freq_aux)
      
      ## 3. Employment in agriculture
      ## Measure: Inclusion
      ## Units: %
      ## Years: 2008-2017
      ## Countries with data: depends on the year
      ## Average produces more information: 2008-2017, 149 countries
      
      AgEmployment <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_social_raw_indicators.xlsx"), sheet = "employment_agriculture")
      AgEmployment$`Series Name` <- AgEmployment$`Series Code` <- AgEmployment$`1990` <- NULL
      names(AgEmployment)[1:2] <- c("Country", "iso3c")
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/social/employment_agriculture")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/employment_agriculture.png"))){
        
        AgEmployment <- AgEmployment %>% tidyr::gather(Year, Value, -(Country:iso3c))
        AgEmployment$Year <- AgEmployment$Year %>% as.numeric()
        AgEmployment %>%
          gghighlight::gghighlight_line(aes(x = Year, y = Value, group = Country), predicate = max(Value) > 30) +
          ggplot2::scale_x_continuous(breaks = seq(2000,2017,2)) +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Employment in agriculture\n(% of total employment)") +
          ggplot2::theme(axis.text = element_text(size = 15),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/employment_agriculture.png"), units = "in", width = 10, height = 8)
      }
      
      AgEmployment$Agr.employment <- apply(AgEmployment[,which(names(AgEmployment)=="2008"):ncol(AgEmployment)],1,function(x){median(x, na.rm = T)})
      AgEmployment <- AgEmployment %>% dplyr::select(iso3c, Agr.employment)
      AgEmployment <- dplyr::inner_join(x = country_codes, y = AgEmployment, by = "iso3c")
      AgEmployment <- AgEmployment %>% dplyr::select(country.name.en, iso3c, Agr.employment)
      AgEmployment <- AgEmployment %>% tidyr::drop_na()
      
      if(!file.exists(paste0(outdir,"/employment_agriculture_rcnt.png"))){
        AgEmployment %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Agr.employment), y = Agr.employment)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Employment in agriculture\n(% of total employment)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/employment_agriculture_rcnt.png"), units = "in", width = 20, height = 8)
      }
      
      # Consolidate Social indicators
      socialDim <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = FemaleLaborForce, by = c("country.name.en", "iso3c"))
      socialDim <- dplyr::left_join(x = socialDim, y = fairtrade, by = c("country.name.en", "iso3c"))
      socialDim <- dplyr::left_join(x = socialDim, y = AgEmployment, by = c("country.name.en", "iso3c"))
      
      socialDim <- socialDim[-which(apply(X = socialDim[,3:ncol(socialDim)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 3),]
      rownames(socialDim) <- socialDim$country.name.en
      socialDim$country.name.en <- NULL
      
      write.csv(x = socialDim, file = paste0(data_path, "/dimensions/indicators/social_dimension.csv"), row.names = T)
      
      rm(AgEmployment, FemaleLaborForce, fairtrade)
      
    } else {
      socialDim <- read.csv(paste0(data_path, "/dimensions/indicators/social_dimension.csv"), row.names = 1)
      if(!file.exists(paste0(data_path,"/descriptive_graphs/indicators/social/social_pca.png"))){
        df <- socialDim[complete.cases(socialDim),]
        eco_pca <- df[,-1] %>% FactoMineR::PCA(scale.unit = T, graph = F)
        eco_pca %>% factoextra::fviz_pca_biplot(repel = TRUE) +
          ggplot2::theme_bw() +
          ggplot2::ggsave(filename = paste0(data_path,"/descriptive_graphs/indicators/social/social_pca.png"), units = "in", width = 12, height = 8)
      }
    }
    
    # Food and nutrition
    if(!file.exists(paste0(data_path, "/dimensions/indicators/food_nutrition_dimension.csv"))){
      
      ## 1. Food available for human consumption
      ## Measure: Food security availability
      ## Units: kcal/capita/day
      ## Years: 2016
      ## Countries with data: 113
      
      # GFSI indices 2016
      gfsi <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "gfsi", skip = 5)
      gfsi <- gfsi[,10:ncol(gfsi)]
      gfsi <- gfsi[-c(1:4),]
      gfsi <- gfsi[-c(7:8),]
      gfsi <- gfsi[-10,]
      gfsi <- gfsi[-18,]
      gfsi <- gfsi[-19,]
      gfsi <- gfsi[-22,]
      gfsi <- gfsi[-26,]
      gfsi <- gfsi[-29,]
      gfsi <- gfsi[1:36, 1:115]
      names(gfsi)[1:2] <- c("Variable", "Units")
      gfsi$Units <- NULL
      gfsi <- gfsi %>% tidyr::gather(Country, Value, -Variable) %>% tidyr::spread(Variable, Value)
      nmList <- names(gfsi)
      gfsi <- data.frame(Country = gfsi$Country, apply(X = gfsi[,-1], MARGIN = 2, FUN = as.numeric))
      names(gfsi) <- nmList; rm(nmList)
      gfsi$Country <- as.character(gfsi$Country)
      
      AverageFoodSupply <- gfsi[,c("Country","2.1.1) Average food supply")]
      names(AverageFoodSupply)[2] <- "Food.available"
      AverageFoodSupply <- dplyr::inner_join(x = country_codes, y = AverageFoodSupply, by = c("country.name.en" = "Country"))
      AverageFoodSupply <- AverageFoodSupply %>% dplyr::select(country.name.en, iso3c, Food.available)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/food_nutrition/food_availability")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/food_availability_rcnt.png"))){
        AverageFoodSupply %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Food.available), y = Food.available)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Per capita food available for\nhuman consumption (kcal/capita/day)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/food_availability_rcnt.png"), units = "in", width = 20, height = 8)
      }
      
      ## 2. Food consumption
      ## Measure: Food security access
      ## Units: % of total household expenditure
      ## Years: 2016
      ## Countries with data: 113
      
      FoodConsumption <- gfsi[,c("Country","1.1) Food consumption as a share of household expenditure")]
      names(FoodConsumption)[2] <- "Food.consumption"
      FoodConsumption <- dplyr::inner_join(x = country_codes, y = FoodConsumption, by = c("country.name.en" = "Country"))
      FoodConsumption <- FoodConsumption %>% dplyr::select(country.name.en, iso3c, Food.consumption)
      FoodConsumption$Food.consumption <- FoodConsumption$Food.consumption %>% round(1)
      rm(gfsi)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/food_nutrition/food_consumption")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/food_consumption_rcnt.png"))){
        FoodConsumption %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Food.consumption), y = Food.consumption)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Food consumption as share of total income\n(% of total household expenditure)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/food_consumption_rcnt.png"), units = "in", width = 20, height = 8)
      }
      
      ## 3. City access
      ## Measure: Food security access
      ## Original name: Estimated travel time to the nearest city of 50,000 or more people in year 2000
      ## Units: minutes of travel time
      ## Years: 2000
      ## Countries with data: 245
      
      city_access <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "city_access")
      city_access <- city_access[,c("ISO3","MEDIAN")]
      names(city_access) <- c("iso3c","City.access")
      city_access <- dplyr::inner_join(x = country_codes, y = city_access, by = "iso3c")
      city_access <- city_access %>% dplyr::select(country.name.en, iso3c, City.access)
      city_access$City.access[which(city_access$City.access == -9999)] <- NA
      city_access <- city_access %>% tidyr::drop_na()
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/food_nutrition/city_access")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/city_access_rcnt.png"))){
        city_access %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, City.access), y = City.access)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Travel time to nearest city (minutes)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/city_access_rcnt.png"), units = "in", width = 20, height = 8)
      }
      
      ## 4. Access to improved water resource
      ## Measure: Food security utilization
      ## Units: %
      ## Years: 1990:2014 (Selected period: 2000-2014)
      ## Countries with data: depends on the year
      ## Average produces more information: 2010-2014, 198 countries
      
      improved_water <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "access_improved_water")
      improved_water <- improved_water %>% dplyr::select(Country, Year, Value)
      names(improved_water)[3] <- "Access.improved.water"
      improved_water <- improved_water %>% dplyr::filter(Year >= 2000)
      improved_water <- improved_water %>% dplyr::filter(Year <= 2014)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/food_nutrition/improved_water")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/improved_water.png"))){
        improved_water %>%
          gghighlight::gghighlight_line(aes(x = Year, y = Access.improved.water, group = Country), predicate = max(Access.improved.water) > 99) +
          ggplot2::scale_x_continuous(breaks = seq(2000,2014,2)) +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Access to improved water resource (%)") +
          ggplot2::theme(axis.text = element_text(size = 15),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/improved_water.png"), units = "in", width = 10, height = 8)
      }
      
      improved_water <- improved_water %>% tidyr::spread(Year, Access.improved.water)
      improved_water$Access.improved.water <- apply(improved_water[,which(names(improved_water)=="2005"):which(names(improved_water)=="2014")],1,function(x){median(x, na.rm = T)})
      improved_water <- improved_water[,c("Country","Access.improved.water")]
      improved_water$Country <- as.character(improved_water$Country)
      improved_water <- dplyr::inner_join(x = country_codes, y = improved_water, by = c("country.name.en" = "Country"))
      improved_water <- improved_water %>% dplyr::select(country.name.en, iso3c, Access.improved.water)
      improved_water <- improved_water %>% tidyr::drop_na()
      
      if(!file.exists(paste0(outdir,"/improved_water_median.png"))){
        improved_water %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Access.improved.water), y = Access.improved.water)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Access to improved water resource (%)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/improved_water_median.png"), units = "in", width = 20, height = 8)
      }
      
      ## 5. Access to electricity
      ## Measure: Food security utilization
      ## Original name: Access to electricity
      ## Units: (%)
      ## Years: 1990:2015 (Selected period: 2000:2014)
      ## Countries with data: 211
      
      access_electricity <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "access_electricity", skip = 3)
      names(access_electricity)[1:2] <- c("Country", "iso3c")
      access_electricity <- access_electricity %>% dplyr::select(Country, iso3c, `2000`:`2014`)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/food_nutrition/access_electricity")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/access_electricity.png"))){
        
        access_electricity <- access_electricity %>% tidyr::gather(Year, Value, -(Country:iso3c))
        access_electricity$Year <- access_electricity$Year %>% as.numeric
        access_electricity$Value <- access_electricity$Value %>% as.numeric
        access_electricity %>%
          gghighlight::gghighlight_line(aes(x = Year, y = Value, group = Country), predicate = max(Value) > 99) +
          ggplot2::scale_x_continuous(breaks = seq(2000,2014,2)) +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Access to electricity (%)") +
          ggplot2::theme(axis.text = element_text(size = 15),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/access_electricity.png"), units = "in", width = 10, height = 8)
      }
      
      access_electricity <- access_electricity[,c("iso3c",
                              apply(X = access_electricity, 2, function(x){sum(!is.na(x))}) %>% tail(., n = 1) %>% names())]
      names(access_electricity)[2] <- "Access.electricity"
      access_electricity <- dplyr::inner_join(x = country_codes, y = access_electricity, by = "iso3c")
      access_electricity <- access_electricity %>% dplyr::select(country.name.en, iso3c, Access.electricity)
      access_electricity <- access_electricity %>% tidyr::drop_na()
      access_electricity$Access.electricity <- access_electricity$Access.electricity %>% round(1)
      
      if(!file.exists(paste0(outdir,"/access_electricity_median.png"))){
        access_electricity %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Access.electricity), y = Access.electricity)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Access to electricity (%)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/access_electricity_median.png"), units = "in", width = 20, height = 8)
      }
      
      ## 6. Price volatility index
      ## Measure: Food security stability
      ## Units: (CV %)
      ## Years: 2011:2017
      ## Countries with data: depends on the year
      ## Average produces more information: 2013-2017, 194 countries
      
      price_volatility <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "price_volatility")
      price_volatility <- price_volatility %>% dplyr::select(Country, Year, `Food CPI CV`)
      names(price_volatility)[3] <- "Price.volatility.index"
      price_volatility$Price.volatility.index <- price_volatility$Price.volatility.index %>% as.character() %>% as.numeric()
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/food_nutrition/price_volatility")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/price_volatility.png"))){
        
        price_volatility %>%
          gghighlight::gghighlight_line(aes(x = Year, y = Price.volatility.index * 100, group = Country), predicate = max(Price.volatility.index * 100) > 60) +
          ggplot2::scale_x_continuous(breaks = seq(2011,2017,2)) +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Price volatility index (%)") +
          ggplot2::theme(axis.text = element_text(size = 15),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/price_volatility.png"), units = "in", width = 10, height = 8)
      }
      
      price_volatility <- price_volatility %>% tidyr::spread(Year, Price.volatility.index)
      price_volatility$Price.volatility.index <- apply(price_volatility[,which(names(price_volatility)=="2013"):which(names(price_volatility)=="2017")],1,function(x){median(x, na.rm = T)}) * 100
      price_volatility <- price_volatility[,c("Country","Price.volatility.index")]
      price_volatility$Country <- as.character(price_volatility$Country)
      price_volatility <- dplyr::inner_join(x = country_codes, y = price_volatility, by = c("country.name.en" = "Country"))
      price_volatility <- price_volatility %>% dplyr::select(country.name.en, iso3c, Price.volatility.index)
      price_volatility$Price.volatility.index <- price_volatility$Price.volatility.index %>% round(1)
      price_volatility <- price_volatility %>% tidyr::drop_na()
      
      if(!file.exists(paste0(outdir,"/price_volatility_median.png"))){
        price_volatility %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Price.volatility.index), y = Price.volatility.index)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Price volatility index (%)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/access_electricity_median.png"), units = "in", width = 20, height = 8)
      }
      
      ## 7. Food supply variability
      ## Measure: Food security stability
      ## Units: kcal/capita/day
      ## Years: (Selected period: 2000:2011)
      ## Countries with data: 162
      
      fsvar <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "food_supply_variability")
      fsvar <- fsvar %>% dplyr::select(Country, Year, Value)
      names(fsvar)[3] <- "Food.supply.variability"
      fsvar$Country <- as.character(fsvar$Country)
      fsvar <- fsvar %>% dplyr::filter(Year >= 2000)
      fsvar <- fsvar %>% filter(Year <= 2011)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/food_nutrition/food_supply_variability")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/food_supply_variability.png"))){
        fsvar %>%
          gghighlight::gghighlight_line(aes(x = Year, y = Food.supply.variability, group = Country), predicate = max(Food.supply.variability) > 200) +
          ggplot2::scale_x_continuous(breaks = seq(2000,2011,2)) +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Per capita food supply variability\n(kcal/capita/day)") +
          ggplot2::theme(axis.text = element_text(size = 15),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/food_supply_variability.png"), units = "in", width = 10, height = 8)
      }
      
      fsvar <- fsvar %>% tidyr::spread(Year, Food.supply.variability)
      fsvar <- fsvar[,c("Country",
                        apply(X = fsvar, 2, function(x){sum(!is.na(x))}) %>% tail(., n = 1) %>% names())]
      names(fsvar)[2] <- "Food.supply.variability"
      fsvar <- dplyr::inner_join(x = country_codes, y = fsvar, by = c("country.name.en" = "Country"))
      fsvar <- fsvar %>% dplyr::select(country.name.en, iso3c, Food.supply.variability)
      fsvar <- fsvar %>% tidyr::drop_na()
      
      if(!file.exists(paste0(outdir,"/food_supply_variability_rcnt.png"))){
        fsvar %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Food.supply.variability), y = Food.supply.variability)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Per capita food supply variability\n(kcal/capita/day)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/food_supply_variability_rcnt.png"), units = "in", width = 20, height = 8)
      }
      
      ## 8. Burden of food borne illness
      ## Measure: Food safety
      ## Original name: Burden of food borne illness
      ## Units: foodborne illnesses per 100,000
      ## Years: 2010
      ## Countries with data: 193
      
      foodBorne_illness <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "foodborne_illness")
      foodBorne_illness$Region <- NULL
      foodBorne_illness$Country <- as.character(foodBorne_illness$Country)
      foodBorne_illness <- dplyr::inner_join(x = country_codes, y = foodBorne_illness, by = c("country.name.en" = "Country"))
      foodBorne_illness <- foodBorne_illness %>% dplyr::select(country.name.en, iso3c, Foodborne.illness)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/food_nutrition/foodborne_illnesses")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/foodborne_illnesses_rcnt.png"))){
        foodBorne_illness %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Foodborne.illness), y = Foodborne.illness)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Burden of foodborne illness\n(foodborne illnesses per 100,000)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/foodborne_illnesses_rcnt.png"), units = "in", width = 20, height = 8)
      }
      
      ## 9. Food loss
      ## Measure: Food waste and use
      ## Original name: Total waste/total domestic supply quantity (tonnes)
      ## Units: (%)
      ## Years: 2016
      ## Countries with data: 113
      
      food_loss <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "food_loss")
      food_loss$Country <- as.character(food_loss$Country)
      food_loss <- dplyr::inner_join(x = country_codes, y = food_loss, by = c("country.name.en" = "Country"))
      food_loss <- food_loss %>% dplyr::select(country.name.en, iso3c, Food.loss)
      food_loss$Food.loss <- food_loss$Food.loss %>% round(1)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/food_nutrition/food_loss")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/food_loss_rcnt.png"))){
        food_loss %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Food.loss), y = Food.loss)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Food loss as\n% of total food produced") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/food_loss_rcnt.png"), units = "in", width = 20, height = 8)
      }
      
      ## 10. Diet diversification
      ## Measure: Nutrition diet
      ## Original name: Diet diversification
      ## Units: (%)
      ## Years: 2001:2010
      ## Countries with data: 165
      
      diet_div <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "diet_diversification")
      diet_div <- diet_div %>% dplyr::select(Country, Year, Value)
      names(diet_div)[3] <- "Diet.diversification"
      diet_div$Country <- as.character(diet_div$Country)
      diet_div <- diet_div %>% dplyr::filter(Year >= 2000)
      diet_div$Year <- as.character(diet_div$Year)
      diet_div$Year[which(diet_div$Year == "2000-2002")] <- "2001"
      diet_div$Year[which(diet_div$Year == "2001-2003")] <- "2002"
      diet_div$Year[which(diet_div$Year == "2002-2004")] <- "2003"
      diet_div$Year[which(diet_div$Year == "2003-2005")] <- "2004"
      diet_div$Year[which(diet_div$Year == "2004-2006")] <- "2005"
      diet_div$Year[which(diet_div$Year == "2005-2007")] <- "2006"
      diet_div$Year[which(diet_div$Year == "2006-2008")] <- "2007"
      diet_div$Year[which(diet_div$Year == "2007-2009")] <- "2008"
      diet_div$Year[which(diet_div$Year == "2008-2010")] <- "2009"
      diet_div$Year[which(diet_div$Year == "2009-2011")] <- "2010"
      diet_div$Year[which(diet_div$Year == "2010-2012")] <- "2011"
      diet_div$Year[which(diet_div$Year == "2011-2013")] <- "2012"
      diet_div$Year[which(diet_div$Year == "2012-2014")] <- "2013"
      diet_div$Year[which(diet_div$Year == "2013-2015")] <- "2014"
      diet_div$Year[which(diet_div$Year == "2014-2016")] <- "2015"
      diet_div$Year <- as.numeric(diet_div$Year)
      diet_div <- diet_div %>% filter(Year <= 2010)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/food_nutrition/diet_diversification")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/diet_diversification.png"))){
        diet_div %>%
          gghighlight::gghighlight_line(aes(x = Year, y = Diet.diversification, group = Country), predicate = max(Diet.diversification) > 70) +
          ggplot2::scale_x_continuous(breaks = seq(2001,2010,2)) +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Diet diversification\n(kcal/caput/day)") +
          ggplot2::theme(axis.text = element_text(size = 15),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/diet_diversification.png"), units = "in", width = 10, height = 8)
      }
      
      diet_div <- diet_div %>% tidyr::spread(Year, Diet.diversification)
      diet_div <- diet_div[,c("Country",
                              apply(X = diet_div, 2, function(x){sum(!is.na(x))}) %>% tail(., n = 1) %>% names())]
      names(diet_div)[2] <- "Diet.diversification"
      diet_div <- dplyr::inner_join(x = country_codes, y = diet_div, by = c("country.name.en" = "Country"))
      diet_div <- diet_div %>% dplyr::select(country.name.en, iso3c, Diet.diversification)
      diet_div <- diet_div %>% tidyr::drop_na()
      
      if(!file.exists(paste0(outdir,"/diet_diversification_rcnt.png"))){
        diet_div %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Diet.diversification), y = Diet.diversification)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Diet diversification\n(kcal/caput/day)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/diet_diversification_rcnt.png"), units = "in", width = 20, height = 8)
      }
      
      ## 11. Stunting
      ## Measure: Undernutrition
      ## Units: (%)
      ## Years: 2000:2014
      ## Countries with data: depends on the year
      ## Average produces more information: 2010-2014, 98 countries
      
      stunting <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "stunting", skip = 1)
      names(stunting)[4] <- "Stunting"
      stunting <- stunting %>% select(Country, Year, Stunting)
      stunting$Country <- as.character(stunting$Country)
      stunting$Year <- stunting$Year %>% as.character %>% as.numeric
      stunting <- stunting %>% dplyr::filter(Year >= 2000)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/food_nutrition/stunting")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/stunting.png"))){
        stunting %>%
          gghighlight::gghighlight_line(aes(x = Year, y = Stunting, group = Country), predicate = max(Stunting) > 50) +
          ggplot2::scale_x_continuous(breaks = seq(2000,2014,2)) +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Stunting, children aged < 5 years stunted (%)") +
          ggplot2::theme(axis.text = element_text(size = 15),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/stunting.png"), units = "in", width = 10, height = 8)
      }
      
      stunting <- stunting %>% tidyr::spread(Year, Stunting)
      stunting$Stunting <- apply(stunting[,which(names(stunting)=="2005"):which(names(stunting)=="2014")],1,function(x){median(x, na.rm = T)})
      stunting <- stunting[,c("Country","Stunting")]
      stunting <- dplyr::inner_join(x = country_codes, y = stunting, by = c("country.name.en" = "Country"))
      stunting <- stunting %>% dplyr::select(country.name.en, iso3c, Stunting)
      stunting <- stunting %>% tidyr::drop_na()
      stunting$Stunting <- stunting$Stunting %>% round(1)
      
      if(!file.exists(paste0(outdir,"/stunting_median.png"))){
        stunting %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Stunting), y = Stunting)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Stunting, children aged < 5 years stunted (%)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/stunting_median.png"), units = "in", width = 20, height = 8)
      }
      
      ## 12. Obesity
      ## Measure: Overnutrition
      ## Units: (%)
      ## Years: 2000:2014
      ## Countries with data: 191
      
      obesity <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "obesity", skip = 2)
      names(obesity)[1] <- "Country"
      names(obesity)[2:ncol(obesity)] <- gsub(pattern = "_1", replacement = "", x = names(obesity)[2:ncol(obesity)])
      obesity <- obesity[-1,]
      names(obesity)[2:ncol(obesity)] <- paste0(names(obesity)[2:ncol(obesity)], "-", obesity[1,2:ncol(obesity)])
      obesity <- obesity[-1,]
      obesity <- obesity %>% tidyr::gather(year_sex, Value, -Country) %>% tidyr::separate(year_sex, c("Year", "Sex"))
      obesity$Value <- gsub(pattern = " \\[*.*?\\]", replacement = "", x = obesity$Value) %>% as.character %>% as.numeric
      aux <- obesity %>% dplyr::group_by(Country, Year) %>% dplyr::summarise(Value = mean(Value, na.rm = T))
      aux$Sex <- "Average"
      obesity <- rbind(obesity %>% as.data.frame, aux %>% as.data.frame); rm(aux)
      obesity <- obesity %>% dplyr::filter(Sex == "Average")
      obesity$Country <- as.character(obesity$Country)
      obesity$Year <- as.numeric(as.character(obesity$Year))
      obesity$Sex <- NULL
      colnames(obesity)[3] <- "Obesity"
      obesity <- obesity %>% dplyr::filter(Year >= 2000)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/food_nutrition/obesity")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/obesity.png"))){
        obesity %>%
          gghighlight::gghighlight_line(aes(x = Year, y = Obesity, group = Country), predicate = max(Obesity) > 40) +
          ggplot2::scale_x_continuous(breaks = seq(2000,2014,2)) +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Prevalence of obesity\n(% of the population over 18 years of age)") +
          ggplot2::theme(axis.text = element_text(size = 15),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/obesity.png"), units = "in", width = 10, height = 8)
      }
      
      obesity <- obesity %>% tidyr::spread(Year, Obesity)
      obesity <- obesity[,c("Country",
                            apply(X = obesity, 2, function(x){sum(!is.na(x))}) %>% tail(., n = 1) %>% names())]
      names(obesity)[2] <- "Obesity"
      obesity <- dplyr::inner_join(x = country_codes, y = obesity, by = c("country.name.en" = "Country"))
      obesity <- obesity %>% dplyr::select(country.name.en, iso3c, Obesity)
      obesity <- obesity %>% tidyr::drop_na()
      obesity$Obesity <- obesity$Obesity %>% round(1)
      
      if(!file.exists(paste0(outdir,"/obesity_rcnt.png"))){
        obesity %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Obesity), y = Obesity)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Prevalence of obesity\n(% of the population over 18 years of age)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/obesity_rcnt.png"), units = "in", width = 20, height = 8)
      }
      
      ## 13. Nutrient deficiency in vitamin A
      ## Measure: Overnutrition
      ## Units: (%)
      ## Years: 2000:2014
      ## Countries with data: 195
      
      serum_retinol <- readxl::read_excel(paste0(data_path, "/inputs_raw/indicators/sfs_food_nutrition_raw_indicators.xlsx"), sheet = "nutrition_deficiency")
      serum_retinol$Country <- serum_retinol$Country %>% as.character
      serum_retinol <- dplyr::inner_join(x = country_codes, y = serum_retinol, by = c("country.name.en" = "Country"))
      serum_retinol <- serum_retinol %>% dplyr::select(country.name.en, iso3c, Serum.retinol.deficiency)
      
      outdir <- paste0(data_path,"/descriptive_graphs/indicators/food_nutrition/deficiency_vitamin_A")
      if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
      if(!file.exists(paste0(outdir,"/deficiency_vitamin_A_rcnt.png"))){
        serum_retinol %>%
          ggplot2::ggplot(aes(x = reorder(country.name.en, Serum.retinol.deficiency), y = Serum.retinol.deficiency)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::theme_bw() +
          ggplot2::xlab("") +
          ggplot2::ylab("Nutrient deficiency\nSerum retinol deficiency (%)") +
          ggplot2::theme(axis.text = element_text(size = 9.5),
                         axis.text.x = element_text(hjust=0.95, vjust=0.2, angle = 90),
                         axis.title = element_text(size = 20)) +
          ggplot2::ggsave(filename = paste0(outdir,"/deficiency_vitamin_A_rcnt.png"), units = "in", width = 20, height = 8)
      }
      
      # Consolidate Food and nutrition indicators
      foodNutDim <- dplyr::left_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = AverageFoodSupply, by = c("country.name.en", "iso3c"))
      foodNutDim <- dplyr::left_join(x = foodNutDim, y = FoodConsumption, by = c("country.name.en", "iso3c"))
      foodNutDim <- dplyr::left_join(x = foodNutDim, y = city_access, by = c("country.name.en", "iso3c"))
      foodNutDim <- dplyr::left_join(x = foodNutDim, y = improved_water, by = c("country.name.en", "iso3c"))
      foodNutDim <- dplyr::left_join(x = foodNutDim, y = access_electricity, by = c("country.name.en", "iso3c"))
      foodNutDim <- dplyr::left_join(x = foodNutDim, y = price_volatility, by = c("country.name.en", "iso3c"))
      foodNutDim <- dplyr::left_join(x = foodNutDim, y = fsvar, by = c("country.name.en", "iso3c"))
      foodNutDim <- dplyr::left_join(x = foodNutDim, y = foodBorne_illness, by = c("country.name.en", "iso3c"))
      foodNutDim <- dplyr::left_join(x = foodNutDim, y = food_loss, by = c("country.name.en", "iso3c"))
      foodNutDim <- dplyr::left_join(x = foodNutDim, y = diet_div, by = c("country.name.en", "iso3c"))
      foodNutDim <- dplyr::left_join(x = foodNutDim, y = stunting, by = c("country.name.en", "iso3c"))
      foodNutDim <- dplyr::left_join(x = foodNutDim, y = obesity, by = c("country.name.en", "iso3c"))
      foodNutDim <- dplyr::left_join(x = foodNutDim, y = serum_retinol, by = c("country.name.en", "iso3c"))
      foodNutDim$country.name.en <- foodNutDim$country.name.en %>% as.character
      
      foodNutDim <- foodNutDim[-which(apply(X = foodNutDim[,3:ncol(foodNutDim)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 13),]
      rownames(foodNutDim) <- foodNutDim$country.name.en
      foodNutDim$country.name.en <- NULL
      write.csv(foodNutDim, paste0(data_path, "/dimensions/indicators/food_nutrition_dimension.csv"), row.names = T)
      
      rm(AverageFoodSupply, FoodConsumption, city_access, improved_water, access_electricity, price_volatility,
         fsvar, food_loss, diet_div, stunting, obesity, foodBorne_illness, serum_retinol)
      
    } else {
      food_nutritionDim <- read.csv(paste0(data_path, "/dimensions/indicators/food_nutrition_dimension.csv"), row.names = 1)
      if(!file.exists(paste0(data_path,"/descriptive_graphs/indicators/food_nutrition/food_nutrition_pca.png"))){
        df <- food_nutritionDim[complete.cases(food_nutritionDim),]
        fnt_pca <- df[,-1] %>% FactoMineR::PCA(scale.unit = T, graph = F)
        fnt_pca %>% factoextra::fviz_pca_biplot(repel = TRUE) +
          ggplot2::theme_bw() +
          ggplot2::ggsave(filename = paste0(data_path,"/descriptive_graphs/indicators/food_nutrition/food_nutrition_pca.png"), units = "in", width = 12, height = 8)
      }
    }
    
  } else {
    
    cat("    1.2. Intermediate target files already exists: loading them ...\n")
    
    # Load country codes list
    country_codes <- read.csv(paste0(data_path, "/inputs_raw/country_codes.csv"))
    
    Dimensions <- c("environmental", "economic", "social", "food_nutrition")
    grep2      <- Vectorize(grep, vectorize.args = "pattern") %>% unlist
    listed_fls <- listed_fls[grep2(Dimensions, listed_fls)]; rm(grep2)
    loaded_tbs <- listed_fls %>% purrr::map(., function(.){read.csv(., row.names = 1)}); rm(listed_fls)
    names(loaded_tbs) <- Dimensions; rm(Dimensions)
    
    all_data <- dplyr::left_join(x = country_codes %>% dplyr::select(iso3c), y = loaded_tbs[["environmental"]], by = "iso3c")
    all_data <- dplyr::left_join(x = all_data, y = loaded_tbs[["economic"]], by = "iso3c")
    all_data <- dplyr::left_join(x = all_data, y = loaded_tbs[["social"]], by = "iso3c")
    all_data <- dplyr::left_join(x = all_data, y = loaded_tbs[["food_nutrition"]], by = "iso3c")
    all_data <- all_data[-which(apply(X = all_data[,2:ncol(all_data)], MARGIN = 1, FUN = function(x) sum(is.na(x))) == 27),]
    
    all_data <- dplyr::right_join(x = country_codes %>% dplyr::select(country.name.en, iso3c), y = all_data, by = "iso3c")
    rownames(all_data) <- all_data$country.name.en; all_data$country.name.en <- NULL
    rm(loaded_tbs, country_codes)
    if(!file.exists(paste0(data_path,"/outputs/indicators/sfs_raw_indicators.csv"))){
      write.csv(x = all_data, file = paste0(data_path,"/outputs/indicators/sfs_raw_indicators.csv"), row.names = T)
    }
    
  }
  
  return(cat("SFS indicators compiled successfully!\n"))
  
}
generate_indicators_tables(data_path = data_path)
rm(generate_indicators_tables, data_path)
