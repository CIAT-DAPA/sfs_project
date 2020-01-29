library(tidyverse)

# ------------------------------------------------------------------ #
# Baseline data
# ------------------------------------------------------------------ #

sfs_data <- read.csv('D:/ToBackup/sustainable_food_systems/sfs_repo/data/outputs/indicators/sfs_raw_indicators.csv', row.names=1)

# ------------------------------------------------------------------ #
# New data
# ------------------------------------------------------------------ #

# Total factor productivity
tfp_data <- readxl::read_excel('//dapadfs/Workspace_cluster_9/Sustainable_Food_System/economic_and_social_indicators/datasets/organized/Agricultural_total_factor_productivity.xlsx',sheet='Data cleaned')
tfp_data <- tfp_data %>% dplyr::select(iso3c, TFP_growth)

# Clean fuels for cooking
ffc_data <- readxl::read_excel('//dapadfs/Workspace_cluster_9/Sustainable_Food_System/economic_and_social_indicators/datasets/organized/Clean_fuels_for_cooking.xls',sheet='Data cleaned')
ffc_data <- ffc_data %>% dplyr::select(iso3c, Y2016)
names(ffc_data)[2] <- 'Fuels.cooking'

# Literacy rate adults
lra_data <- readxl::read_excel('//dapadfs/Workspace_cluster_9/Sustainable_Food_System/economic_and_social_indicators/datasets/organized/Literacy_rate_adults.xls',sheet='Data cleaned')
lra_data$Lit.rate <- lra_data %>% apply(X = .[,paste0('Y',2009:2018)], MARGIN = 1, FUN = median, na.rm = T)
lra_data <- lra_data %>% dplyr::select(iso3c, Lit.rate)

# Women seats in national parliaments
wnp_data <- readxl::read_excel('//dapadfs/Workspace_cluster_9/Sustainable_Food_System/economic_and_social_indicators/datasets/organized/Women_seats_national_parliaments.xls',sheet='Data cleaned')
wnp_data <- wnp_data %>% dplyr::select(iso3c, Y2018)
names(wnp_data)[2] <- 'Women.parliaments'

# ------------------------------------------------------------------ #
# Reviewing counts for all indicators
# ------------------------------------------------------------------ #

# Baseline: 17 countries
sum(complete.cases(sfs_data[,2:28]))

# Including Total factor productivity: 15 countries
sum(complete.cases(dplyr::left_join(sfs_data, tfp_data, by = 'iso3c') %>% .[,2:ncol(.)]))

# Including Clean fuels for cooking: 17 countries
sum(complete.cases(dplyr::left_join(sfs_data, ffc_data, by = 'iso3c') %>% .[,2:ncol(.)]))

# Including Literacy rate adults: 15 countries
sum(complete.cases(dplyr::left_join(sfs_data, lra_data, by = 'iso3c') %>% .[,2:ncol(.)]))

# Including Women seats in national parliaments: 17 countries
sum(complete.cases(dplyr::left_join(sfs_data, wnp_data, by = 'iso3c') %>% .[,2:ncol(.)]))

# ------------------------------------------------------------------ #
# Reviewing counts for selected indicators
# ------------------------------------------------------------------ #

frontier_df <- readRDS("D:/ToBackup/sustainable_food_systems/sfs_repo/data/outputs/indicators/edge_frontier/all_maximum_combinations_tibble.RDS")
frontier_df_fltrd <- frontier_df %>% dplyr::filter(max == 1)
frontier_df_fltrd <- frontier_df_fltrd %>% dplyr::arrange(nIndicators); rm(frontier_df)
selected <- frontier_df_fltrd$indicators_list[24][[1]]

# Baseline: 97 countries
sum(complete.cases(sfs_data[,selected]))

# Including Total factor productivity: 69 countries
sum(complete.cases(dplyr::left_join(sfs_data[,c('iso3c',selected)], tfp_data, by = 'iso3c') %>% .[,2:ncol(.)]))

# Including Clean fuels for cooking: 96 countries
sum(complete.cases(dplyr::left_join(sfs_data[,c('iso3c',selected)], ffc_data, by = 'iso3c') %>% .[,2:ncol(.)]))

# Including Literacy rate adults: 75 countries
sum(complete.cases(dplyr::left_join(sfs_data[,c('iso3c',selected)], lra_data, by = 'iso3c') %>% .[,2:ncol(.)]))

# Including Women seats in national parliaments: 97 countries
sum(complete.cases(dplyr::left_join(sfs_data[,c('iso3c',selected)], wnp_data, by = 'iso3c') %>% .[,2:ncol(.)]))

# ------------------------------------------------------------------ #
# Reviewing counts economic + social
# ------------------------------------------------------------------ #

# Including economic + Clean fuels for cooking: 15 countries
sum(complete.cases(dplyr::left_join(dplyr::left_join(sfs_data, tfp_data, by = 'iso3c'), ffc_data, by = 'iso3c') %>% .[,2:ncol(.)]))

# Including economic + Literacy rate adults: 15 countries
sum(complete.cases(dplyr::left_join(dplyr::left_join(sfs_data, tfp_data, by = 'iso3c'), lra_data, by = 'iso3c') %>% .[,2:ncol(.)]))

# Including economic + Women seats in national parliaments: 15 countries
sum(complete.cases(dplyr::left_join(dplyr::left_join(sfs_data, tfp_data, by = 'iso3c'), wnp_data, by = 'iso3c') %>% .[,2:ncol(.)]))



# Including economic + Clean fuels for cooking: 68 countries
sum(complete.cases(dplyr::left_join(dplyr::left_join(sfs_data[,c('iso3c',selected)], tfp_data, by = 'iso3c'), ffc_data, by = 'iso3c') %>% .[,2:ncol(.)]))

# Including economic + Literacy rate adults: 66 countries
sum(complete.cases(dplyr::left_join(dplyr::left_join(sfs_data[,c('iso3c',selected)], tfp_data, by = 'iso3c'), lra_data, by = 'iso3c') %>% .[,2:ncol(.)]))

# Including economic + Women seats in national parliaments: 69 countries
sum(complete.cases(dplyr::left_join(dplyr::left_join(sfs_data[,c('iso3c',selected)], tfp_data, by = 'iso3c'), wnp_data, by = 'iso3c') %>% .[,2:ncol(.)]))


# New counts
sum(complete.cases(dplyr::left_join(sfs_data[,c('iso3c',selected,'Agr.employment','Fairtrade.ctg')], ffc_data, by = 'iso3c') %>% .[,2:ncol(.)]))
sum(complete.cases(dplyr::left_join(sfs_data[,c('iso3c',selected,'Agr.employment')], ffc_data, by = 'iso3c') %>% .[,2:ncol(.)]))
sum(complete.cases(dplyr::left_join(sfs_data[,c('iso3c',selected,'Fairtrade.ctg')], ffc_data, by = 'iso3c') %>% .[,2:ncol(.)]))

final_countries <- dplyr::left_join(sfs_data[,c('iso3c',selected,'Agr.employment')], ffc_data, by = 'iso3c')[complete.cases(dplyr::left_join(sfs_data[,c('iso3c',selected,'Fairtrade.ctg')], ffc_data, by = 'iso3c') %>% .[,2:ncol(.)]),]$iso3c
prevs_countries <- sfs_data[,c('iso3c',selected)][complete.cases(sfs_data[,selected]),]$iso3c %>% as.character

base::setdiff(prevs_countries,final_countries)
