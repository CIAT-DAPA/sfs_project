
sfs_df <- vroom::vroom('D:/ToBackup/sustainable_food_systems/sfs_repo/data/outputs/indicators/sfs_final_index.csv')
sfs_df <- sfs_df %>% dplyr::arrange(SFS_index)
sfs_df <- sfs_df %>%
  dplyr::mutate(quantile = ntile(SFS_index, 4))

names(sfs_df)[1] <- 'country'
names(sfs_df)[8] <- 'cluster'
sfs_df <- sfs_df %>% dplyr::select(country,iso3c,cluster)

write.csv(sfs_df, 'D:/sfs_quartiles.csv', row.names = F)

# Adding GDP
sfs_df$country <- rownames(sfs_df)
sfs_df <- sfs_df %>% dplyr::select(country, iso3c)

dplyr::left_join(sfs_df, data.frame(iso3c = gdp$iso3c, gdp = gdp$Y2017), by = "iso3c") %>%
  dplyr::select(country,iso3c,gdp) %>%
  tidyr::replace_na(replace = list(country='Venezuela',
                                   iso3c='VEN',
                                   gdp=gdp$Y2014[which(gdp$iso3c == 'VEN')])) %>%
  dplyr::mutate(quartile = ntile(gdp, 4)) %>%
  dplyr::select(country, iso3c, quartile) %>%
  dplyr::rename(cluster = quartile) %>%
  readr::write_csv(., 'D:/gdp_quartiles.csv')

dplyr::left_join(sfs_df, data.frame(iso3c = gdp$iso3c, gdp = gdp$Y2017), by = "iso3c") %>%
  dplyr::select(iso3c,gdp) %>%
  tidyr::replace_na(replace = list(iso3c='VEN',
                                   gdp=gdp$Y2014[which(gdp$iso3c == 'VEN')])) %>%
  dplyr::mutate(quartile = ntile(gdp, 3)) %>%
  dplyr::select(iso3c, quartile) %>%
  dplyr::rename(cluster = quartile) %>%
  readr::write_csv(., 'D:/gdp_terciles.csv')
