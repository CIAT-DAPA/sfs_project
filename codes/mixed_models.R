# Consolidating new dataset SFS indices over time (97 countries)
# H. Achicanoy
# CIAT, 2020

options(warn = -1, scipen = 999)
suppressPackageStartupMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, vroom, dtw, proxy, factoextra, parallelDist, dendextend))

root <- 'D:/ToBackup/sustainable_food_systems/sfs_repo/data/outputs/indicators/SFS_index_over_time'
cntr <- vroom::vroom('D:/ToBackup/sustainable_food_systems/sfs_repo/data2020/inputs_raw/country_codes.csv', delim = ',')

gdpt <- readxl::read_excel(paste0(root,'/SFS_and_GDP_dynamics.xlsx'), sheet = 'gdp_per_capita_2010_us_dollars')
gdpt <- gdpt %>%
  dplyr::mutate(GDP2000_2003 = gdpt %>% dplyr::select('2000':'2003') %>% rowMeans(na.rm = T)) %>%
  dplyr::mutate(GDP2004_2006 = gdpt %>% dplyr::select('2004':'2006') %>% rowMeans(na.rm = T)) %>%
  dplyr::mutate(GDP2007_2009 = gdpt %>% dplyr::select('2007':'2009') %>% rowMeans(na.rm = T)) %>%
  dplyr::mutate(GDP2010_2012 = gdpt %>% dplyr::select('2010':'2012') %>% rowMeans(na.rm = T)) %>%
  dplyr::mutate(GDP2013_2016 = gdpt %>% dplyr::select('2013':'2016') %>% rowMeans(na.rm = T)) %>%
  dplyr::select(iso3c, GDP2000_2003:GDP2013_2016)
gdpt <- gdpt %>%
  tidyr::pivot_longer(cols = GDP2000_2003:GDP2013_2016,
                      names_to = 'Period',
                      values_to = 'GDP')
gdpt$Period <- gsub('GDP','',gdpt$Period)
gdpt$Period <- gsub('_','-',gdpt$Period)

tpnt <- list.dirs(root, full.names = F, recursive = F) %>%
  grep('^[0-9]', ., value = T)

inot <- tpnt %>%
  purrr::map(.x = ., .f = function(x){
    
    tbl <- vroom::vroom(paste0(root,'/',x,'/outputs/sfs_final_index.csv'), delim = ',')
    tbl <- dplyr::left_join(x = cntr %>%
                              dplyr::select(iso3c, country.name.en),
                            y = tbl,
                            by = 'iso3c')
    tbl <- tbl %>%
      tidyr::pivot_longer(cols = Environment:SFS_index,
                          names_to = 'Class',
                          values_to = 'Value') %>%
      dplyr::mutate(Period = x)
    return(tbl)
    
  }) %>%
  do.call(rbind,.) %>%
  tidyr::drop_na()

df <- inot %>%
  tidyr::pivot_wider(names_from = Class, values_from = Value)
cl1 <- vroom::vroom('D:/sfs_pca_clustering.csv',delim=',')
names(cl1)[3] <- 'cl_pc'
cl2 <- vroom::vroom('D:/sfs_ts_clustering.csv',delim=',')
names(cl2)[3] <- 'cl_ts'
cl3 <- vroom::vroom('D:/sfs_quartiles.csv',delim=',')
names(cl3)[3] <- 'qr_ix'
cl4 <- vroom::vroom('D:/gdp_quartiles.csv',delim=',')
names(cl4)[3] <- 'qr_gp'

df <- dplyr::left_join(x = df, y = cl1, by = c('iso3c','country.name.en' = 'country'))
df <- dplyr::left_join(x = df, y = cl2, by = c('iso3c','country.name.en' = 'country'))
df <- dplyr::left_join(x = df, y = cl3, by = c('iso3c','country.name.en' = 'country'))
df <- dplyr::left_join(x = df, y = cl4, by = c('iso3c','country.name.en' = 'country'))

readr::write_csv(df, 'D:/df_multilevel.csv')
df <- readr::read_csv('D:/df_multilevel.csv')
cl5 <- vroom::vroom('D:/gdp_terciles.csv',delim=',')
names(cl5)[2] <- 'tr_gp'
df <- dplyr::left_join(x = df, y = cl5, by = c('iso3c'))

str(df)
df$iso3c <- factor(df$iso3c)
df$country.name.en <- factor(df$country.name.en)
df$Period <- factor(df$Period, levels=unique(df$Period), ordered=T)
df$cl_pc <- factor(df$cl_pc)
df$cl_ts <- factor(df$cl_ts)
df$qr_ix <- factor(df$qr_ix)
df$qr_gp <- factor(df$qr_gp)
df$tr_gp <- factor(df$tr_gp)

# Load GDP over time data for all periods
root <- 'D:/ToBackup/sustainable_food_systems/sfs_repo/data2020/outputs/indicators/SFS_index_over_time'
gdpt <- readxl::read_excel(paste0(root,'/SFS_and_GDP_dynamics.xlsx'), sheet = 'gdp_per_capita_2010_us_dollars')
gdpt <- gdpt %>%
  dplyr::mutate(GDP2000_2003 = gdpt %>% dplyr::select('2000':'2003') %>% rowMeans(na.rm = T)) %>%
  dplyr::mutate(GDP2004_2006 = gdpt %>% dplyr::select('2004':'2006') %>% rowMeans(na.rm = T)) %>%
  dplyr::mutate(GDP2007_2009 = gdpt %>% dplyr::select('2007':'2009') %>% rowMeans(na.rm = T)) %>%
  dplyr::mutate(GDP2010_2012 = gdpt %>% dplyr::select('2010':'2012') %>% rowMeans(na.rm = T)) %>%
  dplyr::mutate(GDP2013_2016 = gdpt %>% dplyr::select('2013':'2016') %>% rowMeans(na.rm = T)) %>%
  dplyr::select(iso3c, GDP2000_2003:GDP2013_2016)
gdpt <- gdpt %>%
  tidyr::pivot_longer(cols = GDP2000_2003:GDP2013_2016,
                      names_to = 'Period',
                      values_to = 'GDP')
gdpt$Period <- gsub('GDP','',gdpt$Period)
gdpt$Period <- gsub('_','-',gdpt$Period)

df <- dplyr::left_join(x = df, y = gdpt, by = c('iso3c','Period'))
df <- df %>% dplyr::filter(Period %in% c("2000-2003","2004-2006","2007-2009","2010-2012"))
gdp_avg <- df %>%
  dplyr::group_by(country.name.en,iso3c) %>%
  dplyr::summarise(GDP_mean = mean(GDP))
df <- dplyr::left_join(x = df, y = gdp_avg, by = c('iso3c','country.name.en'))
rm(gdp_avg, cntr, cl5, gdpt)

write.csv(df, 'D:/df_multilevel_new.csv', row.names = F)
df <- read.csv('D:/df_multilevel_new.csv')
df$iso3c           <- factor(df$iso3c)
df$country.name.en <- factor(df$country.name.en)
df$Period          <- factor(df$Period, levels=unique(df$Period), ordered=T)
df$cl_pc           <- factor(df$cl_pc)
df$cl_ts           <- factor(df$cl_ts)
df$qr_ix           <- factor(df$qr_ix)
df$qr_gp           <- factor(df$qr_gp)
df$tr_gp           <- factor(df$tr_gp)

# Benchmark model
fit0 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)+(1|country.name.en), data = df, REML = T)
# Benchmark model + log GDP average
fit1 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)+log(GDP_mean)+(1|country.name.en), data = df, REML = T)
# Compare fit0 vs fit1
anova(fit0, fit1)
# Benchmark model + log GDP 4 points
fit2 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)+log(GDP)+(1|country.name.en), data = df, REML = T)
# Compare fit0 vs fit2
anova(fit0, fit2)
# Benchmark model + interactions among dimensions
fit3 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(1|country.name.en), data = df, REML = T)
# Compare fit0 vs fit3
anova(fit0, fit3)
# Benchmark model 3 + random slope by GDP terciles
# fit4 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(1+tr_gp|country.name.en), data = df, REML = T)
fit4 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(tr_gp||country.name.en), data = df, REML = T)
# Compare fit3 vs fit4
anova(fit3, fit4)
# Benchmark model 3 + random slope by PCA clusters
fit5.1 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(cl_pc||country.name.en), data = df, REML = T)
# fit5.2 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(1+cl_pc|country.name.en), data = df, REML = T)
# Compare fit3 vs fit5
anova(fit3, fit5.1)
anova(fit4, fit5.1)
# Benchmark model 3 + random slope by PCA clusters + log(GDP) time series
fit6 <- lme4::lmer(SFS_index~(Economic+Social+Food_nutrition)^2+log(GDP)+(cl_pc||country.name.en), data = df, REML = T)
anova(fit5.1, fit6)

# Extra calculation Chris
fit3.tr_gp1 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(1|country.name.en),
                          data = df %>% dplyr::filter(tr_gp == 1),
                          REML = T)
fit3.cl_pc1 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(1|country.name.en),
                          data = df %>% dplyr::filter(cl_pc == 1),
                          REML = T)

fit3.tr_gp2 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(1|country.name.en),
                          data = df %>% dplyr::filter(tr_gp == 2),
                          REML = T)
fit3.cl_pc2 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(1|country.name.en),
                          data = df %>% dplyr::filter(cl_pc == 2),
                          REML = T)

fit3.tr_gp3 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(1|country.name.en),
                          data = df %>% dplyr::filter(tr_gp == 3),
                          REML = T)
fit3.cl_pc3 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(1|country.name.en),
                          data = df %>% dplyr::filter(cl_pc == 3),
                          REML = T)

# New adjustments Chris
fit5.1.cl_pc1 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(log(GDP)||country.name.en),
                            data = df %>% dplyr::filter(cl_pc == 1),
                            REML = T)
fit5.1.cl_pc2 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(log(GDP)||country.name.en),
                            data = df %>% dplyr::filter(cl_pc == 2),
                            REML = T)
fit5.1.cl_pc3 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(log(GDP)||country.name.en),
                            data = df %>% dplyr::filter(cl_pc == 3),
                            REML = T)

fit1 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(1|cl_pc/country.name.en), data = df, REML = T)
fit2 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(1+log(GDP)|cl_pc/country.name.en), data = df, REML = T)
fit3 <- lme4::lmer(SFS_index~(Environment+Economic+Social+Food_nutrition)^2+(1+GDP|cl_pc/country.name.en), data = df, REML = T)

library(sjPlot)
(re.effects <- plot_model(fit2, type = "re", show.values = TRUE, digits = 5))
summary(fit3)
