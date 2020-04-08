# SFS index over time (22 indicators)
# H. Achicanoy
# CIAT, 2020

options(warn = -1, scipen = 999)
suppressPackageStartupMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, vroom, dtw, proxy, factoextra, parallelDist, dendextend))

# Root directory
root <- 'D:/ToBackup/sustainable_food_systems/sfs_repo/data2020/outputs/indicators/SFS_index_over_time'
# Country codes
cntr <- vroom::vroom('D:/ToBackup/sustainable_food_systems/sfs_repo/data2020/inputs_raw/country_codes.csv', delim = ',')
# Load GDP over time data for all periods
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

# Load SFS index over time for all periods
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

# Merge SFS index with GDP data over time
inot_gdp <- dplyr::left_join(x = inot, y = gdpt, by = c('iso3c','Period'))
inot_gdp$Period <- factor(inot_gdp$Period, levels = c('2000-2003','2004-2006','2007-2009','2010-2012','2013-2016'), ordered = T)
inot_gdp$Order  <- NA
inot_gdp$Order[inot_gdp$Period == '2000-2003'] <- 1
inot_gdp$Order[inot_gdp$Period == '2004-2006'] <- 2
inot_gdp$Order[inot_gdp$Period == '2007-2009'] <- 3
inot_gdp$Order[inot_gdp$Period == '2010-2012'] <- 4
inot_gdp$Order[inot_gdp$Period == '2013-2016'] <- 5
# Filter by the first 4 periods
inot_gdp <- inot_gdp %>%
  dplyr::filter(Order %in% 1:4)
inot_gdp$Period <- factor(inot_gdp$Period, levels = c('2000-2003','2004-2006','2007-2009','2010-2012'), ordered = T)
inot_gdp$Order  <- factor(inot_gdp$Order, levels = 1:4, ordered = T)

# GDP vs Indices + component over time
index_vs_gdp <- function(df = inot_gdp, out = 'D:/GPD_vs_indices', GDP_log = F){
  
  info2plots <- data.frame(Class = c('SFS_index','Environment','Economic','Social','Food_nutrition'),
                           Title = paste0(c('SFS index','Environment','Economic','Social','Food and nutrition'), ' vs GDP per capita 2010 (US$)'),
                           Descr = c('Country food system sustainability scores', paste0(c('Environment','Economic','Social','Food and nutrition'),' scores')))
  
  for(i in 1:nrow(info2plots)){
    gp2 <- df %>%
      dplyr::filter(Class == info2plots$Class[i]) %>%
      ggplot(aes(x = GDP, y = Value, colour = Period, group = country.name.en)) +
      geom_point() +
      geom_path() +
      ylim(0, 1) +
      scale_colour_brewer(palette = 'Set1') +
      ggplot2::xlab("GDP per capita 2010 (US$)") +
      ggplot2::ylab(info2plots$Descr[i]) +
      ggplot2::labs(title = info2plots$Title[i]) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title   = element_text(size = 15),
                     axis.text    = element_text(size = 13),
                     legend.title = element_blank(),
                     legend.text  = element_text(size = 13))
    ggplot2::ggsave(filename = paste0(out, '/GDP_vs_', info2plots$Class[i], '_index_over_time.png'), plot = gp2, device = "png", units = "in", width = 12, height = 8)
    if(GDP_log){
      gp2 <- gp2 + ggplot2::scale_x_log10()
      ggplot2::ggsave(filename = paste0(out, '/Log_GDP_vs_', info2plots$Class[i], '_index_over_time.png'), plot = gp2, device = "png", units = "in", width = 12, height = 8)
    }
    
  }
  
}
index_vs_gdp(df = inot_gdp, out = 'D:/GPD_vs_indices2020', GDP_log = T)



gp2 <- inot_gdp %>%
  dplyr::filter(Class == 'Economic') %>%
  ggplot(aes(x = GDP, y = Value, colour = Period, group = country.name.en)) +
  geom_point() +
  geom_path() +
  ylim(0, 1) +
  scale_colour_brewer(palette = 'Set1') +
  ggplot2::xlab("GDP per capita 2010 (US$)") +
  ggplot2::ylab("Country food system sustainability scores") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title   = element_text(size = 15),
                 axis.text    = element_text(size = 13),
                 legend.title = element_blank(),
                 legend.text  = element_text(size = 13))
ggplot2::ggsave(filename = 'D:/Economic_index_over_time.png', plot = gp2, device = "png", units = "in", width = 12, height = 8)

gp2 <- inot_gdp %>%
  dplyr::filter(Class == 'SFS_index') %>%
  ggplot(aes(x = log(GDP), y = Value, colour = Period, group = country.name.en)) +
  geom_point() +
  geom_path() +
  ylim(0, 1) +
  scale_colour_brewer(palette = 'Set1') +
  ggplot2::xlab("log(GDP per capita 2010 (US$))") +
  ggplot2::ylab("Country food system sustainability scores") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title   = element_text(size = 15),
                 axis.text    = element_text(size = 13),
                 legend.title = element_blank(),
                 legend.text  = element_text(size = 13))
ggplot2::ggsave(filename = 'D:/log_SFS_index_over_time.png', plot = gp2, device = "png", units = "in", width = 12, height = 8)

gp2 <- inot_gdp %>%
  dplyr::filter(Class %in% c('Food_nutrition','SFS_index')) %>%
  tidyr::pivot_wider(names_from = Class, values_from = Value) %>%
  ggplot(aes(x = Food_nutrition, y = SFS_index, colour = Period, group = country.name.en)) +
  geom_point() +
  geom_path() +
  ylim(0, 1) +
  scale_colour_brewer(palette = 'Set1') +
  ggplot2::xlab("Food and nutrition sustainability scores") +
  ggplot2::ylab("Country food system sustainability scores") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title   = element_text(size = 15),
                 axis.text    = element_text(size = 13),
                 legend.title = element_blank(),
                 legend.text  = element_text(size = 13))
ggplot2::ggsave(filename = 'D:/SFS_vs_food_nutrition_index_over_time.png', plot = gp2, device = "png", units = "in", width = 12, height = 8)

ts_countries <- inot_gdp %>%
  dplyr::filter(Class == 'SFS_index') %>%
  dplyr::select(country.name.en, Value, Order) %>%
  dplyr::filter(Order != 5) %>%
  dplyr::group_by(country.name.en) %>%
  dplyr::mutate(SFS_index_nrm = scale(Value)) %>%
  # ggplot2::ggplot(aes(x = Order, y = SFS_index_nrm, colour = country.name.en)) +
  # ggplot2::geom_line()
  dplyr::select(country.name.en, SFS_index_nrm, Order) %>% 
  tidyr::pivot_wider(names_from = Order, values_from = SFS_index_nrm)
rownames(ts_countries) <- ts_countries$country.name.en; ts_countries$country.name.en <- NULL

ts_countries[is.na(ts_countries)] <- 0

hc <- ts_countries %>%
  as.matrix %>%
  dist(., method = "DTW") %>%
  hclust(., method = "ward.D2")

plot(hc, cex = 0.7, hang = -1, col = "blue")
cl <- rect.hclust(hc, 4)

fviz_dend(hc, cex = 0.5,
          k = 3, # Cut in four groups
          palette = "jco" # Color palette
)

plot(ts_countries[which(rownames(ts_countries) == names(cl[[4]][1])),] %>% as.numeric(), ty = "l",
     xlab = "Time index", ylab = "Standardize value", ylim = c(-3,3))
for(i in 2:length(cl[[1]])){
  lines(ts_countries[which(rownames(ts_countries) == names(cl[[4]][i])),] %>% as.numeric(), col = alpha(rgb(0,0,0), 0.5), cex = 0.1)
}

# DTW multivariate
ts_gdp <- inot_gdp %>%
  dplyr::filter(Class == 'SFS_index') %>%
  dplyr::select(country.name.en, GDP, Order) %>%
  dplyr::group_by(country.name.en) %>%
  dplyr::mutate(GDP_nrm = scale(GDP)) %>%
  # ggplot2::ggplot(aes(x = Order, y = SFS_index_nrm, colour = country.name.en)) +
  # ggplot2::geom_line()
  dplyr::select(country.name.en, GDP_nrm, Order) %>% 
  tidyr::pivot_wider(names_from = Order, values_from = GDP_nrm)
rownames(ts_gdp) <- ts_gdp$country.name.en; ts_gdp$country.name.en <- NULL

ts_gdp[is.na(ts_gdp)] <- 0

dtw_mat <- matrix(NA, nrow = nrow(ts_countries), ncol = nrow(ts_countries))
for(i in 1:nrow(ts_countries)){
  for(j in 1:nrow(ts_countries)){
    m1 <- matrix(c(ts_countries[i,] %>% as.numeric,
                   ts_gdp[i,] %>% as.numeric), nrow = 2, byrow = T)
    m2 <- matrix(c(ts_countries[j,] %>% as.numeric,
                   ts_gdp[j,] %>% as.numeric), nrow = 2, byrow = T)
    dtw_mat[i,j] <- parallelDist::parDist(x = list(m1, m2), method = "dtw")
  }
}

rownames(dtw_mat) <- rownames(ts_countries)
colnames(dtw_mat) <- rownames(ts_countries)

hc_mtv <- dtw_mat %>%
  as.dist %>%
  hclust(., method = "ward.D2")

plot(hc_mtv, cex = 0.7, hang = -1, col = "blue")
cl <- rect.hclust(hc, 4)

fviz_dend(hc_mtv, cex = 0.5,
          k = 4, # Cut in four groups
          palette = "jco" # Color palette
)

# Comparing dendrograms
dend_list <- dendlist(as.dendrogram(hc), as.dendrogram(hc_mtv))
dendlist(as.dendrogram(hc), as.dendrogram(hc_mtv)) %>%
  untangle(method = "step1side") %>% # Find the best alignment layout
  tanglegram()

dendlist(as.dendrogram(hc), as.dendrogram(hc_mtv)) %>%
  untangle(method = "step1side") %>% # Find the best alignment layout
  entanglement()

dendlist(as.dendrogram(hc), as.dendrogram(hc_mtv)) %>%
  untangle(method = "step1side") %>% 
  tanglegram(
    highlight_distinct_edges = TRUE, # Turn-off dashed lines
    common_subtrees_color_lines = FALSE, # Turn-off line colors
    common_subtrees_color_branches = TRUE # Color common branches 
  )

cor.dendlist(dend_list, method = "cophenetic")
cor.dendlist(dend_list, method = "baker")
