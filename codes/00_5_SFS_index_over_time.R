library(tidyverse)

sfs_ts <- read.csv('D:/SFS_index_over_time/sfs_final_index_over_time.csv', row.names = 1)
sfs_ts <- sfs_ts[,1:6]
sfs_ts$Country <- rownames(sfs_ts)
sfs_ts <- sfs_ts %>% tidyr::gather(key = Period, value = SFS_index, c(-iso3c, -Country))
sfs_ts$Period <- sfs_ts$Period %>% gsub("SFS_", "", .) %>% gsub(".", "-", ., fixed = T)

dza <- sfs_ts %>% 
  dplyr::filter(Country %in% "Algeria") %>% 
  ggplot2::ggplot(aes(x = Period, y = SFS_index, group = 1)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::ylab("Country food system sustainability score") +
  ggplot2::theme_bw() +
  ggplot2::ylim(.13, .34)

chl <- sfs_ts %>% 
  dplyr::filter(Country %in% "Chile") %>% 
  ggplot2::ggplot(aes(x = Period, y = SFS_index, group = 1)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::ylab("Country food system sustainability score") +
  ggplot2::theme_bw() +
  ggplot2::ylim(.47, .58)

ind <- sfs_ts %>% 
  dplyr::filter(Country %in% "India") %>% 
  ggplot2::ggplot(aes(x = Period, y = SFS_index, group = 1)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::ylab("Country food system sustainability score") +
  ggplot2::theme_bw() +
  ggplot2::ylim(.29, .33)

tgo <- sfs_ts %>% 
  dplyr::filter(Country %in% "Togo") %>% 
  ggplot2::ggplot(aes(x = Period, y = SFS_index, group = 1)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::ylab("Country food system sustainability score") +
  ggplot2::theme_bw() +
  ggplot2::ylim(.43, .51)

library(ggpubr)
tsv <- ggarrange(dza, chl, ind, tgo, labels = c('Algeria','Chile','India','Togo'), ncol = 1, hjust = -1.5, vjust = 2)
ggplot2::ggsave("D:/SFS_index_over_time/SFS_index_four_countries.pdf", tsv, units = 'in', width = 6, height = 14)
