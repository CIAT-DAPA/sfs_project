
g <- gc(reset = T); rm(list = ls()); options(scipen = 999, warn = -1)

# Load packages
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(raster, rgdal, maptools, jsonlite, foreach, doParallel, XML, plspm, reshape, tidyverse, countrycode, caret,
                                missMDA, missForest, treemap, viridisLite, highcharter, corrplot, cluster, factoextra, FactoMineR, gghighlight,
                                EnvStats, compiler, caretEnsemble))

# Define data directory
data_path <- "D:/ToBackup/sustainable_food_systems/sfs_repo/data"
# data_path <- "//dapadfs.cgiarad.org/workspace_cluster_9/Sustainable_Food_System/data"

# Load SFS indices and drivers
drivers   <- read.csv(paste0(data_path,"/outputs/drivers/sfs_drivers.csv"), row.names = 1)
sfs_index <- read.csv(paste0(data_path,"/outputs/indicators/sfs_final_index.csv"), row.names = 1)

# Drivers list
drivers_list <- names(drivers)[-1]

# Merge indices and drivers
sfs_df <- dplyr::left_join(x = sfs_index, y = drivers, by = "iso3c")
rownames(sfs_df) <- rownames(sfs_index); rm(sfs_index, drivers)

drivers_old   <- read.csv("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Drivers/dimensions/demand_consumer.csv", row.names = 1)
sfs_index_old <- read.csv("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/old/sfs_index_calculated.csv", row.names = 1)

# Merge indices and drivers
sfs_df_old <- dplyr::left_join(x = sfs_index_old, y = drivers_old, by = "iso3c")
rownames(sfs_df_old) <- rownames(sfs_index_old); rm(sfs_index_old, drivers_old)

drvr <- dplyr::left_join(x = sfs_df_old %>% select(iso3c, chg_empl_services),
                         y = sfs_df %>% select(iso3c, chg_empl_services),
                         by = "iso3c")
drvr %>%
  ggplot(aes(x = chg_empl_services.x, y = chg_empl_services.y)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Old") + 
  ylab("New")

indx <- dplyr::left_join(x = sfs_df_old %>% select(iso3c, SFS_index),
                         y = sfs_df %>% select(iso3c, SFS_index),
                         by = "iso3c")
indx %>%
  ggplot(aes(x = SFS_index.x, y = SFS_index.y)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Old") + 
  ylab("New")

sfs_df %>%
  ggplot(aes(x = chg_empl_services, y = SFS_index)) +
  geom_point() +
  theme_bw() +
  xlim(-50, 25) +
  ylim(0.2, 0.8)

sfs_df_old %>%
  ggplot(aes(x = chg_empl_services, y = SFS_index)) +
  geom_point() +
  theme_bw() +
  xlim(-50, 25) +
  ylim(0.2, 0.8)


sfs_df_old %>% 
  ggplot(aes(x = chg_empl_services, y = SFS_index)) + 
  geom_point() +
  geom_point(data = sfs_df_old %>%filter(iso3c == "ETH"), aes(x = chg_empl_services, y = SFS_index), colour = "red")

sfs_df %>% 
  ggplot(aes(x = chg_empl_services, y = SFS_index)) + 
  geom_point() +
  geom_point(data = sfs_df %>%filter(iso3c == "ETH"), aes(x = chg_empl_services, y = SFS_index), colour = "red")

cor(sfs_df_old$chg_empl_services, sfs_df_old$SFS_index, method = "spearman", use = "pairwise.complete.obs")
cor(sfs_df$chg_empl_services, sfs_df$SFS_index, method = "spearman", use = "pairwise.complete.obs")

calc_correlation(df = sfs_df, driver = "chg_empl_services")
calc_correlation(df = sfs_df_old, driver = "chg_empl_services")

cor(sfs_df$chg_empl_services, sfs_df_old$SFS_index, method = "spearman", use = "pairwise.complete.obs")
plot(sfs_df$chg_empl_services, sfs_df_old$SFS_index, xlim = c(-50, 25), ylim = c(0.2, 0.8), pch = 20)

test2 <- data.frame(chg_empl_services = sfs_df$chg_empl_services, SFS_index = sfs_df_old$SFS_index)
test2 <- test2 %>% drop_na()
test2 <- test2 %>% arrange(chg_empl_services)

cor(test2$chg_empl_services[-(1:2)],test2$SFS_index[-(1:2)], method = "spearman")

cor(sfs_df$chg_empl_services[-which(sfs_df$iso3c %in% c("ETH", "DZA", "PER"))],
    sfs_df$SFS_index[-which(sfs_df$iso3c %in% c("ETH", "DZA", "PER"))],
    method = "spearman", use = "pairwise.complete.obs")
cor(sfs_df_old$chg_empl_services[-which(sfs_df_old$iso3c %in% c("ETH", "DZA", "PER"))],
    sfs_df_old$SFS_index[-which(sfs_df_old$iso3c %in% c("ETH", "DZA", "PER"))],
    method = "spearman", use = "pairwise.complete.obs")
cor(sfs_df_old$chg_empl_services[-which(sfs_df_old$iso3c %in% c("DZA", "PER"))],
    sfs_df_old$SFS_index[-which(sfs_df_old$iso3c %in% c("DZA", "PER"))],
    method = "spearman", use = "pairwise.complete.obs")


plot(empl_services[empl_services$iso3c == "ETH",41:55] %>% as.numeric(), ty = "l")


cor.test(sfs_df$chg_empl_services[-which(sfs_df$iso3c %in% c("ETH"))],
    sfs_df$SFS_index[-which(sfs_df$iso3c %in% c("ETH"))],
    method = "spearman", use = "pairwise.complete.obs")

cor.test(sfs_df$chg_empl_services[-which(sfs_df$iso3c %in% c("ETH", "DZA", "PER"))],
    sfs_df$SFS_index[-which(sfs_df$iso3c %in% c("ETH", "DZA", "PER"))],
    method = "spearman", use = "pairwise.complete.obs")
