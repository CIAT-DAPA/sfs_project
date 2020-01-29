# Number of countries per income category
sfs_data <- read.csv("D:/ToBackup/sustainable_food_systems/sfs_repo/data/outputs/indicators/sfs_final_index.csv")
countries <- read.csv("D:/countries_chris.csv")

library(tidyverse)

countries2 <- countries %>% dplyr::filter(iso3c %in% sfs_data$iso3c)
table(countries2$income)
table(countries2$income)/sum(table(countries2$income))

# Regression model
fit <- lm(SFS_index ~ ., data = sfs_df %>% dplyr::select(SFS_index:chg_urban_pop,
                                                         chg_empl_services:chg_access_infr,
                                                         land_degradation:chg_food_safety_cncr))
fit %>% summary()

fit2 <- lm(SFS_index ~ ., data = sfs_df %>% dplyr::select(SFS_index:chg_access_infr,land_degradation:chg_food_safety_cncr))
fit2 %>% summary()

library(imputeTS)
indx_drvs <- sfs_df %>% dplyr::select(SFS_index:chg_urban_pop,
                         chg_empl_services:chg_access_infr,
                         land_degradation:chg_food_safety_cncr)
indx_drvs <- imputeTS::na_mean(indx_drvs)

fit <- lm(SFS_index ~ ., data = indx_drvs)
fit %>% summary()

# Cluster analysis

df1 <- sfs_df %>% dplyr::select(SFS_index:chg_urban_pop,
                         chg_empl_services:chg_access_infr,
                         land_degradation:chg_food_safety_cncr)

df2 <- sfs_df %>% dplyr::select(chg_diet_health_attn:chg_urban_pop,
                                chg_empl_services:chg_access_infr,
                                land_degradation:chg_food_safety_cncr)

df1 %>% dplyr::select(SFS_index) %>% dist() %>% hclust(method = "ward.D2") %>% plot(hang = -1, cex = 0.6)
df2 %>% dist() %>% hclust(method = "ward.D2") %>% plot(hang = -1, cex = 0.6)

df1 %>% apply(., 2, function(x) sum(is.na(x))) %>% sort(decreasing = T)


df1 %>% FactoMineR::PCA(graph = F) %>%
  factoextra::fviz_screeplot(addlabels = TRUE, ylim = c(0, 50))

test <- df1 %>% FactoMineR::PCA(graph = F)
test2 <- df1 %>% FactoMineR::PCA(graph = F, quanti.sup = 1)
test$ind$cos2 %>% View()

df1 %>% FactoMineR::PCA(graph = F, quanti.sup = 1) %>%
  factoextra::fviz_pca_var(col.var="cos2",
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                           repel = TRUE # Avoid text overlapping
  )


df2 %>% FactoMineR::PCA(graph = F) %>%
  factoextra::fviz_screeplot(addlabels = TRUE, ylim = c(0, 50))

df2 %>% FactoMineR::PCA(graph = F) %>%
  factoextra::fviz_pca_var(col.var="cos2",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE # Avoid text overlapping
  )
df2 %>% FactoMineR::PCA(graph = F) %>% FactoMineR::HCPC(nb.clust = -1, graph = F) %>%
  factoextra::fviz_cluster(.,
               repel = TRUE,            # Avoid label overlapping
               show.clust.cent = TRUE, # Show cluster centers
               palette = "jco",         # Color palette see ?ggpubr::ggpar
               ggtheme = theme_minimal(),
               main = "Factor map"
  )

cat('Use the script called: 01_4_SDrivers_calculating_correlations.R\n')

countries <- read.csv("D:/countries_chris.csv")
sfs_df <- dplyr::left_join(x = sfs_df, y = countries %>% dplyr::select(iso3c,Income), by = 'iso3c')
levels(sfs_df$Income)[3:4] <- 'Middle income'
sfs_df$Income <- factor(sfs_df$Income, levels = c('High income','Middle income','Low income'))

gp2 <- jcknf_smpl %>%
  ggplot2::ggplot(aes(x, y, group = id)) +
  ggplot2::geom_line(alpha = 0.5, colour = "dodgerblue2") +
  ggplot2::geom_point(data = df_fltr_updt %>% mutate(id = 0), aes(x, y, colour = Income)) +
  ggplot2::xlab(label) +
  ggplot2::ylab("Country food system sustainability scores") +
  ggplot2::ylim(.20, .80) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.title = element_blank()) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.title   = element_text(size = 15),
                 axis.text    = element_text(size = 13),
                 legend.title = element_blank(),
                 legend.text  = element_text(size = 13))
gp2
