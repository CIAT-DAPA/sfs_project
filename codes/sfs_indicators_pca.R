library(tidyverse)
library(FactoMineR)
library(factoextra)

all_data <- read.csv('D:/ToBackup/sustainable_food_systems/sfs_repo/data2020/outputs/indicators/sfs_raw_indicators.csv', row.names = 1) 
selected <- c("Emissions.agriculture.total","Water.withdrawal","Soil.carbon.content","Arable.land","GEF.benefits.biodiversity","Crop.diversity",
              "AgValueAdded",
              "Female.labor.force",
              "Food.available","Food.consumption","City.access","Access.improved.water","Access.electricity","Price.volatility.index","Food.supply.variability","Foodborne.illness","Food.loss","Diet.diversification","Obesity","Serum.retinol.deficiency")
selected2 <- c("Emissions.agriculture.total","Water.withdrawal","Soil.carbon.content","Arable.land","GEF.benefits.biodiversity","Crop.diversity",
              "AgValueAdded","Agr.employment",
              "Female.labor.force","Fuels.cooking",
              "Food.available","Food.consumption","City.access","Access.improved.water","Access.electricity","Price.volatility.index","Food.supply.variability","Foodborne.illness","Food.loss","Diet.diversification","Obesity","Serum.retinol.deficiency")
df <- all_data[,selected]
df <- df[complete.cases(df),]

df2 <- all_data[,selected2]
df2 <- df2[complete.cases(df2),]

pca_ <- df %>%
  FactoMineR::PCA(X = ., scale.unit = T, graph = F)

# Explained variance
pca_ %>%
  fviz_eig(., addlabels = TRUE, ylim = c(0, 50))

# Variable importance per component
corrplot::corrplot(pca_$var$cos2, is.corr = FALSE)

# Color by cos2 values: quality on the factor map
pca_ %>%
  fviz_pca_var(., col.var = "cos2",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
               repel = TRUE # Avoid text overlapping
  )

pca_ %>%
  fviz_pca_ind(., pointsize = "cos2", 
               pointshape = 21, fill = "#E7B800",
               repel = TRUE # Avoid text overlapping (slow if many points)
  )

hcpc_ <- pca_ %>%
  HCPC(., graph = FALSE)

hcpc_ %>%
  fviz_dend(., 
            cex = 0.7,                     # Label size
            palette = "jco",               # Color palette see ?ggpubr::ggpar
            rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
            rect_border = "jco",           # Rectangle color
            labels_track_height = 0.8      # Augment the room for labels
  )

hcpc_ %>%
  fviz_cluster(.,
               repel = TRUE,            # Avoid label overlapping
               show.clust.cent = TRUE, # Show cluster centers
               palette = "jco",         # Color palette see ?ggpubr::ggpar
               ggtheme = theme_minimal(),
               main = "Factor map"
  )

pca_clustering <- data.frame(country = rownames(hcpc_$data.clust),
                             iso3c   = all_data$iso3c[match(rownames(df), rownames(all_data))],
                             cluster = hcpc_$data.clust$clust)
write.csv(pca_clustering, 'D:/sfs_pca_clustering.csv', row.names = F)
