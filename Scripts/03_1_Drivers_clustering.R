sfs_index_none <- calc_sfs_index(combList = textFile2[[17]], correct_skew = "none", data = all_data, fnt_type = "arithmetic")
sfs_index_log <- calc_sfs_index(combList = textFile2[[17]], correct_skew = "log", data = all_data, fnt_type = "arithmetic")
sfs_index_boxcox <- calc_sfs_index(combList = textFile2[[17]], correct_skew = "box_cox", data = all_data, fnt_type = "arithmetic")

pairs(cbind(sfs_index_none$SFS_index, sfs_index_log$SFS_index, sfs_index_boxcox$SFS_index))
pairs(cbind(sfs_index_none$Environment, sfs_index_log$Environment, sfs_index_boxcox$Environment))
pairs(cbind(sfs_index_none$Economic, sfs_index_log$Economic, sfs_index_boxcox$Economic))
pairs(cbind(sfs_index_none$Social, sfs_index_log$Social, sfs_index_boxcox$Social))
pairs(cbind(sfs_index_none$Food_nutrition, sfs_index_log$Food_nutrition, sfs_index_boxcox$Food_nutrition))

rank_summary <- rep(NA, length(textFile2_uptd))
stdv_summary <- rep(NA, length(textFile2_uptd))
for(i in 1:length(textFile2_uptd)){
  
  ref_vals <- calc_sfs_index(combList = textFile2_uptd[[i]], correct_skew = "box_cox", data = all_data, fnt_type = "arithmetic")
  ref_cntr <- calc_sfs_index(combList = textFile2_uptd[[i]], correct_skew = "box_cox", data = all_data, fnt_type = "arithmetic") %>% rownames %>% sort
  
  snsr_flt <- sensitivity_results[[i]]
  snsr_flt <- snsr_flt %>% dplyr::filter(iso3c %in% ref_cntr & skew_method == "box_cox")
  
  calc_diff <- rep(NA, length(ref_cntr))
  calc_medn <- rep(NA, length(ref_cntr))
  calc_stdv <- rep(NA, length(ref_cntr))
  for(j in 1:length(ref_cntr)){
    calc_diff[j] <- median(snsr_flt[snsr_flt$iso3c == ref_cntr[j], "SFS_index"]) - ref_vals[rownames(ref_vals)==ref_cntr[j],"SFS_index"]
    calc_medn[j] <- median(snsr_flt[snsr_flt$iso3c == ref_cntr[j], "SFS_index"])
    calc_stdv[j] <- sd(snsr_flt[snsr_flt$iso3c == ref_cntr[j], "SFS_index"])
  }
  
  rank_summary[i] <- sum(abs(rank(ref_vals[,"SFS_index"]) - rank(calc_medn)))/length(ref_cntr)
  stdv_summary[i] <- sd(calc_stdv)
  
}

d1_all <- drivers %>% drop_na() %>% select(2:ncol(.)) %>% dist %>% hclust("ward.D2") %>% as.dendrogram()
d1_pca <- db %>% drop_na() %>% dist %>% hclust("ward.D2") %>% as.dendrogram()

dend_list <- dendlist("All variables" = d1_all, "After PCA" = d1_pca)

tanglegram(d1_all, d1_pca)
tanglegram(d1_all, d1_pca,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)
cor.dendlist(dend_list, method = "cophenetic")
cor.dendlist(dend_list, method = "baker")


demand <- demand_consumer %>%
  select(2:ncol(.)) %>%
  filter(rownames(.) %in% rownames(drivers %>% drop_na())) %>%
  FactoMineR::PCA(X = ., scale.unit = T, graph = F) %>% .$ind %>% .$coord %>% data.frame
production <- production_supply %>%
  select(2:ncol(.)) %>%
  filter(rownames(.) %in% rownames(drivers %>% drop_na())) %>%
  FactoMineR::PCA(X = ., scale.unit = T, graph = F) %>% .$ind %>% .$coord %>% data.frame
trade <- trade_distribution %>%
  select(2:ncol(.)) %>%
  filter(rownames(.) %in% rownames(drivers %>% drop_na())) %>%
  FactoMineR::PCA(X = ., scale.unit = T, graph = F) %>% .$ind %>% .$coord %>% data.frame

demand <- demand %>% select(Dim.1)
colnames(demand) <- "Demand"
production <- production %>% select(Dim.1)
colnames(production) <- "Production"
trade <- trade %>% select(Dim.1)
colnames(trade) <- "Trade"

db <- Reduce(merge, lapply(list(demand, production, trade), function(x) data.frame(x, rn = row.names(x))))
db[,-1] %>% cor(., method = "spearman") %>% corrplot::corrplot(method = "square")
rownames(db) <- rownames(drivers %>% drop_na())
db <- db[,-1]
db.scaled <- db %>% scale

clara.res <- clara(db, 2, samples = 50, pamLike = TRUE)
fviz_cluster(clara.res,
             # palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic()
)
fviz_nbclust(db, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
fviz_nbclust(db, kmeans, method = "silhouette")
fviz_nbclust(db, pam, method = "silhouette")
fviz_nbclust(db, clara, method = "silhouette")

set.seed(123)
fviz_nbclust(db, kmeans, nstart = 25, method = "gap_stat", nboot = 50) + labs(subtitle = "Gap statistic method")
fviz_nbclust(db, pam, method = "gap_stat", nboot = 50) + labs(subtitle = "Gap statistic method")
fviz_nbclust(db, clara, method = "gap_stat", nboot = 50) + labs(subtitle = "Gap statistic method")


library("NbClust")
nb <- NbClust(db, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
fviz_nbclust(nb)

nb <- NbClust(db, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2")
fviz_nbclust(nb)

fviz_silhouette(clara.res, palette = "jco", ggtheme = theme_classic())


fviz_dist2 <- function (dist.obj, order = TRUE, show_labels = TRUE, lab_size = NULL, 
                        gradient = list(low = "red", mid = "white", high = "blue")) 
{
  if (!inherits(dist.obj, "dist")) 
    stop("An object of class dist is required.")
  if (order) {
    res.hc <- stats::hclust(dist.obj, method = "ward.D2")
    dist.obj <- as.matrix(dist.obj)[res.hc$order, res.hc$order]
  }
  else dist.obj <- as.matrix(dist.obj)
  rownames(dist.obj) <- colnames(dist.obj) <- paste0(rownames(dist.obj), 
                                                     "-")
  d <- reshape2::melt(dist.obj)
  p <- ggplot(d, aes_string(x = "X1", y = "X2")) + ggplot2::geom_tile(aes_string(fill = "value"))
  if (is.null(gradient$mid)) 
    p <- p + ggplot2::scale_fill_gradient(low = gradient$low, 
                                          high = gradient$high)
  else p <- p + ggplot2::scale_fill_gradient2(midpoint = mean(dist.obj), 
                                              low = gradient$low, mid = gradient$mid, high = gradient$high, 
                                              space = "Lab")
  if (show_labels) 
    p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
                   axis.text.x = element_text(angle = 45, hjust = 1, 
                                              size = lab_size), axis.text.y = element_text(size = lab_size))
  else p <- p + theme(axis.text = element_blank(), axis.ticks = element_blank(), 
                      axis.title.x = element_blank(), axis.title.y = element_blank())
  return(p)
}

fviz_dist2(dist.obj = dist(df), order = T, show_labels = FALSE) + labs(title = "SFS data")



library(clValid)
clVal <- clValid(obj = db,
                 nClust = 2:10,
                 clMethods = c("hierarchical", "kmeans", "diana", "fanny", "model", "sota", "pam", "clara", "agnes"),
                 validation = c("internal", "stability"),
                 metric = "euclidean",
                 method = "average")
summary(clVal)

db$cluster <- clara.res$clustering

plot3D::scatter3D(x = db$Demand,
                  y = db$Production,
                  z = db$Trade,
                  colvar = as.integer(db$cluster),
                  col = c("#1B9E77", "#D95F02"),
                  bty = "g",
                  pch = 20,
                  cex = 2,
                  phi = 20,
                  colkey = list(side = 1, length = 0.5),
                  xlab = "Demand/consumer",
                  ylab = "Production/supply",
                  zlab = "Trade/Distribution",
                  labels = c("Cluster 1", "Cluster 2"))

scatter3D_fancy <- function(x, y, z,..., colvar = z)
{
  panelfirst <- function(pmat) {
    XY <- trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 2, add = TRUE, colkey = FALSE)
    
    XY <- trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 2, add = TRUE, colkey = FALSE)
  }
  scatter3D(x, y, z, ..., colvar = colvar, panel.first=panelfirst,
            colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75)) 
}

scatter3D_fancy(x = db$Demand,
                  y = db$Production,
                  z = db$Trade,
                  bty = "g",
                  pch = 20,
                  cex = 2,
                  phi = 20,
                  xlab = "Demand/consumer",
                  ylab = "Production/supply",
                  zlab = "Trade/Distribution")



ref_vals$iso3c <- rownames(ref_vals)
todo <- left_join(drivers, ref_vals, by = "iso3c")

todo %>%
  select(ch_diet_health_attn:chg_serv_trd, SFS_index) %>% cor(., use = "pairwise.complete.obs", method = "spearman") %>%
  corrplot.mixed(., upper = "square")

plot(todo$chg_serv_trd, todo$SFS_index)

todo %>% ggplot(aes(x = chg_serv_trd, y = SFS_index)) + geom_point() # + geom_smooth(se = F)

