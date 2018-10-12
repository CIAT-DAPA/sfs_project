envPos <- 2:8; ecoPos <- 9:11; socPos <- 12:14; fntPos <- 15:28

eco_soc <- all_data[,c(1, ecoPos, socPos)]
eco_soc <- eco_soc[complete.cases(eco_soc),]

ref_countries <- eco_soc$iso3c

# ---------------------------------------------------------------------------------- #
# Environment
# ---------------------------------------------------------------------------------- #

env_combinations <- combn(x = envPos, m = 3)
env_mtchs <- lapply(1:ncol(env_combinations), function(i){
  db_tmp <- all_data[, c(1, env_combinations[,i])]
  db_tmp <- db_tmp[complete.cases(db_tmp),]
  db_res <- tibble(matches = list(intersect(ref_countries, db_tmp$iso3c)),
                   combination = i,
                   count = intersect(ref_countries, db_tmp$iso3c) %>% length)
  return(db_res)
})
env_mtchs <- do.call(rbind, env_mtchs)
env_mtchs <- env_mtchs %>% filter(count == 50)

colnames(all_data[,env_combinations[,32]])

env_combinations[,10]
env_combinations[,11]
env_combinations[,13]
env_combinations[,32]

# ---------------------------------------------------------------------------------- #
# Food and nutrition
# ---------------------------------------------------------------------------------- #

fnt_combinations <- combn(x = fntPos, m = 3)
fnt_mtchs <- lapply(1:ncol(fnt_combinations), function(i){
  db_tmp <- all_data[, c(1, fnt_combinations[,i])]
  db_tmp <- db_tmp[complete.cases(db_tmp),]
  db_res <- tibble(matches = list(intersect(ref_countries, db_tmp$iso3c)),
                   combination = i,
                   count = intersect(ref_countries, db_tmp$iso3c) %>% length)
  return(db_res)
})
fnt_mtchs <- do.call(rbind, fnt_mtchs)
fnt_mtchs <- fnt_mtchs %>% filter(count == 51)

colnames(all_data[,fnt_combinations[,200]])

fnt_combinations[,145]
fnt_combinations[,146]
fnt_combinations[,155]
fnt_combinations[,200]


combList <- names(all_data)[c(env_combinations[,10], ecoPos, socPos, fnt_combinations[,145])]
combListDF <- expand.grid(
  
  env_mtchs$combination %>% purrr::map(.f = function(x){colnames(all_data[,env_combinations[,x]])}),
  list(c(colnames(all_data[,ecoPos]), colnames(all_data[,socPos]))),
  fnt_mtchs$combination %>% purrr::map(.f = function(x){colnames(all_data[,fnt_combinations[,x]])})
  
)
str(combListDF)

combList <- apply(X = combListDF, MARGIN = 1, FUN = function(x){unlist(c(x))})

index_simulation <- lapply(1:ncol(combList), function(i){
  
  db_res <- calc_sfs_index(combList = combList[,i], data = all_data, fnt_type = "arithmetic")
  db_res$Combination <- i
  db_res$iso3c <- rownames(db_res)
  db_res$Approach <- "Proposed"
  
  normalization <- function(x){
    y = x/max(x, na.rm = T)
    return(y)
  }
  data <- all_data
  for(j in 2:ncol(data)){
    data[,j] <- normalization(x = data[,j])
    data[which(data[,j] == 0), j] <- data[which(data[,j] == 0), j] + 0.01
  }; rm(j)
  signs <- c(NA, -1, +1, -1, +1, -1, +1, -1, +1, -1, +1, +1, +1, +1, +1, -1, +1, +1, +1, -1, -1, -1, -1, +1, +1, -1, -1, -1)
  theory <- T
  mtch <- match(combList[,i], names(data))
  for(m in mtch){
    if(theory){
      if(signs[m] < 0){
        data[,m] <- 1 - data[,m]
        data[which(data[,m] == 0), m] <- data[which(data[,m] == 0), m] + 0.01
      }
    }
  }; rm(m)
  data <- data %>% select(iso3c, combList[,i] %>% as.character)
  data <- data %>% filter(iso3c %in% ref_countries)
  data2 <- data
  data2$SFS_index <- data2 %>% select(-iso3c) %>% apply(., 1, FUN = EnvStats::geoMean)
  data2$Combination <- i
  data2$Approach <- "All geometric"
  data3 <- data
  data3$SFS_index <- data3 %>% select(-iso3c) %>% apply(., 1, FUN = mean)
  data3$Combination <- i
  data3$Approach <- "All arithmetic"
  
  db_res <- plyr::rbind.fill(db_res,
                             data2 %>% select(iso3c, SFS_index, Combination, Approach),
                             data3 %>% select(iso3c, SFS_index, Combination, Approach))
  
  return(db_res)
  
})
index_simulation <- do.call(rbind, index_simulation)
index_simulation %>%
  ggplot(aes(x = reorder(iso3c, SFS_index, FUN = median), y = SFS_index, fill = Approach)) +
  geom_boxplot(position=position_dodge(1)) +
  # geom_jitter(position = position_jitter(0.2)) +
  # geom_point(data = sfs_index[which(sfs_index$iso3c %in% unique(index_simulation$iso3c)),] %>%
  #              select(iso3c, SFS_index) %>% mutate(Approach = "Proposed"),
  #            aes(x = iso3c, y = SFS_index), col = "red", size = 3) +
  ylim(0, 1) +
  xlab("Countries") +
  ylab("SFS index")



library(ggplot2)
theme_set(theme_bw())

gg <- sfs_index %>%
  ggplot(aes(x = reorder(country.name.en, SFS_index, FUN = median), y = SFS_index, label = round(SFS_index, 2))) +
  geom_point(stat = 'identity', aes(col = SFS_index), size = 6) +
  scale_color_gradient(low = "#f0650e", high = "#0091ff", space = "Lab") +
  # scale_color_manual(name = "Mileage", 
  #                    labels = c("Above Average", "Below Average"), 
  #                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  geom_text(color = "white", size = 2) +
  labs(title = "Sustainable Food Systems index", 
       subtitle = "Global understanting of system dynamics") +
  ylab("Sustainable food systems index") +
  xlab("") +
  # ylim(0, 1) +
  coord_flip()

g <- ggplotly(gg, source = 'source') %>% 
  layout(title = "Click and drag to select points") %>%
  highlight("plotly_selected")

build <- plotly_build(g)

build$data[[1]]$text <- paste0('Country: ', as.character(sfs_index$country.name.en), '<br>', 
                               'Score: ', as.character(round(sfs_index$SFS_index, 2))) 

build


# Version 1
d <- plotly::highlight_key(sfs_index)
g <- qplot(data = d, x = Environment, y = SFS_index) %>%
  subplot(qplot(data = d, x = Economic, y = Food_nutrition)) %>%
  layout(title = "Click and drag to select points",
         dragmode = 'lasso') %>%
  highlight("plotly_selected")
build <- plotly_build(g)
build$x$data[[1]]$text <- paste0('Enviromental: ', as.character(round(sfs_index$Environment, 2)), '<br>', 
                                 'SFS score: ', as.character(round(sfs_index$SFS_index, 2)), '<br>',
                                 'Country: ', as.character(sfs_index$country.name.en))
build$x$data[[2]]$text <- paste0('Economic: ', as.character(round(sfs_index$Environment, 2)), '<br>', 
                                 'Food and nutrition: ', as.character(round(sfs_index$SFS_index, 2)), '<br>',
                                 'Country: ', as.character(sfs_index$country.name.en))
build

# Version 2
d <- plotly::highlight_key(sfs_index)
g1 <- ggplot(data = d, aes(x = Environment, y = SFS_index)) +
  geom_point() +
  # stat_smooth(color = 'red', method = 'loess', span = .9, se = FALSE) +
  xlim(0, 1) +
  ylim(0, 1)
g2 <- ggplot(data = d, aes(x = Economic, y = Food_nutrition)) +
  geom_point() +
  xlim(0, 1) +
  ylim(0, 1)

g <- g1 %>%
  subplot(g2) %>%
  layout(title = "Click and drag to select points",
         dragmode = 'lasso') %>%
  highlight("plotly_selected")
suppressMessages(build <- plotly_build(g))
build$x$data[[1]]$text <- paste0('Enviromental: ', as.character(round(sfs_index$Environment, 2)), '<br>', 
                                 'SFS score: ', as.character(round(sfs_index$SFS_index, 2)), '<br>',
                                 'Country: ', as.character(sfs_index$country.name.en))
build$x$data[[2]]$text <- paste0('Economic: ', as.character(round(sfs_index$Environment, 2)), '<br>', 
                                 'Food and nutrition: ', as.character(round(sfs_index$SFS_index, 2)), '<br>',
                                 'Country: ', as.character(sfs_index$country.name.en))
suppressMessages(build)


# ------------------------------------------------------------------------------- #
# SFS all indicators and sub-indexes analysis
# ------------------------------------------------------------------------------- #

ttst <- read.csv("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/sfs_prcs_indicators_plus_indexes.csv", row.names = 1)
suppressMessages(library(DALEX))
suppressMessages(library(e1071))
suppressMessages(library(ceterisParibus))
suppressMessages(library(gbm))
suppressMessages(library(xgboost))
ttst2 <- ttst %>% dplyr::select(iso3c:Serum.retinol.deficiency, SFS_index)
# Run models
sfs_lm <- lm(SFS_index ~ ., data = ttst2[,-1])
sfs_rf <- randomForest(SFS_index ~ ., data = ttst2[,-1])
sfs_sv <- svm(SFS_index ~ ., data = ttst2[,-1])
sfs_gbm <- gbm(SFS_index ~ ., data = ttst2[,-1], n.trees = 500)
# Explainers
expl_lm <- DALEX::explain(sfs_lm, data = ttst2[,2:(ncol(ttst2)-1)], y = ttst2$SFS_index)
expl_rf <- DALEX::explain(sfs_rf, data = ttst2[,2:(ncol(ttst2)-1)], y = ttst2$SFS_index)
expl_sv <- DALEX::explain(sfs_sv, data = ttst2[,2:(ncol(ttst2)-1)], y = ttst2$SFS_index)
expl_gbm <- DALEX::explain(sfs_gbm, data = ttst2[,2:(ncol(ttst2)-1)], y = ttst2$SFS_index, predict_function = function(model, x) predict(model, x, n.trees = 500))
# Model performance
mp_lm <- DALEX::model_performance(expl_lm)
mp_rf <- DALEX::model_performance(expl_rf)
mp_sv <- DALEX::model_performance(expl_sv)
mp_gbm <- DALEX::model_performance(expl_gbm)
plot(mp_lm, mp_rf, mp_sv, mp_gbm)
plot(mp_lm, mp_rf, mp_sv, mp_gbm, geom = "boxplot")
# Feature importance
vi_lm <- DALEX::variable_importance(expl_lm, loss_function = loss_root_mean_square, type = "difference")
vi_rf <- DALEX::variable_importance(expl_rf, loss_function = loss_root_mean_square, type = "difference")
vi_sv <- DALEX::variable_importance(expl_sv, loss_function = loss_root_mean_square, type = "difference")
vi_gbm <- DALEX::variable_importance(expl_gbm, loss_function = loss_root_mean_square, type = "difference")
plot(vi_lm, vi_rf, vi_sv, vi_gbm)
vi_gnr <- rbind(vi_lm, vi_rf, vi_sv, vi_gbm)
vi_gnr %>% ggplot(aes(x = reorder(variable, -dropout_loss), y = dropout_loss, group = label, fill = label)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip()

# Single variable response
sv_lm  <- single_variable(expl_lm, variable =  "Obesity", type = "pdp")
sv_rf  <- single_variable(expl_rf, variable =  "Obesity", type = "pdp")
sv_sv  <- single_variable(expl_sv, variable =  "Obesity", type = "pdp")
sv_gbm  <- single_variable(expl_gbm, variable =  "Obesity", type = "pdp")
plot(sv_lm, sv_rf, sv_sv, sv_gbm)

ggplot(mp_rf, aes(observed, diff)) + geom_point() + 
  xlab("Observed") + ylab("Predicted - Observed") + 
  ggtitle("Diagnostic plot for the individual model") + theme_mi2()

## Ceteris Paribus plots using neighbours
# my_obs <- ttst[which(ttst$iso3c == "JOR"),-1]
# neighbours <- select_neighbours(ttst[,-1], observation = my_obs, n = 10)
# head(neighbours)
# profile_rf_neig  <- ceteris_paribus(expl_rf,
#                                     observations = neighbours, 
#                                     y = neighbours$SFS_index)
# plot(profile_rf_neig, 
#      selected_variables = "Obesity", size_residuals = 2,
#      color_residuals = "red", show_residuals = TRUE, show_observations = FALSE)

profile_model <- ceteris_paribus(expl_lm, observations = ttst[,-1], y = ttst$SFS_index)
plot(profile_model, selected_variables = "Obesity", size_residuals = 2, color_residuals = "red", show_residuals = T, show_observations = T) +
  ceteris_paribus_layer(profile_model, size = 3, alpha = 1, color = "blue",
                        aggregate_profiles = median, show_observations = FALSE,
                        selected_variables = "Obesity")

# ------------------------------------------------------------------------------- #
# SFS index controled by GDP analysis
# ------------------------------------------------------------------------------- #

ttst <- read.csv("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/sfs_prcs_indicators_plus_indexes.csv", row.names = 1)
suppressMessages(library(DALEX))
suppressMessages(library(e1071))
suppressMessages(library(ceterisParibus))
suppressMessages(library(gbm))
suppressMessages(library(randomForest))
suppressMessages(library(xgboost))

gdp <- read.csv("D:/gdp_data/gdp_per_capita.csv")
gdp <- gdp %>% select(Country.Code, X2014)
names(gdp) <- c("iso3c", "GDP")

nms <- rownames(ttst)

ttst <- left_join(x = ttst, y = gdp, by = "iso3c")
rownames(ttst) <- nms

ttst2 <- ttst %>% dplyr::select(iso3c:Serum.retinol.deficiency, GDP, SFS_index)
# Run models
sfs_lm <- lm(SFS_index ~ ., data = ttst2[,-1])
sfs_rf <- randomForest(SFS_index ~ ., data = ttst2[,-1])
sfs_sv <- svm(SFS_index ~ ., data = ttst2[,-1])
sfs_gbm <- gbm(SFS_index ~ ., data = ttst2[,-1], n.trees = 500)
# Explainers
expl_lm <- DALEX::explain(sfs_lm, data = ttst2[,2:(ncol(ttst2)-1)], y = ttst2$SFS_index)
expl_rf <- DALEX::explain(sfs_rf, data = ttst2[,2:(ncol(ttst2)-1)], y = ttst2$SFS_index)
expl_sv <- DALEX::explain(sfs_sv, data = ttst2[,2:(ncol(ttst2)-1)], y = ttst2$SFS_index)
expl_gbm <- DALEX::explain(sfs_gbm, data = ttst2[,2:(ncol(ttst2)-1)], y = ttst2$SFS_index, predict_function = function(model, x) predict(model, x, n.trees = 500))
# Model performance
mp_lm <- DALEX::model_performance(expl_lm)
mp_rf <- DALEX::model_performance(expl_rf)
mp_sv <- DALEX::model_performance(expl_sv)
mp_gbm <- DALEX::model_performance(expl_gbm)
plot(mp_lm, mp_rf, mp_sv, mp_gbm)
plot(mp_lm, mp_rf, mp_sv, mp_gbm, geom = "boxplot")
# Feature importance
vi_lm <- DALEX::variable_importance(expl_lm, loss_function = loss_root_mean_square, type = "difference")
vi_rf <- DALEX::variable_importance(expl_rf, loss_function = loss_root_mean_square, type = "difference")
vi_sv <- DALEX::variable_importance(expl_sv, loss_function = loss_root_mean_square, type = "difference")
vi_gbm <- DALEX::variable_importance(expl_gbm, loss_function = loss_root_mean_square, type = "difference")
plot(vi_lm, vi_rf, vi_sv, vi_gbm)
vi_gnr <- rbind(vi_lm, vi_rf, vi_sv, vi_gbm)
vi_gnr %>% ggplot(aes(x = reorder(variable, -dropout_loss), y = dropout_loss, group = label, fill = label)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip()

# ------------------------------------------------------------------------------- #
# Food and nutrition analysis
# ------------------------------------------------------------------------------- #

fnn <- ttst %>% select(iso3c, Food.available:Serum.retinol.deficiency, Food_nutrition)

# Run models
fnn_lm <- lm(Food_nutrition ~ ., data = fnn[,-1])
fnn_rf <- randomForest(Food_nutrition ~ ., data = fnn[,-1])
fnn_sv <- svm(Food_nutrition ~ ., data = fnn[,-1])
fnn_gbm <- gbm(Food_nutrition ~ ., data = fnn[,-1], n.trees = 500)
# Explainers
fnn_expl_lm <- DALEX::explain(fnn_lm, data = fnn[,2:(ncol(fnn)-1)], y = fnn$Food_nutrition)
fnn_expl_rf <- DALEX::explain(fnn_rf, data = fnn[,2:(ncol(fnn)-1)], y = fnn$Food_nutrition)
fnn_expl_sv <- DALEX::explain(fnn_sv, data = fnn[,2:(ncol(fnn)-1)], y = fnn$Food_nutrition)
fnn_expl_gbm <- DALEX::explain(fnn_gbm, data = fnn[,2:(ncol(fnn)-1)], y = fnn$Food_nutrition, predict_function = function(model, x) predict(model, x, n.trees = 500))
# Model performance
fnn_mp_lm <- DALEX::model_performance(fnn_expl_lm)
fnn_mp_rf <- DALEX::model_performance(fnn_expl_rf)
fnn_mp_sv <- DALEX::model_performance(fnn_expl_sv)
fnn_mp_gbm <- DALEX::model_performance(fnn_expl_gbm)
plot(fnn_mp_lm, fnn_mp_rf, fnn_mp_sv, fnn_mp_gbm)
plot(fnn_mp_lm, fnn_mp_rf, fnn_mp_sv, fnn_mp_gbm, geom = "boxplot")
# Feature importance
fnn_vi_lm <- DALEX::variable_importance(fnn_expl_lm, loss_function = loss_root_mean_square, type = "difference")
fnn_vi_rf <- DALEX::variable_importance(fnn_expl_rf, loss_function = loss_root_mean_square, type = "difference")
fnn_vi_sv <- DALEX::variable_importance(fnn_expl_sv, loss_function = loss_root_mean_square, type = "difference")
fnn_vi_gbm <- DALEX::variable_importance(fnn_expl_gbm, loss_function = loss_root_mean_square, type = "difference")
plot(fnn_vi_lm, fnn_vi_rf, fnn_vi_sv, fnn_vi_gbm)
fnn_vi_gnr <- rbind(fnn_vi_lm, fnn_vi_rf, fnn_vi_sv, fnn_vi_gbm)
fnn_vi_gnr %>% ggplot(aes(x = reorder(variable, -dropout_loss), y = dropout_loss, group = label, fill = label)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip()

# Single variable response
fnn_sv_lm <- single_variable(fnn_expl_lm, variable = "Obesity", type = "pdp")
fnn_sv_rf <- single_variable(fnn_expl_rf, variable = "Obesity", type = "pdp")
fnn_sv_sv <- single_variable(fnn_expl_sv, variable = "Obesity", type = "pdp")
fnn_sv_gbm <- single_variable(fnn_expl_gbm, variable = "Obesity", type = "pdp")
plot(fnn_sv_lm, fnn_sv_rf, fnn_sv_sv, fnn_sv_gbm)

ggplot(mp_rf, aes(observed, diff)) + geom_point() + 
  xlab("Observed") + ylab("Predicted - Observed") + 
  ggtitle("Diagnostic plot for the random forest model") + theme_mi2()

my_obs <- fnn[which(fnn$iso3c == "COL"),-1]
neighbours <- select_neighbours(fnn[,-1], observation = my_obs, n = 10)
head(neighbours)

profile_rf_neig <- ceteris_paribus(expl_rf,
                                    observations = neighbours, 
                                    y = neighbours$Food_nutrition)
plot(profile_rf_neig,
     selected_variables = "Obesity", size_residuals = 2,
     color_residuals = "red", show_residuals = T, show_observations = T)

profile_rf_glbl <- ceteris_paribus(expl_rf,
                                   observations = fnn[,-1], 
                                   y = fnn$Food_nutrition)
plot(profile_rf_glbl,
     selected_variables = "Obesity", size_residuals = 2,
     color_residuals = "red", show_residuals = T, show_observations = T) +
  ceteris_paribus_layer(profile_rf_glbl, size = 3, alpha = 1, color = "blue",
                        aggregate_profiles = median, show_observations = FALSE,
                        selected_variables = "Obesity")
