# Understanding SFS dynamics: SFS project
# Implemented by: H. Achicanoy & P. Alvarez
# CIAT, 2019

# R options
g <- gc(reset = T); rm(list = ls()); options(scipen = 999, warn = -1)

# Load packages
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, DALEX, caret, caretEnsemble, ceterisParibus))

# Define data directory
data_path <- "D:/ToBackup/sustainable_food_systems/sfs_repo/data"
# data_path <- "//dapadfs.cgiarad.org/workspace_cluster_9/Sustainable_Food_System/data"

# Load SFS indices
sfs_index <- read.csv(paste0(data_path,"/outputs/indicators/sfs_final_index.csv"), row.names = 1)
sfs_index <- sfs_index %>% dplyr::arrange(SFS_index)
sfs_chngs <- sfs_index
sfs_chngs <- sfs_chngs %>% dplyr::arrange(SFS_index)
sfs_chngs <- data.frame(Environment    = diff(sfs_chngs$Environment),
                        Economic       = diff(sfs_chngs$Economic),
                        Social         = diff(sfs_chngs$Social),
                        Food_nutrition = diff(sfs_chngs$Food_nutrition),
                        SFS_index      = diff(sfs_chngs$SFS_index))

rolling_average_plot <- function(obs = 15){
  plot(zoo::rollmean(x = sfs_index$Environment, k = obs), xlab = 'Countries position (high values = more sustainable)', ylab = 'Score values', ylim = c(0,1), ty = 'l') # Black
  legend('bottomright', legend = c('Environment','Economic','Social','Food and nutrition'), col = 1:4, lty = 1)
  lines(zoo::rollmean(x = sfs_index$Economic, k = obs), col = 2) # Red
  lines(zoo::rollmean(x = sfs_index$Social, k = obs), col = 3) # Green
  lines(zoo::rollmean(x = sfs_index$Food_nutrition, k = obs), col = 4) # Blue
}
rolling_average_plot(obs = 10)


pairs(sfs_index)
pairs(sfs_chngs)
cor(sfs_index[,-1], method = "spearman") %>% round(2)
cor(sfs_chngs, method = "spearman") %>% round(2)
cor(sfs_chngs[1:20,], method = "spearman")
cor(sfs_chngs[21:40,], method = "spearman")
cor(sfs_chngs[41:96,], method = "spearman")
plot(sfs_chngs[,'Social'], sfs_index$SFS_index[-1], pch = 20)
cor(sfs_chngs[,'Social'], sfs_index$SFS_index[-1], method = "spearman")

# Moving linear regression models
1:73 %>% purrr::map(.f = function(i){
  lm_fit  <- lm(SFS_index ~ ., data = sfs_index[i:(24 + i),-1])
  results <- broom::tidy(lm_fit)
  results$order <- i
  return(results)
}) %>% dplyr::bind_rows() %>%
  dplyr::filter(term %in% c('Environment', 'Economic', 'Social', 'Food_nutrition')) %>%
  ggplot(aes(x = order, y = estimate, colour = term)) +
  geom_line()

# Using local explanations
# Linear model
lm_fit <- lm(SFS_index ~ ., data = sfs_index[,-1])
exp_lm <- DALEX::explain(lm_fit, data = sfs_index[,2:5], y = sfs_index$SFS_index)

# Random forest
rf_fit <- ranger::ranger(SFS_index ~ ., data = sfs_index[,-1], num.trees = 1000, importance = 'impurity')
exp_rf <- DALEX::explain(rf_fit, data = sfs_index[,2:5], y = sfs_index$SFS_index)

# Model performance
mp_lm <- DALEX::model_performance(exp_lm)
mp_rf <- DALEX::model_performance(exp_rf)
plot(mp_lm, mp_rf, geom = 'boxplot')

# Variable importance
vi_lm <- DALEX::variable_importance(exp_lm, loss_function = DALEX::loss_root_mean_square)
vi_rf <- DALEX::variable_importance(exp_rf, loss_function = DALEX::loss_root_mean_square)
plot(vi_lm, vi_rf)

# Local explanation linear model
1:nrow(sfs_index) %>% purrr::map(.f = function(i){
  bd_lm <- breakDown::broken(model = exp_lm, new_observation = sfs_index[i,], data = sfs_index)
  results <- data.frame(variable = bd_lm$variable_name, contribution = bd_lm$contribution, observation = i)
  return(results)
}) %>% dplyr::bind_rows() %>%
  dplyr::filter(variable %in% c('Environment', 'Economic', 'Social', 'Food_nutrition')) %>%
  ggplot(aes(x = observation, y = contribution, colour = variable)) +
  geom_line()

# Local explanation random forest
1:nrow(sfs_index) %>% purrr::map(.f = function(i){
  bd_rf <- breakDown::broken(model = exp_rf, new_observation = sfs_index[i,], data = sfs_index)
  results <- data.frame(variable = bd_rf$variable_name, contribution = bd_rf$contribution, observation = i)
  return(results)
}) %>% dplyr::bind_rows() %>%
  dplyr::filter(variable %in% c('Environment', 'Economic', 'Social', 'Food_nutrition')) %>%
  ggplot(aes(x = observation, y = contribution, colour = variable)) +
  geom_line()

