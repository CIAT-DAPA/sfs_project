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
  plot(x = zoo::rollmean(x = sfs_index$SFS_index, k = obs), y = zoo::rollmean(x = sfs_index$Environment, k = obs), xlab = 'SFS overall score', ylab = 'Score values', ylim = c(0,1), ty = 'l') # Black
  legend('bottomright', legend = c('Environment','Economic','Social','Food and nutrition'), col = 1:4, lty = 1)
  lines(x = zoo::rollmean(x = sfs_index$SFS_index, k = obs), y = zoo::rollmean(x = sfs_index$Economic, k = obs), col = 2) # Red
  lines(x = zoo::rollmean(x = sfs_index$SFS_index, k = obs), y = zoo::rollmean(x = sfs_index$Social, k = obs), col = 3) # Green
  lines(x = zoo::rollmean(x = sfs_index$SFS_index, k = obs), y = zoo::rollmean(x = sfs_index$Food_nutrition, k = obs), col = 4) # Blue
}
rolling_average_plot(obs = 15)

# Normalize predictors previously include interaction effects
sfs_index2 <- sfs_index
sfs_index2[,c('Environment','Economic','Social','Food_nutrition')] <- sfs_index2[,c('Environment','Economic','Social','Food_nutrition')] %>% scale(center = T, scale = T) %>% data.frame

# Fitting a linear model (without intercept)
lm_fit <- lm(SFS_index ~ (Environment + Economic + Social + Food_nutrition)^2 - 1, data = sfs_index2)

# Moving linear regression models
linear_fits <- 1:73 %>% purrr::map(.f = function(i){
  lm_fit  <- lm(SFS_index ~ (Environment + Economic + Social + Food_nutrition)^2 - 1, data = sfs_index2[i:(24 + i),-1])
  results <- broom::tidy(lm_fit)
  results$order <- i
  return(results)
}) %>% dplyr::bind_rows()
linear_fits$sfs_mean <- 1:73 %>% purrr::map(.f = function(i){ sfs_index$SFS_index[i:(24 + i)] %>% mean}) %>% unlist %>% rep(., each = 10)
linear_fits %>%
  dplyr::filter(term %in% c('Environment', 'Economic', 'Social', 'Food_nutrition')) %>% # & p.value < 0.05
  ggplot(aes(x = sfs_mean, y = estimate, colour = term)) +
  scale_color_manual(values=c("red", "black", 'blue', "green")) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, col = 'red', lty = 2, size = 2)
linear_fits %>%
  dplyr::filter(term %in% c('Environment:Economic','Environment:Social','Environment:Food_nutrition',
                            'Economic:Social','Economic:Food_nutrition','Social:Food_nutrition')) %>% # & p.value < 0.05
  ggplot(aes(x = sfs_mean, y = estimate, colour = term)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, col = 'red', lty = 2, size = 2)

# Fitting a beta regression model (without intercept)
bt_fit <- betareg::betareg(SFS_index ~ (Environment + Economic + Social + Food_nutrition)^2 - 1, link = 'logit', data = sfs_index2)
plot(sfs_index2$SFS_index, bt_fit$fitted.values, xlab = "SFS index values", ylab = "SFS predicted values", pch = 20); abline(0,1)
cor.test(sfs_index2$SFS_index, bt_fit$fitted.values)

# Moving Beta regression models
beta_fits <- 1:73 %>% purrr::map(.f = function(i){
  bt_fit  <- betareg::betareg(SFS_index ~ Environment + Economic + Social + Food_nutrition - 1, link = 'logit', data = sfs_index2[i:(24 + i),-1])
  results <- broom::tidy(bt_fit)
  results$order <- i
  return(results)
}) %>% dplyr::bind_rows()
beta_fits %>%
  dplyr::filter(term %in% c('Environment','Economic','Social','Food_nutrition')) %>% #  & p.value < 0.05
  ggplot(aes(x = order, y = estimate, colour = term)) +
  scale_color_manual(values=c("red", "black", 'blue', "green")) + 
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, col = 'red', lty = 2, size = 2)

beta_fits2 <- 1:73 %>% purrr::map(.f = function(i){ # 1:73, 25 countries (24); # 1:48, 50 countries (49)
  bt_fit  <- betareg::betareg(SFS_index ~ (Environment + Economic + Social + Food_nutrition)^2 - 1, link = 'logit', data = sfs_index2[i:(24 + i),-1])
  results <- broom::tidy(bt_fit)
  results$order <- i
  return(results)
}) %>% dplyr::bind_rows()
beta_fits2 %>%
  dplyr::filter(term %in% c('Environment','Economic','Social','Food_nutrition',
                            'Environment:Economic','Environment:Social','Environment:Food_nutrition',
                            'Economic:Social','Economic:Food_nutrition','Social:Food_nutrition')) %>% #  & p.value < 0.05
  ggplot(aes(x = order, y = estimate, colour = term)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, col = 'red', lty = 2, size = 2)

# Using local explanations
# Linear model
lm_fit <- lm(SFS_index ~ . - 1, data = sfs_index[,-1])
exp_lm <- DALEX::explain(lm_fit, data = sfs_index[,2:5], y = sfs_index$SFS_index)

# Beta model
exp_bt <- DALEX::explain(bt_fit, data = sfs_index[,2:5], y = sfs_index$SFS_index)

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
  scale_color_manual(values=c("red", "black", 'blue', "green")) + 
  geom_line(size = 1.2) +
  geom_point(size = 3)

# Local explanation beta model
1:nrow(sfs_index) %>% purrr::map(.f = function(i){
  bd_bt <- breakDown::broken(model = exp_bt, new_observation = sfs_index[i,], data = sfs_index)
  results <- data.frame(variable = bd_bt$variable_name, contribution = bd_bt$contribution, observation = i)
  return(results)
}) %>% dplyr::bind_rows() %>%
  dplyr::filter(variable %in% c('Environment', 'Economic', 'Social', 'Food_nutrition')) %>%
  ggplot(aes(x = observation, y = contribution, colour = variable)) +
  scale_color_manual(values=c("red", "black", 'blue', "green")) + 
  geom_line(size = 1.2) +
  geom_point(size = 3)

# Local explanation random forest
1:nrow(sfs_index) %>% purrr::map(.f = function(i){
  bd_rf <- breakDown::broken(model = exp_rf, new_observation = sfs_index[i,], data = sfs_index)
  results <- data.frame(variable = bd_rf$variable_name, contribution = bd_rf$contribution, observation = i)
  return(results)
}) %>% dplyr::bind_rows() %>%
  dplyr::filter(variable %in% c('Environment', 'Economic', 'Social', 'Food_nutrition')) %>%
  ggplot(aes(x = observation, y = contribution, colour = variable)) +
  scale_color_manual(values=c("red", "black", 'blue', "green")) + 
  geom_line(size = 1.2) +
  geom_point(size = 3)

# ------------------------------------------------------------- #
# Creating quartile groups ordered by SFS overall score
# ------------------------------------------------------------- #

sfs_index2 <- sfs_index
sfs_index2$Groups <- cut(sfs_index$SFS_index, breaks = quantile(sfs_index$SFS_index, probs = seq(0, 1, 0.25)))
levels(sfs_index2$Groups) <- paste0('g',1:4)
sfs_index2$Groups[1] <- 'g1'

# Mean summarization
summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE,
                      conf.interval = .95, .drop = TRUE){
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Median summarization
summarySEMedian <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE,
                            conf.interval = .95, .drop = TRUE){
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = median   (xx[[col]], na.rm=na.rm),
                     sd   = mad     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Mean summary
env_dt <- summarySE(data = sfs_index2[,-1], measurevar = "Environment", groupvars = "Groups")
eco_dt <- summarySE(data = sfs_index2[,-1], measurevar = "Economic", groupvars = "Groups")
soc_dt <- summarySE(data = sfs_index2[,-1], measurevar = "Social", groupvars = "Groups")
fnt_dt <- summarySE(data = sfs_index2[,-1], measurevar = "Food_nutrition", groupvars = "Groups")

names(env_dt)[3] <- 'Values'
names(eco_dt)[3] <- 'Values'
names(soc_dt)[3] <- 'Values'
names(fnt_dt)[3] <- 'Values'

env_dt$Dimension <- "Environment"
eco_dt$Dimension <- "Economic"
soc_dt$Dimension <- "Social"
fnt_dt$Dimension <- "Food_nutrition"

final <- rbind(env_dt, eco_dt, soc_dt, fnt_dt)

pd <- position_dodge(0.1)

ggplot(final, aes(x = Groups, y = Values)) + 
  geom_errorbar(aes(ymin = Values-ci, ymax = Values+ci), colour = "black", width = .1, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 3) +
  facet_wrap(~Dimension) +
  ylim(0, 1)
rm(env_dt, eco_dt, soc_dt, fnt_dt, pd, final)

# Median summary
env_dt <- summarySEMedian(data = sfs_index2[,-1], measurevar = "Environment", groupvars = "Groups")
eco_dt <- summarySEMedian(data = sfs_index2[,-1], measurevar = "Economic", groupvars = "Groups")
soc_dt <- summarySEMedian(data = sfs_index2[,-1], measurevar = "Social", groupvars = "Groups")
fnt_dt <- summarySEMedian(data = sfs_index2[,-1], measurevar = "Food_nutrition", groupvars = "Groups")

names(env_dt)[3] <- 'Values'
names(eco_dt)[3] <- 'Values'
names(soc_dt)[3] <- 'Values'
names(fnt_dt)[3] <- 'Values'

env_dt$Dimension <- "Environment"
eco_dt$Dimension <- "Economic"
soc_dt$Dimension <- "Social"
fnt_dt$Dimension <- "Food_nutrition"

final2 <- rbind(env_dt, eco_dt, soc_dt, fnt_dt)

pd <- position_dodge(0.1)

ggplot(final2, aes(x = Groups, y = Values)) + 
  geom_errorbar(aes(ymin = Values-ci, ymax = Values+ci), colour = "black", width = .1, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 3) +
  facet_wrap(~Dimension) +
  ylim(0, 1)
rm(env_dt, eco_dt, soc_dt, fnt_dt, pd, final2)

# Fitting Beta regression models
sfs_index2[,c('Environment','Economic','Social','Food_nutrition')] <- sfs_index2[,c('Environment','Economic','Social','Food_nutrition')] %>% scale(center = T, scale = T) %>% data.frame
models <- lapply(paste0('g',1:4), function(g){
  db <- sfs_index2[sfs_index2$Groups == g,]
  #db[,c('Environment','Economic','Social','Food_nutrition')] <- db[,c('Environment','Economic','Social','Food_nutrition')] %>% scale(center = T, scale = F) %>% data.frame
  bt_fit <- betareg::betareg(SFS_index ~ (Environment + Economic + Social + Food_nutrition)^2 -1 , link = 'logit', data = db)
  return(bt_fit)
})
models %>% purrr::map(.f = summary)

# ------------------------------------------------------------- #
# Creating quartile groups ordered by GDP per capita
# ------------------------------------------------------------- #

gdp <- read.csv(paste0(data_path,'/inputs_raw/gdp_per_capita_2010_us_dollars.csv'))
sfs_index3 <- dplyr::left_join(sfs_index, data.frame(iso3c = gdp$iso3c, gdp = gdp %>% dplyr::select(X2004:X2015) %>% apply(., 1, median, na.rm = T)), by = "iso3c")
sfs_index3 <- sfs_index3 %>% dplyr::arrange(gdp)

sfs_index3$Groups <- cut(sfs_index3$gdp, breaks = quantile(sfs_index3$gdp, probs = seq(0, 1, 0.25)))
levels(sfs_index3$Groups) <- paste0('g',1:4)
sfs_index3$Groups[1] <- 'g1'

# Mean summary
env_dt <- summarySE(data = sfs_index3[,-1], measurevar = "Environment", groupvars = "Groups")
eco_dt <- summarySE(data = sfs_index3[,-1], measurevar = "Economic", groupvars = "Groups")
soc_dt <- summarySE(data = sfs_index3[,-1], measurevar = "Social", groupvars = "Groups")
fnt_dt <- summarySE(data = sfs_index3[,-1], measurevar = "Food_nutrition", groupvars = "Groups")

names(env_dt)[3] <- 'Values'
names(eco_dt)[3] <- 'Values'
names(soc_dt)[3] <- 'Values'
names(fnt_dt)[3] <- 'Values'

env_dt$Dimension <- "Environment"
eco_dt$Dimension <- "Economic"
soc_dt$Dimension <- "Social"
fnt_dt$Dimension <- "Food_nutrition"

final <- rbind(env_dt, eco_dt, soc_dt, fnt_dt)

pd <- position_dodge(0.1)

ggplot(final, aes(x = Groups, y = Values)) + 
  geom_errorbar(aes(ymin = Values - ci, ymax = Values + ci), colour = "black", width = .1, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 3) +
  facet_wrap(~Dimension) +
  ylim(0, 1)
rm(env_dt, eco_dt, soc_dt, fnt_dt, final, pd)

# Median summary
env_dt <- summarySEMedian(data = sfs_index3[,-1], measurevar = "Environment", groupvars = "Groups")
eco_dt <- summarySEMedian(data = sfs_index3[,-1], measurevar = "Economic", groupvars = "Groups")
soc_dt <- summarySEMedian(data = sfs_index3[,-1], measurevar = "Social", groupvars = "Groups")
fnt_dt <- summarySEMedian(data = sfs_index3[,-1], measurevar = "Food_nutrition", groupvars = "Groups")

names(env_dt)[3] <- 'Values'
names(eco_dt)[3] <- 'Values'
names(soc_dt)[3] <- 'Values'
names(fnt_dt)[3] <- 'Values'

env_dt$Dimension <- "Environment"
eco_dt$Dimension <- "Economic"
soc_dt$Dimension <- "Social"
fnt_dt$Dimension <- "Food_nutrition"

final2 <- rbind(env_dt, eco_dt, soc_dt, fnt_dt)

pd <- position_dodge(0.1)

ggplot(final2, aes(x = Groups, y = Values)) + 
  geom_errorbar(aes(ymin = Values - ci, ymax = Values + ci), colour = "black", width = .1, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 3) +
  facet_wrap(~Dimension) +
  ylim(0, 1)
rm(env_dt, eco_dt, soc_dt, fnt_dt, final2, pd)

# Fitting Beta regression models
sfs_index3[,c('Environment','Economic','Social','Food_nutrition')] <- sfs_index3[,c('Environment','Economic','Social','Food_nutrition')] %>% scale(center = T, scale = T) %>% data.frame
models <- lapply(paste0('g',1:4), function(g){
  db <- sfs_index3[sfs_index3$Groups == g,]
  bt_fit <- betareg::betareg(SFS_index ~ (Environment + Economic + Social + Food_nutrition)^2 - 1, link = 'logit', data = db)
  return(bt_fit)
})
models %>% purrr::map(.f = summary)

# Exploring SFS index delta change vs drivers
sfs_delta   <- readxl::read_excel(paste0(data_path, '/outputs/indicators/SFS_index_over_time/SFS_and_GDP_dynamics.xlsx'), sheet = 'SFS_delta_change')
sfs_drivers <- read.csv(paste0(data_path, '/outputs/drivers/sfs_drivers.csv'), row.names = 1)

sfs_df <- dplyr::left_join(x = sfs_delta, y = sfs_drivers, by = 'iso3c'); rm(sfs_delta, sfs_drivers)

variables <- names(sfs_df)[-1]

# Correlations with outliers

i_loop <- lapply(1:length(variables), function(i){
  j_loop <- lapply(1:length(variables), function(j){
    df <- sfs_df[,c(variables[i],variables[j])]; names(df) <- c('x','y'); df <- df %>% tidyr::drop_na()
    results <- cor.test(df$x, df$y, method = "spearman") %>% broom::tidy() %>% data.frame()
    results$x <- variables[i]
    results$y <- variables[j]
    results <- results %>% dplyr::select(x, y, estimate, p.value)
    return(results)
  })
  j_loop <- dplyr::bind_rows(j_loop)
  return(j_loop)
})
i_loop <- dplyr::bind_rows(i_loop)

write.csv(i_loop, "D:/sfs_corr_outliers.csv", row.names = F)

png(height = 8, width = 8, res = 300, units = "in", file = 'D:/sfs_corr_outliers.png')
par(cex = 1)
corrplot::corrplot(corr = reshape2::acast(data = i_loop, formula = x~y, value.var = "estimate"),
                   p.mat = reshape2::acast(data = i_loop, formula = x~y, value.var = "p.value"),
                   insig = "blank",
                   order = "hclust",
                   tl.pos = "lt",
                   cl.cex = par("cex"),
                   tl.cex = par("cex"),
                   number.cex = 0.45,
                   lower.col = "black")
dev.off()


# Correlations removing outliers

i_loop_ro <- lapply(1:length(variables), function(i){
  j_loop_ro <- lapply(1:length(variables), function(j){
    
    if(i == j){
      results <- data.frame(x = variables[i], y = variables[j], estimate = 1, p.value = 0)
      return(results)
    } else {
      df <- sfs_df[,c(variables[i],variables[j])]; names(df) <- c('x','y'); df <- df %>% tidyr::drop_na()
      
      # Try different outliers detection methods
      O3s  <- df %>% OutliersO3::O3prep(method = c("HDo", "PCS", "BAC", "adjOut", "DDC", "MCD"), tols = .1, boxplotLimits = 6)
      O3s1 <- OutliersO3::O3plotM(O3s)
      otlr <- O3s1$outsTable
      if(length(grep(pattern = "0", x = otlr$Combination)) > 0){
        otlr <- otlr[-grep(pattern = "0", x = otlr$Combination),]
      }
      df_updt <- df[-unique(as.numeric(otlr$Case)),]
      
      results <- cor.test(df_updt$x, df_updt$y, method = "spearman") %>% broom::tidy() %>% data.frame()
      results$x <- variables[i]
      results$y <- variables[j]
      results <- results %>% dplyr::select(x, y, estimate, p.value)
      return(results)
    }
    
  })
  j_loop_ro <- dplyr::bind_rows(j_loop_ro)
  return(j_loop_ro)
})
i_loop_ro <- dplyr::bind_rows(i_loop_ro)

write.csv(i_loop_ro, "D:/sfs_corr_cleaned.csv", row.names = F)

png(height = 8, width = 8, res = 300, units = "in", file = 'D:/sfs_corr_cleaned.png')
par(cex = 1)
corrplot::corrplot(corr = reshape2::acast(data = i_loop_ro, formula = x~y, value.var = "estimate"),
                   p.mat = reshape2::acast(data = i_loop_ro, formula = x~y, value.var = "p.value"),
                   insig = "blank",
                   order = "hclust",
                   tl.pos = "lt",
                   cl.cex = par("cex"),
                   tl.cex = par("cex"),
                   number.cex = 0.45,
                   lower.col = "black")
dev.off()


delta <- drivers %>% purrr::map(.f = function(drv){
  df <- sfs_df[,c(drv,'SFS_delta')]; df <- df %>% tidyr::drop_na()
  names(df) <- c('x','y')
  db <- cor.test(df$x, df$y, method = "spearman") %>% broom::tidy()
  db$Driver <- drv
  return(db)
}) %>% dplyr::bind_rows() %>% dplyr::select(Driver, estimate, p.value)
names(delta)[2:3] <- c("Delta_Spearman","Delta_pvalue")

index <- drivers %>% purrr::map(.f = function(drv){
  df <- sfs_df[,c(drv,'SFS_index')]; df <- df %>% tidyr::drop_na()
  names(df) <- c('x','y')
  db <- cor.test(df$x, df$y, method = "spearman") %>% broom::tidy()
  db$Driver <- drv
  return(db)
}) %>% dplyr::bind_rows() %>% dplyr::select(Driver, estimate, p.value)
names(index)[2:3] <- c("Index_Spearman","Index_pvalue")

cor_df <- dplyr::left_join(x = index, y = delta, by = "Driver")
write.csv(cor_df, paste0(data_path, '/outputs/indicators/SFS_index_over_time/SFS_delta_index_vs_drivers_correlation.csv'), row.names = F)


cor(sfs_df[,-1], use = "pairwise.complete.obs", method = "pearson") %>% corrplot::corrplot()
cor(sfs_df[,-1], use = "pairwise.complete.obs", method = "spearman") %>% corrplot::corrplot()
