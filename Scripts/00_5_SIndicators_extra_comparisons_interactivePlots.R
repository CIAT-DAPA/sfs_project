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

purrr::cross3(.x = env_mtchs$combination %>% purrr::map(.f = function(x){colnames(all_data[,env_combinations[,x]])}),
              .y = list(c(colnames(all_data[,ecoPos]), colnames(all_data[,socPos]))),
              .z = fnt_mtchs$combination %>% purrr::map(.f = function(x){colnames(all_data[,fnt_combinations[,x]])}))

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
  return(db_res)
  
})
index_simulation <- do.call(rbind, index_simulation)
index_simulation %>%
  ggplot(aes(x = reorder(iso3c, SFS_index, FUN = median), y = SFS_index)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(0.2)) +
  geom_point(data = sfs_index[which(sfs_index$iso3c %in% unique(index_simulation$iso3c)),] %>%
               select(iso3c, SFS_index),
             aes(x = iso3c, y = SFS_index), col = "red", size = 5) +
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





library(tidyverse)
library(sf)
library(rworldmap)

sfs_index <- read.csv("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/sfs_index_normalized_indicators.csv")
data("countriesCoarse")

# ----------------------------- #
# Creating map
# ----------------------------- #

countriesCoarse <- merge(countriesCoarse, sfs_index, by.x = "ISO3", by.y = "iso3c")

pal <- colorNumeric('Reds', NULL)

map <- leaflet(countriesCoarse) %>%
  addProviderTiles('CartoDB.Positron') %>%
  clearShapes() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0, 
              fillColor = ~pal(SFS_index), fillOpacity = 0.7, 
              layerId = ~ISO3) %>%
  addLegend(position = 'bottomright', pal = pal, 
            values = countriesCoarse$SFS_index, title = 'Score')
map

# ----------------------------- #
# Creating map
# ----------------------------- #
