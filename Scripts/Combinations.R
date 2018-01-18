
countries2 <- countries
countries2@data <- left_join(x = countries@data, y = all_data, by = c("ISO3" = "iso3c"))
xy <- coordinates(countries2)

library(sf)
countries <- st_read(dsn = "./Input_data/world_shape", layer = "all_countries")
countries <- left_join(x = countries, y = all_data, by = c("ISO3" = "iso3c"))
countries2 <- as(countries, "Spatial")

library(spdep)
w <- poly2nb(countries2, row.names = countries2$OBJECTID)
condition <- unlist(lapply(1:length(w), function(i){
  if(length(w[[i]]) == 1){
    if(w[[i]] == 0){
      z <- TRUE
    } else {
      z <- FALSE
    }
  } else {
    z <- FALSE
  }
  return(z)
}))
# w2 <- w[!condition]
# class(w2) <- "nb"

countries2 <- st_as_sf(as(countries2, "SpatialPolygonsDataFrame"))
countries2 <- countries2[!condition,]; countries2 <- as(countries2, "Spatial")
xy <- coordinates(countries2)

w <- poly2nb(countries2, row.names = countries2$OBJECTID)

plot(countries2, col = 'gray', border = 'blue', lwd = 2)
plot(w, xy, col = 'red', lwd = 2, add = TRUE)
wm <- nb2mat(w, style='B')
wm

# Moran Index
n <- length(countries2)
y <- countries2$City.access
ybar <- mean(y)

dy <- y - ybar
g <- expand.grid(dy, dy)
yiyj <- g[,1] * g[,2]

pm <- matrix(yiyj, ncol=n)

pmw <- pm * wm
pmw

spmw <- sum(pmw)
spmw

smw <- sum(wm)
sw  <- spmw / smw

vr <- n / sum(dy^2)

MI <- vr * sw
MI

EI <- -1/(n-1)
EI

ww <-  nb2listw(w, style='B')
ww

moran.test(countries2$Obesity, ww, randomisation=FALSE)
moran.mc(countries2$Obesity, ww, nsim=99)

spplot(countries2, "FabioVar")
spplot(countries2, "City.access")

sum(complete.cases(all_data[,2:6]))
sum(complete.cases(all_data[,3:7]))
sum(complete.cases(all_data[,4:8]))



















# ================================================ #
# Environment
# ================================================ #
envPos <- 2:8
envCountries <- lapply(1:length(envPos), function(i){
  combinations <- combn(x = names(all_data)[envPos], m = i)
  combCounts <- unlist(lapply(1:ncol(combinations), function(j){
    nCountries <- sum(complete.cases(all_data[,combinations[,j]]))
    return(nCountries)
  }))
})
names(envCountries) <- 1:length(envPos)
envCountries2 <- data.frame(Indicators = rep(names(envCountries), lapply(envCountries, length)),
                            Countries = unlist(envCountries),
                            Dimension = "Environment")
library(tidyverse)
envCountries2 %>% ggplot(aes(x = Indicators, y = Countries)) + geom_point()

# ================================================ #
# Economic
# ================================================ #
ecoPos <- 9:11
ecoCountries <- lapply(1:length(ecoPos), function(i){
  combinations <- combn(x = names(all_data)[ecoPos], m = i)
  combCounts <- unlist(lapply(1:ncol(combinations), function(j){
    # nCountries <- sum(complete.cases(all_data[,combinations[,j]]))
    lComb <- list(nCountries = sum(complete.cases(all_data[,combinations[,j]])),
                  Indicators = paste(combinations[,j], collapse = ", "))
    return(lComb) # nCountries
  }))
})
names(ecoCountries) <- 1:length(ecoPos)
ecoCountries2 <- data.frame(Indicators = rep(names(ecoCountries), lapply(ecoCountries, length)),
                            Countries = unlist(ecoCountries),
                            Dimension = "Economic")
library(tidyverse)
ecoCountries2 %>% ggplot(aes(x = Indicators, y = Countries)) + geom_point()

# ================================================ #
# Social
# ================================================ #
socPos <- 12:14
socCountries <- lapply(1:length(socPos), function(i){
  combinations <- combn(x = names(all_data)[socPos], m = i)
  combCounts <- unlist(lapply(1:ncol(combinations), function(j){
    nCountries <- sum(complete.cases(all_data[,combinations[,j]]))
    return(nCountries)
  }))
})
names(socCountries) <- 1:length(socPos)
socCountries2 <- data.frame(Indicators = rep(names(socCountries), lapply(socCountries, length)),
                            Countries = unlist(socCountries),
                            Dimension = "Social")
library(tidyverse)
socCountries2 %>% ggplot(aes(x = Indicators, y = Countries)) + geom_point()

# ================================================ #
# Food and nutrition
# ================================================ #
fntPos <- 15:28
fntCountries <- lapply(1:length(fntPos), function(i){
  combinations <- combn(x = names(all_data)[fntPos], m = i)
  combCounts <- unlist(lapply(1:ncol(combinations), function(j){
    nCountries <- sum(complete.cases(all_data[,combinations[,j]]))
    return(nCountries)
  }))
})
names(fntCountries) <- 1:length(fntPos)
fntCountries2 <- data.frame(Indicators = rep(names(fntCountries), lapply(fntCountries, length)),
                            Countries = unlist(fntCountries),
                            Dimension = "Food and nutrition")
library(tidyverse)
fntCountries2 %>% ggplot(aes(x = Indicators, y = Countries)) + geom_point()

all_combinations <- rbind(envCountries2, ecoCountries2, socCountries2, fntCountries2)
all_combinations$Indicators <- all_combinations$Indicators %>% as.character %>% as.numeric
all_combinations %>% ggplot(aes(x = Indicators, y = Countries)) +
  geom_point() + facet_wrap(~Dimension) +
  scale_x_continuous(breaks = 1:14)








allPos <- 2:28
allCountries <- lapply(1:length(allPos), function(i){
  combinations <- combn(x = names(all_data)[allPos], m = i)
  combCounts <- unlist(lapply(1:ncol(combinations), function(j){
    nCountries <- sum(complete.cases(all_data[,combinations[,j]]))
    return(nCountries)
  }))
})
names(allCountries) <- 1:length(fntPos)
allCountries2 <- data.frame(Indicators = rep(names(allCountries), lapply(allCountries, length)),
                            Countries = unlist(allCountries),
                            Dimension = "All")
library(tidyverse)
allCountries2 %>% ggplot(aes(x = Indicators, y = Countries)) + geom_point()










