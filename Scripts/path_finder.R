
data = all_data; combList = dfs2$Indicators[[761]]

path_finder <- function(data = all_data, combList = dfs2$Indicators[[761]], id = nInd){
  
  # -------------------------------------------------------------------- #
  # Verify names position
  # -------------------------------------------------------------------- #
  envPos <- 2:8; ecoPos <- 9:11; socPos <- 12:14; fntPos <- 15:28
  mtch <- match(combList, names(data))
  envUpt <- base::intersect(envPos, mtch)
  ecoUpt <- base::intersect(ecoPos, mtch)
  socUpt <- base::intersect(socPos, mtch)
  fntUpt <- base::intersect(fntPos, mtch)
  
  # -------------------------------------------------------------------- #
  # Create all possible combinations for each dimension
  # -------------------------------------------------------------------- #
  # Environment
  envPos <- envUpt
  envCountries <- lapply(1:length(envPos), function(i){
    combinations <- combn(x = names(all_data)[envPos], m = i) # Combination of indicators
    combCounts <- lapply(1:ncol(combinations), function(j){
      lComb <- list(Indicators = combinations[,j])
      return(lComb)
    })
    return(combCounts)
  })
  names(envCountries) <- 1:length(envPos)
  env_mxIndc <- purrr::modify_depth(envCountries, 2, "Indicators")
  
  # Economic
  ecoPos <- ecoUpt
  ecoCountries <- lapply(1:length(ecoPos), function(i){
    combinations <- combn(x = names(all_data)[ecoPos], m = i) # Combination of indicators
    combCounts <- lapply(1:ncol(combinations), function(j){
      lComb <- list(Indicators = combinations[,j])
      return(lComb)
    })
    return(combCounts)
  })
  names(ecoCountries) <- 1:length(ecoPos)
  eco_mxIndc <- purrr::modify_depth(ecoCountries, 2, "Indicators")
  
  # Social
  socPos <- socUpt
  socCountries <- lapply(1:length(socPos), function(i){
    combinations <- combn(x = names(all_data)[socPos], m = i) # Combination of indicators
    combCounts <- lapply(1:ncol(combinations), function(j){
      lComb <- list(Indicators = combinations[,j])
      return(lComb)
    })
    return(combCounts)
  })
  names(socCountries) <- 1:length(socPos)
  soc_mxIndc <- purrr::modify_depth(socCountries, 2, "Indicators")
  
  # Food and nutrition
  fntPos <- fntUpt
  fntCountries <- lapply(1:length(fntPos), function(i){
    combinations <- combn(x = names(all_data)[fntPos], m = i) # Combination of indicators
    # For each combination of indicators calculate the number of countries with complete data
    combCounts <- lapply(1:ncol(combinations), function(j){
      lComb <- list(Indicators = combinations[,j])
      return(lComb)
    })
    return(combCounts)
  })
  names(fntCountries) <- 1:length(fntPos)
  fnt_mxIndc <- purrr::modify_depth(fntCountries, 2, "Indicators")
  
  # -------------------------------------------------------------------- #
  # Concatenate names within all possibilities in each dimension
  # -------------------------------------------------------------------- #
  env_mxIndc <- env_mxIndc %>% purrr::map(., function(x){lapply(1:length(x), function(i){paste(x[[i]], collapse = "_")})})
  eco_mxIndc <- eco_mxIndc %>% purrr::map(., function(x){lapply(1:length(x), function(i){paste(x[[i]], collapse = "_")})})
  soc_mxIndc <- soc_mxIndc %>% purrr::map(., function(x){lapply(1:length(x), function(i){paste(x[[i]], collapse = "_")})})
  fnt_mxIndc <- fnt_mxIndc %>% purrr::map(., function(x){lapply(1:length(x), function(i){paste(x[[i]], collapse = "_")})})
  
  # -------------------------------------------------------------------- #
  # Concatenate names between all possibilities
  # -------------------------------------------------------------------- #
  a <- list(unlist(env_mxIndc),
            unlist(eco_mxIndc),
            unlist(soc_mxIndc),
            unlist(fnt_mxIndc))
  a <- paste0('[',
              '[', paste('"', unlist(env_mxIndc), '"', sep = '', collapse = ','),']', ',',
              '[', paste('"', unlist(eco_mxIndc), '"', sep = '', collapse = ','),']', ',',
              '[', paste('"', unlist(soc_mxIndc), '"', sep = '', collapse = ','),']', ',',
              '[', paste('"', unlist(fnt_mxIndc), '"', sep = '', collapse = ','),']',
              ']')
  
  # setwd("~")
  createCode <- function(code){
    
    sink(code)
    cat(paste0('a=', a), fill = T)
    cat('r=[[]]', fill = T)
    cat('for x in a:', fill = T)
    cat('\t t = []', fill = T)
    cat('\t for y in x:', fill = T)
    cat('\t \t for i in r:', fill = T)
    cat('\t \t \t t.append(i+[y])', fill = T)
    cat('\t r = t', fill = T)
    # cat('f = open("D:/ToBackup/repositories/cc-repo/sfs_project/Scripts/best_combination.txt", "w")', fill = T)
    cat('f = open("//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/Best_combinations/best_combination.txt", "w")', fill = T)
    cat('z = str(r)', fill = T)
    cat('f.write(z)', fill = T)
    cat('f.close()', fill = T)
    sink()
    shell(code)
    
  }
  createCode(code = '//dapadfs/Workspace_cluster_9/Sustainable_Food_System/SFS_indicators/Scripts/Do_mixed_combinations.py')
  
  file.rename(from = "./Best_combinations/best_combination.txt",
              to   = paste0("./Best_combinations/best_combination_", id, ".txt"))
  textFile <- readLines(paste0("./Best_combinations//best_combination_", id, ".txt"))
  textFile <- unlist(strsplit(x = textFile, split = "], [", fixed = T))
  textFile <- lapply(textFile, function(x){
    
    txt_str <- x
    txt_str <- gsub(pattern = "_", replacement = ", ", x = txt_str)
    txt_str <- gsub(pattern = "'", replacement = "", x = txt_str)
    txt_str <- gsub(pattern = "\\]\\]", replacement = "", x = txt_str)
    txt_str <- gsub(pattern = "\\[\\[", replacement = "", x = txt_str)
    txt_str <- unlist(strsplit(x = txt_str, split = ", "))
    return(txt_str)
    
  })
  
  finalCombinations <- lapply(1:length(textFile), function(i){
    df <- data.frame(
      nCountries = sum(complete.cases(all_data[,textFile[[i]]])),
      nIndicators = length(textFile[[i]])
    )
    return(df)
  })
  finalCombinations <- do.call(rbind, finalCombinations)
  finalCombinations <- finalCombinations %>% dplyr::mutate(Indicators = textFile)
  saveRDS(finalCombinations, paste0("./Best_combinations/RDSfiles/bc_", id, ".rds"))
  
  return(cat("Done.\n"))
  
}
finalCombinations <- path_finder(data = all_data, combList = dfs2$Indicators[[761]])

stringdist::stringdist(a = finalCombinations$Indicators[6][[1]] %>% paste(collapse = "_"),
                       b = finalCombinations$Indicators[9][[1]] %>% paste(collapse = "_"))

finalCombinations$matches <- lapply(1:nrow(finalCombinations), function(i){
  sum(dfs2$Indicators[[1]] %in% finalCombinations$Indicators[[i]])
}) %>% unlist

dim(finalCombinations)
dim(finalCombinations %>% dplyr::filter(matches == 4))

finalCombinations <- finalCombinations %>% dplyr::filter(matches == 4)
finalCombinations %>% dplyr::group_by(nIndicators)

nInd <- finalCombinations$nIndicators %>% unique %>% sort
for(i in 2:length(nInd)){
  
  sub_df1 <- finalCombinations %>% dplyr::filter(nIndicators == nInd[i-1])
  sub_df2 <- finalCombinations %>% dplyr::filter(nIndicators == nInd[i])
  
  lapply(1:nrow(sub_df2), function(i){
    sum(sub_df1$Indicators[[1]] %in% sub_df2$Indicators[[i]])
  }) %>% unlist
  
}



finalCombinations %>% dplyr::filter(matches == 4) %>% ggplot(aes(x = nIndicators, y = nCountries)) + geom_point() +
  xlab("Number of indicators") +
  ylab("Countries with complete data") +
  scale_x_continuous(breaks = 4:20) +
  theme_bw()

finalCombinations %>% dplyr::filter(nIndicators == 5 & nCountries < 110 & matches == 4)

