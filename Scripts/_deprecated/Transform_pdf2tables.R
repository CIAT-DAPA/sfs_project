# Plotting a raster just using coordinates and values to be plotted
# H. Achicanoy
# CIAT, 2017

suppressMessages(library(ggplot2))

plot_func <- function(df, name) {
  ggplot(data = df, aes(x = lon, y = lat, fill = rdepth)) +
    geom_raster() +
    coord_equal() +
    theme_bw() +
    scale_fill_continuous(name = name)
}

# plot_func(df = soil_data, name = "Soilcp")

nested_tmp <- df %>% 
  group_by(Index) %>% 
  nest() %>% 
  mutate(plots = map2(data, Index, plot_func)) 

gridExtra::grid.arrange(grobs = nested_tmp$plots)

# Implementing PLS-PM model
# H. Achicanoy
# CIAT, 2017

suppressMessages(library(plspm))
suppressMessages(library(tabplot))
data("russett")
tableplot(russett)

# path matrix (inner model realtionships)
AGRIN = c(0, 0, 0)
INDEV = c(0, 0, 0)
POLINS = c(1, 1, 0)
rus_path = rbind(AGRIN, INDEV, POLINS)
rm(AGRIN, INDEV, POLINS)

# add optional column names
colnames(rus_path) = rownames(rus_path)

# plot the path matrix
innerplot(rus_path)

# list indicating what variables are associated with what latent variables
rus_blocks = list(1:3, 4:5, 6:11)

# all latent variables are measured in a reflective way
rus_modes = rep("A", 3)

# run plspm analysis
rus_pls = plspm(russett, rus_path, rus_blocks, modes = rus_modes, boot.val = 1000)

# path coefficients
rus_pls$path_coefs

# inner model
rus_pls$inner_model

cor(rus_pls$scores, method = "spearman")

suppressMessages(library(caret))








pdftables::convert_pdf(input_file = "C:/Users/haachicanoy/Downloads/selection-rotated.pdf", output_file = "C:/Users/haachicanoy/Downloads/vitaminA_data.csv", api_key = "9e9usfr0t98i")
pdftables::convert_pdf(input_file = "//dapadfs/Workspace_cluster_9/Sustainable_Food_System/Input_data_final/Food_Nutrition/VitaminA_deficiency/IDD_estimates_table_2007.pdf", output_file = "C:/Users/haachicanoy/Downloads/iodine_data.csv", api_key = "9e9usfr0t98i")

library(rvest)
library(stringr)
page <- html("http://www.who.int/vmnis/database/vitamina/countries/en/")

docsList <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("\\.pdf")# %>% # find those that end in xlsx
#.[[1]]                    # look at the first one

namesList <- strsplit(x = docsList, split = "/")
namesList <- unlist(lapply(namesList, function(x){x[[length(x)]]}))

Map('download.file', mode = "wb", url = docsList, destfile = paste0("./Input_data_final/Food_Nutrition/VitaminA_deficiency/", namesList))

download.file(pdf.url, pdf.name, method = 'auto', quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
