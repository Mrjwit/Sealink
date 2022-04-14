#
# Input: cleaned hydrochemical data with coordinates
#         
# Output: Maps displaying spatial data
# 
# Dependencies: see initialisation
#
#
# Author: Mike Wit
# Date: 06-03-2022
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, ggmap, ggspatial, hrbrthemes, viridis,
               sf, leaflet, data.table, cowplot, data.table, ggnewscale,
               gridExtra, qpdf)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/" 

# load cleaned data
data <- openxlsx::read.xlsx(paste0(input, "hydrochemistry_curacao_2021.xlsx"))

# load tissue concentrations Nienke
data_Nienke <- openxlsx::read.xlsx(paste0("C:/Users/mikewit/Documents/SEALINK/Collaboration/",
                           "Nienke/220412_ICP_NE.xlsx"),
                    sheet = "Avg")
data_Nienke_Mike <- openxlsx::read.xlsx(paste0("C:/Users/mikewit/Documents/SEALINK/Collaboration/",
                                               "Nienke/hydrochemistry_curacao_2021_NVDL.xlsx"))

# importing GIS layers
input_GIS <- "C:/Users/mikewit/Documents/SEALINK/GIS/SEALINK/"

cur_coastline <- st_read(paste0(input_GIS,
                                "Layers/Topography/CUR_Coastline.shp")) %>%
  st_transform(crs = 4326)
cur_geology <- st_read(paste0(input_GIS,
                              "Layers/Geology/Geology Shapefile EATH.shp")) %>%
  st_transform(crs = 4326)

cur_zonalmap <- st_read(paste0(input_GIS,
                               "Layers/Landuse/Curacao_EOP.shp")) %>%
  st_transform(crs = 4326)

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# editing data
###############################################################################

# convert dataset to sf point object for plotting
pts <- st_as_sf(data %>% filter(year == 2021,
                                !is.na(xcoord)), coords = c("xcoord", "ycoord"),
                crs = 4326, agr = "constant") 

theme_set(theme_bw())

# editing data Nienke
# place in long format
d_Nienke <- data_Nienke %>%
  pivot_longer(cols = `Al.(mg/mL)`:`Zn.(mg/L)`,
               names_to = "parameter",
               values_to = "value") %>%
  mutate(units = str_extract(parameter, pattern = "\\([^()]+\\)"),
         parameter = sub("\\..*", "", parameter)) %>%
  mutate(x = ifelse(Site == "Eastpoint",x / 1000000, x / 10000000),
         y = ifelse(Site == "Eastpoint",y / 1000000, y / 10000000))

pts_N <- st_as_sf(d_Nienke %>% filter(!is.na(x)), coords = c("x", "y"),
                  crs = 4326)

# editing combined dataset coordinates Nienke
d <- data_Nienke_Mike %>%
  mutate(xcoord = case_when(
    samplecode == "Eastpoint" ~ xcoord / 1000000,
    is.na(watercode) ~ xcoord / 10000000,
    TRUE ~ xcoord )) %>%
  mutate(ycoord = case_when(
    samplecode == "Eastpoint" ~ ycoord / 1000000,
    is.na(watercode) ~ ycoord / 10000000,
    TRUE ~ ycoord )) %>%
  mutate(type = ifelse(is.na(watercode), 
                       "marine", "terrestrial"))

pts_comb <- st_as_sf(d %>% filter(!is.na(xcoord)), coords = c("xcoord", "ycoord"),
                     crs = 4326)

###############################################################################
# Making maps
###############################################################################

## 1. example for 1 plot with NO3 to show how it works ##
# NO3 map with fixed manual scale instead of automated 
ggplot(data = pts %>% filter(parameter == "NO3",
                             sampletype == "groundwater"),
       aes(fill = cut(value, breaks = c(0, 5, 10, 25, 50, 100, 300)))) +
  geom_sf(data = cur_coastline, fill = "antiquewhite") +
  geom_sf(#aes(fill = value), 
    colour = "black",
    pch = 21,
    size = 2) +
  scale_fill_manual(name = "NO3 [mg/l]",
                    labels = c("0 - 5",
                               "5 - 10",
                               "10 - 25",
                               "25 - 50",
                               "50 - 100",
                               ">100"),
                    values = c("cornflowerblue",
                               "lightgreen",
                               "darkolivegreen4",
                               "yellow",
                               "orange",
                               "red")
  ) +
  coord_sf(xlim = c(-69.15, -68.75),
           ylim = c(12.0, 12.4)) +
  ggtitle("Nitrate in groundwater") + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))
# save image
#ggsave(paste0(output, "Output/Figures/Maps/Nutrients/groundwater_NO3.png"))

## 2. automated plotting ## 
# metals with automated continuous scales
# select only relevant metals and rename for plotting
metals <- pts %>% 
  filter(method == "ICP-MS") %>%
  filter(!parameter %in% c("S", "P")) %>%
  # remove Ag and Cd as all are below dl
  #filter(!parameter %in% c("Ag", "Cd")) %>%
  # remove real NA values
  filter(!is.na(value)) %>%
  # change values <dl to NA so that these are grey in map
  mutate(value = ifelse(limit_symbol == "<", NA, value)) %>%
  mutate(name = case_when(
    parameter == "Ag" ~ "Silver", parameter == "Al" ~ "Aluminium", parameter == "As" ~ "Arsenic",
    parameter == "B" ~ "Boron", parameter == "Ba" ~ "Barium", parameter == "Be" ~ "Beryllium",
    parameter == "Ca" ~ "Calcium", parameter == "Cd" ~ "Cadmium", parameter == "Co" ~ "Cobalt",
    parameter == "Cr" ~ "Chromium", parameter == "Cu" ~ "Copper", parameter == "Fe" ~ "Iron",
    parameter == "Fe56" ~ "Iron-56", parameter == "Fe57" ~ "Iron-57", parameter == "K" ~ "Potassium",
    parameter == "Li" ~ "Lithium", parameter == "Mg" ~ "Magnesium", parameter == "Mg26" ~ "Magnesium-26",
    parameter == "Mn" ~ "Manganese", parameter == "Mo" ~ "Molybdenum", parameter == "Na" ~ "Sodium",
    parameter == "Ni" ~ "Nickel", parameter == "P" ~ "Phosphorus", parameter == "Pb" ~ "Lead",
    parameter == "S" ~ "Sulfur", parameter == "Sb" ~ "Antimony", parameter == "Se" ~ "Selenium",
    parameter == "Si" ~ "Silica", parameter == "Ti" ~ "Titanium", parameter == "V" ~ "Vanadium", 
    parameter == "Zn" ~ "Zinc",  
    TRUE ~ "missing" ))

# create empty list to place plots in during loop
plot_list = list()

for(i in sort(unique(metals$parameter))) {
  
  print(i)
  
  d <- metals %>% filter(parameter == i,
                         sampletype == "groundwater")

  p <- ggplot() +
    geom_sf(data = cur_geology, aes(fill = Field), alpha = 0.5) +
    scale_fill_manual(name = "Geology", 
                      values = c("yellowgreen", "purple4", "khaki2", "orange", "lightskyblue1")) +
    new_scale_fill() +
    geom_sf(data = d,
            aes(fill = value),
            colour = "black",
            pch = 21,
            size = 2) +
    scale_fill_distiller(type = "seq",
                      palette = "Spectral",
                      name = paste0(i, " [", unique(d$units), "]"),
                      direction = -1,
                      na.value = "grey50",
                      guide = "colourbar"
  ) +
    coord_sf(xlim = c(-69.15, -68.75),
             ylim = c(12.0, 12.4)) +
    ggtitle(paste(unique(d$name), "concentrations in groundwater on Curacao"),
            subtitle = paste("Values in grey are <", unique(d$detection_limit), unique(d$units))) + 
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "bl", which_north = "true",
                           pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    theme(panel.grid.major = element_line(color = gray(.5), 
                                          linetype = "dashed", 
                                          size = 0.5), 
          panel.background = element_rect(fill = "aliceblue"))
  
  # save individual plots
  ggsave(p, file = paste0(output, "Output/Figures/Maps/Metals2/", i, "_groundwater",  ".pdf"), 
         width = 210, height = 297, units = "mm")
  plot_list[[i]] = p
  
}
# merging PDF's into single file
setwd(paste0(output, "Output/Figures/Maps/Metals2/"))
file_list <- list.files()

qpdf::pdf_combine(input = unlist(file_list),
                  output = paste0(output, "Output/Figures/Maps/Metals2/1.Metals_groundwater.pdf"))

## 3. This loop uses concentration classes based on quantiles instead of continuous scales 
## to improve distinction between high and low values. This is not finished yet! 
# metals with flexible scale based on quantiles

# select only relevant metals and rename for plotting
metals <- pts %>% 
  filter(method == "ICP-MS") %>%
  filter(!parameter %in% c("S", "P")) %>%
  # remove Ag and Cd as all are below dl
  filter(!parameter %in% c("Ag", "Cd")) %>%
  filter(!is.na(value)) %>%
  mutate(name = case_when(
    parameter == "Ag" ~ "Silver", parameter == "Al" ~ "Aluminium", parameter == "As" ~ "Arsenic",
    parameter == "B" ~ "Boron", parameter == "Ba" ~ "Barium", parameter == "Be" ~ "Beryllium",
    parameter == "Ca" ~ "Calcium", parameter == "Cd" ~ "Cadmium", parameter == "Co" ~ "Cobalt",
    parameter == "Cr" ~ "Chromium", parameter == "Cu" ~ "Copper", parameter == "Fe" ~ "Iron",
    parameter == "Fe56" ~ "Iron-56", parameter == "Fe57" ~ "Iron-57", parameter == "K" ~ "Potassium",
    parameter == "Li" ~ "Lithium", parameter == "Mg" ~ "Magnesium", parameter == "Mg26" ~ "Magnesium-26",
    parameter == "Mn" ~ "Manganese", parameter == "Mo" ~ "Molybdenum", parameter == "Na" ~ "Sodium",
    parameter == "Ni" ~ "Nickel", parameter == "P" ~ "Phosphorus", parameter == "Pb" ~ "Lead",
    parameter == "S" ~ "Sulfur", parameter == "Sb" ~ "Antimony", parameter == "Se" ~ "Selenium",
    parameter == "Si" ~ "Silica", parameter == "Ti" ~ "Titanium", parameter == "V" ~ "Vanadium", 
    parameter == "Zn" ~ "Zinc",  
    TRUE ~ "missing" ))

# create empty list to place plots in during loop
plot_list = list()

for(i in sort(unique(metals$parameter))) {
  
  i <- "Pb" # to test
  print(i)
  
  d <- metals %>% filter(parameter == i,
                         sampletype == "groundwater")
  # how to deal with varying quantile classes between metals? 
  quant <- quantile(d$value, probs = seq(0, 1, by = 0.20)) 
  # adjust decimal numbers based on value classes
  quant[quant >= 10] <- round(quant[quant >= 10])
  quant[quant < 10 & quant >= 1] <- round(quant[quant < 10 & quant >= 1], digits = 1)
  quant[quant < 1 & quant >= 0.01] <- round(quant[quant < 1 & quant >= 0.01], digits = 2)
  quant[quant < 0.01] <- round(quant[quant < 0.01], digits = 3)
  
  # create classes per element based on quantiles to be used in plot legend 
  lab_list <- list()
  for(j in 1:length(unique(quant))) {
    # if(min(quant) == median(quant)) {
    #   lab <- paste("<", quant[j])
    #   lab_list[[j]] = lab
    # }
    if(j != length(unique(quant))) {
      lab <- c(paste(unique(quant)[j], "-", unique(quant)[j+1]))
      lab_list[[j]] = lab
    }
  }
  labs <- unlist(lab_list)
  #print(labs)
  
  # breaks for classes, still to be automated for when a lot of samples are < dl or have the same value
  # unique_quant <- c(unique(quantile(d$value, probs = seq(0, 1, by = 0.20))))
  # 
  # breaks <- ifelse(min(quant) == median(quant),
  #                  c(0, unique_quant), unique_quant)
  
  p <- ggplot() +
    geom_sf(data = cur_geology, aes(fill = Field), alpha = 0.5) +
    scale_fill_manual(name = "Geology", 
                      values = c("yellowgreen", "purple4", "khaki2", "orange", "lightskyblue1")) +
    new_scale_fill() +
    geom_sf(data = d,
            # asign classes to individual values using cut, not yet automated.. 
            # aes(fill = ifelse(unique(quant) > 1, 
            #                   cut(value, breaks = c(unique(quantile(value, probs = seq(0, 1, by = 0.20)))),
            #                   include.lowest = TRUE),
            #                   1)),
            aes(fill = cut(value, breaks = c(unique(quantile(value, probs = seq(0, 1, by = 0.20)))),
                           include.lowest = TRUE)),
            colour = "black",
            pch = 21,
            size = 2) +
    scale_fill_brewer(type = "qual",
                      palette = "RdGy",
                      name = paste0(i, " [", unique(d$units), "]"),
                      direction = -1,
                      na.value = "black",
                      labels = labs
    ) +
    coord_sf(xlim = c(-69.15, -68.75),
             ylim = c(12.0, 12.4)) +
    #theme(legend.position = c(0.75, 0.8)) +
    ggtitle(paste(unique(d$name), "concentrations in groundwater on Curacao")) + 
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "bl", which_north = "true",
                           pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    theme(panel.grid.major = element_line(color = gray(.5), 
                                          linetype = "dashed", 
                                          size = 0.5), 
          panel.background = element_rect(fill = "aliceblue"))
  

  ggsave(p, file = paste0(output, "Output/Figures/Maps/Metals2/", i, "_groundwater",  ".pdf"), 
         width = 210, height = 297, units = "mm")
  plot_list[[i]] = p
  
}
# merging PDF's into single file
setwd(paste0(output, "Output/Figures/Maps/Metals2/"))
file_list <- list.files()

qpdf::pdf_combine(input = unlist(file_list),
                  output = paste0(output, "Output/Figures/Maps/Metals2/1.Metals_groundwater.pdf"))

# Another option: create pdf where each page is a separate plot.
# pdf(paste0(output, "Output/Figures/Maps/Metals/1.Metals_groundwater.pdf"))
# for (i in 1:length(plot_list)) {
#     print(plot_list[[i]])
# }
# dev.off() 


##########
# Maps Nienke
##########

## 1. example for 1 plot with NO3 to show how it works ##
# NO3 map with fixed manual scale instead of automated 
ggplot(data = pts_N %>% filter(parameter == "Al")) +
  geom_sf(data = cur_coastline, fill = "antiquewhite") +
  geom_sf(aes(fill = value), 
    colour = "black",
    pch = 21,
    size = 2) +
  coord_sf(xlim = c(-69.15, -68.75),
           ylim = c(12.0, 12.4)) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))

# combined dataset
# scale_fill_distiller(type = "seq",
#                      palette = "Spectral",
#                      name = paste0(i, " [", unique(d$units), "]"),
#                      direction = -1,
#                      na.value = "grey50",
#                      guide = "colourbar"


pts_comb <- pts_comb %>% 
  # filter only metals
  filter(grepl("ICP", method)) %>%
  filter(!parameter %in% c("S", "P")) %>%
  # remove real NA values
  filter(!is.na(value)) %>%
  # change values <dl to NA so that these are grey in map
  mutate(limit_symbol = ifelse(is.na(limit_symbol), "", limit_symbol)) %>%
  mutate(value = ifelse(limit_symbol == "<", NA, value)) %>%
  # change name for plotting
  mutate(name = case_when(
    parameter == "Ag" ~ "Silver", parameter == "Al" ~ "Aluminium", parameter == "As" ~ "Arsenic",
    parameter == "B" ~ "Boron", parameter == "Ba" ~ "Barium", parameter == "Be" ~ "Beryllium",
    parameter == "Ca" ~ "Calcium", parameter == "Cd" ~ "Cadmium", parameter == "Co" ~ "Cobalt",
    parameter == "Cr" ~ "Chromium", parameter == "Cu" ~ "Copper", parameter == "Fe" ~ "Iron",
    parameter == "Fe56" ~ "Iron-56", parameter == "Fe57" ~ "Iron-57", parameter == "K" ~ "Potassium",
    parameter == "Li" ~ "Lithium", parameter == "Mg" ~ "Magnesium", parameter == "Mg26" ~ "Magnesium-26",
    parameter == "Mn" ~ "Manganese", parameter == "Mo" ~ "Molybdenum", parameter == "Na" ~ "Sodium",
    parameter == "Ni" ~ "Nickel", parameter == "P" ~ "Phosphorus", parameter == "Pb" ~ "Lead",
    parameter == "S" ~ "Sulfur", parameter == "Sb" ~ "Antimony", parameter == "Se" ~ "Selenium",
    parameter == "Si" ~ "Silica", parameter == "Ti" ~ "Titanium", parameter == "V" ~ "Vanadium", 
    parameter == "Zn" ~ "Zinc",  
    TRUE ~ "missing" ))

# create empty list to place plots in during loop
plot_list = list()

for(i in sort(unique(pts_comb$parameter))) {
  
  # for testing
  #i <- "As"
  print(i)
  
  d <- pts_comb %>%
    filter(parameter == i)

  p <- ggplot() +
    # Plot background map of Curacao
    geom_sf(data = cur_coastline, fill = "antiquewhite") +
    # Plot terrestrial concentrations
    geom_sf(data = d %>% filter(type == "terrestrial",
                                sampletype == "groundwater"), 
            aes(fill = value),
            colour = "black",
            pch = 21,
            size = 2) +
    # add colour scale for terrestrial samples with circles
    scale_fill_distiller(type = "seq",
                         palette = "Greens",
                         name = paste0("Terrestrial ", i, " [", 
                                       unique(d$units)[1], "]"),
                         direction = 1,
                         na.value = "grey50") +
    # add new scale for tissue samples
    new_scale_fill() +
    # Plot marine concentrations with squares
    geom_sf(data = d %>% filter(type == "marine"),
            aes(fill = value),
            colour = "black",
            pch = 22,
            size = 2) +
    # add colour scale for marine samples
    scale_fill_distiller(type = "seq",
                         palette = "Blues",
                         name = paste0("Niphates eracta ", i, " [", 
                                       unique(d$units)[2], "]"),
                         direction = 1,
                         na.value = "grey50") +
    # zoom in, excluding Klein Curacao
    coord_sf(xlim = c(-69.15, -68.75),
             ylim = c(12.0, 12.4)) +
    # add chart title
    ggtitle(paste(d$name[1], "concentrations in groundwater on land and in sponges, Curacao"),
            subtitle = paste("Values in grey are <", unique(d$detection_limit)[1], 
                             unique(d$units)[1])) + 
    # add scalebar
    annotation_scale(location = "bl", width_hint = 0.5) +
    # add north arrow
    annotation_north_arrow(location = "bl", which_north = "true",
                           pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    # add dashed grid and blue background
    theme(panel.grid.major = element_line(color = gray(.5), 
                                          linetype = "dashed", 
                                          size = 0.5), 
          panel.background = element_rect(fill = "aliceblue"))
  
  ggsave(p, file = paste0(output, "Output/Figures/Maps/Metals_gw_sponges/", i, "_groundwater_sponges",  ".pdf"), 
         width = 210, height = 297, units = "mm")
  plot_list[[i]] = p
  
}

# merging PDF's into single file
setwd(paste0(output, "Output/Figures/Maps/Metals_gw_sponges/"))
file_list <- list.files()

qpdf::pdf_combine(input = unlist(file_list),
                  output = paste0(output, "Output/Figures/Maps/Metals_gw_sponges/1.Metals_groundwater_sponges.pdf"))




