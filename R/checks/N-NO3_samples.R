#
# Input: Hydrochemistry dataset
#         
# Output: overview data N-NO3 samples
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 05-02-2024
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, lubridate, ggpubr, data.table, sf,
               ggspatial, viridis)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/final_merged/" 

# load cleaned data
data <- openxlsx::read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx"))
metadata <- openxlsx::read.xlsx(paste0(input, "metadata_2021_2022_2023.xlsx"))
meta_2020 <- openxlsx::read.xlsx(paste0(input, "metadata_2020.xlsx"))

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
output <- "C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/Lab/N-NO3/" 

###############################################################################
# Editing data
###############################################################################

# list of N-NO3 samples
no3_samples <- c("GW075", "GW076", "WW006", "WW007", "GW077", "GW081",
                 "GW082", "SW006", "GW083", "GW084", "SR019", "GW085",
                 "GW087", "SR020", "SR023", "SR024", "SR025", "GW088",
                 "GW089", "GW090", "GW091", "SR027", "GW092", "GW093",
                 "GW094", "GW095", "SR028", "SR029", "GW096", "SR030",
                 "SR031", "GW099", "GW100", "GW101", "SP004", "GW103",
                 "GW106", "GW107", "GW108", "GW109", "GW111B", "GW114",
                 "GW118", "SR035", "GW119", "SR036", "GW120", "WW009",
                 "GW121", "GW122A", "GW122B", "GW123", "GW124", "GW125A",
                 "GW125B", "SP005", "GW126A", "GW126C", "GW127", "GW128",
                 "GW129", "GW131", "GW132", "GW133B", "GW133C", "GW134",
                 "GW135", "WW010", "WW011", "GW141", "GW143", "GW142B",
                 "GW144", "GW145", "GW146", "GW147", "GW148", "GW149",
                 "GW153")

# add geology and filter N-NO3 samples
d <- data %>% 
  left_join(., metadata %>% dplyr::select(samplecode, geology)) %>%
  filter(samplecode %in% no3_samples)

# overview of EC, Cl, NO2, NO3 and NH4 per N-NO3 sample
d_wide <- d %>% filter(parameter %in% c("EC", "B", "Br", "Cl", "NO2", "NO3", "NH4", "PO4")) %>%
  mutate(value = paste0(limit_symbol, value)) %>%
  dplyr::select(samplecode, sampletype, subtype, xcoord, ycoord, geology, parameter, value) %>%
  pivot_wider(names_from = parameter, 
              values_from = value) %>%
  mutate(fieldwork = ifelse(row_number() >= 43, "fieldwork 3", "fieldwork 2"))
write.csv(d_wide, paste0(output, "overview_N-NO3_samples.csv"))

# adjusting detection limits to convert all data to numeric
d_wide <- d_wide %>%
  mutate(B = as.numeric(B),
         Br = as.numeric(ifelse(Br == "<1", 1, Br)),
         Cl = as.numeric(Cl),
         EC = as.numeric(EC),
         NO3 = as.numeric(ifelse(NO3 == "<0.4", 0.4, NO3)))


####
# Analysis
####

# spatial plot
# convert dataset to sf point object for plotting
pts <- st_as_sf(d_wide %>% filter(!is.na(xcoord)), coords = c("xcoord", "ycoord"),
                crs = 4326, agr = "constant") 
theme_set(theme_bw())

# 1. N-NO3 sample locations between fieldwork 2 and 3
ggplot(data = pts, 
       aes(fill = fieldwork)) +
  geom_sf(data = cur_coastline, fill = "antiquewhite") +
  geom_sf(#aes(fill = value), 
    colour = "black",
    pch = 21,
    size = 2) +
  coord_sf(xlim = c(-69.15, -68.75),
           ylim = c(12.0, 12.4)) +
  ggtitle("N-NO3 sample locations") + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))

# 2. N-NO3 sample types
ggplot(data = pts, 
       aes(fill = sampletype)) +
  geom_sf(data = cur_coastline, fill = "antiquewhite") +
  geom_sf(#aes(fill = value), 
    colour = "black",
    pch = 21,
    size = 2) +
  # scale_fill_manual(name = "NO3 [mg/l]",
  #                   labels = c("0 - 5",
  #                              "5 - 10",
  #                              "10 - 25",
  #                              "25 - 50",
  #                              "50 - 100",
  #                              ">100"),
  #                   values = c("cornflowerblue",
  #                              "lightgreen",
  #                              "darkolivegreen4",
  #                              "yellow",
#                              "orange",
#                              "red")
# ) +
coord_sf(xlim = c(-69.15, -68.75),
         ylim = c(12.0, 12.4)) +
  ggtitle("N-NO3 sample types") + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))

# 3. NO3 concentration of N-NO3 samples viridis continuous scale
ggplot(data = pts %>% mutate(NO3 = as.numeric(ifelse(NO3 == "<0.4", 0.4, NO3))), 
       aes(fill = NO3)) +
  geom_sf(data = cur_coastline, fill = "antiquewhite") +
  geom_sf(#aes(fill = value), 
    colour = "black",
    pch = 21,
    size = 2) +
  # scale_fill_manual(name = "NO3 [mg/l]",
  #                   labels = c("0 - 5",
  #                              "5 - 10",
  #                              "10 - 25",
  #                              "25 - 50",
  #                              "50 - 100",
  #                              ">100"),
  #                   values = c("cornflowerblue",
  #                              "lightgreen",
  #                              "darkolivegreen4",
  #                              "yellow",
  #                              "orange",
  #                              "red")
  # ) +
  scale_fill_viridis() +
  coord_sf(xlim = c(-69.15, -68.75),
         ylim = c(12.0, 12.4)) +
  ggtitle("Nitrate concentrations of N-NO3 samples") + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))

# 4. NO3 concentration of N-NO3 samples manual scale
ggplot(data = pts %>% mutate(NO3 = as.numeric(ifelse(NO3 == "<0.4", 0.4, NO3))), 
       aes(fill = cut(NO3, breaks = c(0, 5, 10, 25, 50, 100, 900)))) +
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
                               "red")) +
  coord_sf(xlim = c(-69.15, -68.75),
           ylim = c(12.0, 12.4)) +
  ggtitle("Nitrate concentrations of N-NO3 samples") + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))


# 4. NO3 concentration of N-NO3 samples viridis scale quantiles
pts <- pts %>% mutate(NO3 = as.numeric(ifelse(NO3 == "<0.4", 0.4, NO3)))

fill_colours = c("chartreuse4", "chartreuse3", "darkolivegreen2",
                 "khaki", "gold", "darkorange", "firebrick", "darkred")
## slightly changed to include 0 and 1
fill_values_quantiles <- seq(0, 1, length.out = length(fill_colours))
## use this for a vector of your quantile breaks for the labels (!)
quants <- quantile(pts$NO3, fill_values_quantiles)
## convert every value in your fill to quantiles
pts$ptile_var <- ecdf(pts$NO3)(pts$NO3)

ggplot(data = pts, 
       aes(fill = NO3)) +
  geom_sf(data = cur_coastline, fill = "antiquewhite") +
  geom_sf(#aes(fill = value), 
    colour = "black",
    pch = 21,
    size = 2) +
  # scale_fill_manual(name = "NO3 [mg/l]",
  #                   labels = c("0 - 5",
  #                              "5 - 10",
  #                              "10 - 25",
  #                              "25 - 50",
  #                              "50 - 100",
  #                              ">100"),
  #                   values = c("cornflowerblue",
  #                              "lightgreen",
  #                              "darkolivegreen4",
  #                              "yellow",
  #                              "orange",
  #                              "red")
  # ) +
  scale_fill_gradientn(colours = fill_colours, 
                       breaks = fill_values_quantiles, 
                       labels = round(quants, 3)) +
  coord_sf(xlim = c(-69.15, -68.75),
           ylim = c(12.0, 12.4)) +
  ggtitle("Nitrate concentrations of N-NO3 samples") + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))


## B vs NO3 ##

# for all samples
ggplot(d_wide, aes(x = NO3, y = B)) +
  geom_point() +
  labs(x = "NO3 (mg/L)", 
       y = "B (ug/L)",
       title = "All N-NO3 sample types")

# for only groundwater samples
ggplot(d_wide %>% filter(sampletype == "groundwater"), aes(x = NO3, y = B)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "NO3 (mg/L)", 
       y = "B (ug/L)",
       title = "N-NO3 groundwater samples")






