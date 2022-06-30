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

# importing GIS layers
input_GIS <- "C:/Users/mikewit/Documents/SEALINK/GIS/SEALINK/"

cur_coastline <- st_read(paste0(input_GIS,
                                "Layers/Topography/CUR_Coastline.shp")) %>%
  st_transform(crs = 4326)
cur_geology <- st_read(paste0(input_GIS,
                              "Layers/Geology/Geology Shapefile EATH.shp")) %>%
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

###############################################################################
# Making maps
###############################################################################

## 1. example for 1 plot with NO3 to show how it works ##
# NO3 map with fixed manual scale instead of automated scales
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

## With geology
# Note, continuous scale still needs to be replaced for quantile range scale

i <- "V"
d <- pts %>% 
  filter(parameter == i,
         sampletype == "groundwater") %>%
  mutate(name = case_when(
    parameter == "V" ~ "Vanadium",
    TRUE ~ "missing"))

ggplot() +
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


