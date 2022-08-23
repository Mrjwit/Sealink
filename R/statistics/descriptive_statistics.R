#
# Input: hydrochemical dataset of Curacao with metadata file
#         
# Output: Tabel of descriptive statistics
# 
# Dependencies: none
#
# Author: Mike Wit
# Date: 15-08-2022
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, moments, scales)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/" 

# load cleaned data
data <- openxlsx::read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx"))
metadata <- openxlsx::read.xlsx(paste0(input, "metadata_2021.xlsx"))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/Output/Statistics/" 

###############################################################################
# Editing data
###############################################################################

# select only 2021, water samples and relavant parameters
d <- data %>%
  filter(year == 2021, sampletype != "air", 
         !parameter %in% c("DO_sat", "HCO3_field", "HCO3_lab", "NO2_field", "NO3_field", "P", "S"),
         !is.na(value)) %>%
  group_by(parameter) %>%
  summarise(units = unique(units),
            dl = paste(unique(detection_limit, collapse = ", ")),
            `% <dl` = round(length(value[limit_symbol == "<"]) / length(value) * 100, digits = 1),
            mean = round(mean(value, na.rm = T), digits = 1),
            median = round(median(value, na.rm = T), digits = 1),
            min = round(min(value, na.rm = T), digits = 1),
            max = round(max(value, na.rm = T), digits = 1),
            sd = round(sd(value, na.rm = T), digits = 1),
            skewness = round(skewness(value, na.rm = T), digits = 1)) 

write.xlsx(d, paste0(output, "descriptive_statistics.xlsx"))

data %>%
  filter(year == 2021, sampletype != "air", 
         !parameter %in% c("DO_sat", "HCO3_field", "HCO3_lab", "NO2_field", "NO3_field", "P", "S")) %>%
  filter(is.na(detection_limit)) %>%
  view()

data %>%
  filter(year == 2021, sampletype != "air", 
         parameter %in% c("Ag", "Cd")) %>%
  view()
