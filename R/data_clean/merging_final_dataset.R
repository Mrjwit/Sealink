#
# Input: all cleaned datafiles
#         
# Output: combined final hydrochemical dataset
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 24-03-2022
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, ggmap, 
               sf, leaflet, data.table, cowplot, data.table)

###############################################################################
# load data
###############################################################################

## Loading data
# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/" 

# load cleaned data of 2021-2022 fieldwork
alk <- openxlsx::read.xlsx(paste0(input, "alkalinity_clean.xlsx"))
ecoli <- openxlsx::read.xlsx(paste0(input, "ecoli_clean.xlsx"))
radon <- openxlsx::read.xlsx(paste0(input, "radon_clean.xlsx"))
labdata <- openxlsx::read.xlsx(paste0(input, "lab_data_long.xlsx"))
isotopes <- openxlsx::read.xlsx(paste0(input, "isotopes_clean.xlsx"))
metadata <- openxlsx::read.xlsx(paste0(input, "survey_clean.xlsx"))

# load dataset of Jessie (2020-2021)
#
#

# load historic hydrochemical dataset of 1977 and 1992
data1977_1992 <- openxlsx::read.xlsx(paste0(input, "hydrochemistry1977-1992.xlsx"))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################

## Editing data
# put metadata of field measurements in long format so that in can be merged
d <- metadata %>%
  select(samplecode, EC_uS, pH, Temp, DO, DO_sat, redox, NO3, NO2) %>%
  rename(NO3_field = NO3,
         NO2_field = NO2,
         Eh = redox) %>%
  pivot_longer(cols = EC_uS:NO2_field,
               names_to = "parameter",
               values_to = "value") %>%
  mutate(limit_symbol = "",
         detection_limit = NA,
         units = case_when(
           parameter == "EC_uS" ~ "uS/cm",
           parameter == "pH" ~ "",
           parameter == "Temp" ~ "Degrees Celcius",
           parameter == "DO" ~ "mg/l",
           parameter == "DO_sat" ~ "%",
           parameter == "Eh" ~ "mV",
           parameter == "NO3_field" ~ "mg/l",
           parameter == "NO2_field" ~ "mg/l",
           TRUE ~ "missing" ),
         method = ifelse(parameter %in% c("NO3_field", "NO2_field"), "Nitrate strip",
                         "Ponsol sensors"),
         notes = "")

# Merge all cleaned datasets but keep metadata separated for now, # Remove units mg N/L and mg P/L
data <- rbind(labdata, 
              alk %>% filter(units == "mg/l"), ecoli, radon, d, isotopes %>% select(-std)) %>%
  arrange(samplecode, parameter) %>%
  mutate(watercode = substr(samplecode, start = 1, stop = 2)) %>%
  # Remove units mg N/L and mg P/L, for PO4 only select DA analysis
  mutate(flag = ifelse(parameter == "PO4" & method == "IC", 1, 0)) %>%
  filter(!units %in% c("mg N/L", "mg P/L"),
         flag == 0) %>%
  select(-flag)

# Add other relevant metadata
d_meta <- metadata %>%
  select(samplecode, Well.type, `Well.depth.below.surface.(m)`, `Depth.of.well.owner.(m)`,
         `Groundwater.level.below.surface.(m)`, sample_depth, sample_method,
         `Geology.according.to.geological.map.(Beets)`, Land.use.based.on.own.observations)

###############################################################################
# Adjust HCO3 values
###############################################################################

# adjust some HCO3 values where they were not consistent with ionic balance
# based on the plots and ionic balance, some changes for HCO3:
# GW002 -> use HCO3 lab instead of HCO3 field ???? Maybe field titration is better and difference is caused by Na/Cl
# GW028 -> use different HCO3 titration -> adjusted in alkalinity sheet
# GW033 -> use HCO3 lab 
# RW001 -> use HCO3 lab
# RW002 -> use HCO3 lab
# SR003 -> use different HCO3 titration -> adjusted in alkalinity sheet

## Convert Alkalinity as CaCO3 to HCO3??? ##

data$parameter <- data$parameter %>%
  recode("HCO3" = "HCO3_field")
d <- data %>%
  filter(parameter %in% c("HCO3_field", "HCO3_lab")) %>%
  select(samplecode, parameter, value) %>%
  pivot_wider(values_from = value,
              names_from = parameter) %>%
  mutate(HCO3 = case_when(
    #is.na(HCO3_field) ~ HCO3_lab,
    samplecode == "GW002" ~ HCO3_lab,
    samplecode == "GW033" ~ HCO3_lab,
    samplecode == "RW001" ~ HCO3_lab,
    samplecode == "RW002" ~ HCO3_lab,
    TRUE ~ HCO3_field
  )) %>%
  pivot_longer(cols = HCO3_field:HCO3,
               names_to = "parameter",
               values_to = "value") %>%
  mutate(limit_symbol = "",
         detection_limit = NA,
         units = "mg/l",
         method = ifelse(samplecode %in% c("GW002", "GW033", "RW001", "RW002"),
                         "DA ALKALIN 550", "Field titration"),
         notes = "", 
         watercode = substr(samplecode, start = 1, stop = 2)) %>%
  filter(parameter == "HCO3")

# add right HCO3 values back to dataset
data <- rbind(data, d) %>%
  arrange(samplecode, parameter)

###############################################################################
# Add watertypes
###############################################################################

# add main watertypes 
data$sampletype <- data$watercode %>% 
  recode("AI" = "air",
         "GW" = "groundwater",
         "SR" = "surfacewater",
         "SW" = "surfacewater", 
         "SP" = "groundwater",
         "WW" = "wastewater",
         "RW" = "rainwater",
         "TW" = "tapwater",
         "SE" = "seawater")

# add secondary watertypes
data$subtype <- data$watercode %>% 
  recode("AI" = "air",
         "GW" = "groundwater",
         "SR" = "surface runoff",
         "SW" = "surfacewater", 
         "SP" = "spring",
         "WW" = "wastewater",
         "RW" = "rainwater",
         "TW" = "tapwater",
         "SE" = "seawater") 

# differentiate between treated (WW001, SW001-SW002) and untreated (WW002-WW003) wastewater
data <- data %>%
  mutate(subtype = case_when(
    samplecode %in% c("WW001", "SW001", "SW002") ~ "treated wastewater",
    samplecode %in% c("WW002", "WW003") ~ "untreated wastewater",
    TRUE ~ subtype ))

###############################################################################
# add other hydrochemistry datasets
###############################################################################

## data 1977 and 1992
# put in right format
d <- data1977_1992 %>%
  mutate(samplecode = putcode,
         limit_symbol = dl,
         detection_limit = NA,
         method = "",
         notes = "",
         watercode = "GW",
         sampletype = "groundwater",
         subtype = "groundwater",
         xcoord = lon,
         ycoord = lat) %>%
  select(samplecode, year, parameter, value, limit_symbol, detection_limit,
         units, method, notes, watercode, sampletype, subtype, xcoord, ycoord)

data <- rbind(data %>% mutate(year = 2021,
                              xcoord = NA,
                              ycoord = NA), d) %>%
  select(samplecode, year, parameter, value, limit_symbol, detection_limit,
         units, method, notes, watercode, sampletype, subtype, xcoord, ycoord)

###############################################################################
# save final database
###############################################################################

# hydrochemistry
openxlsx::write.xlsx(data, paste0(output, "Clean_data/hydrochemistry_curacao.xlsx"))

# metadata file



