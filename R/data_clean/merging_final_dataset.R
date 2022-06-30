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
  dplyr::rename(NO3_field = NO3,
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
  select(samplecode, Well.ID, xcoord, ycoord, date, time, Well.type, `Inner.well.diameter.(m)`, `Well.depth.below.surface.(m)`, 
         `Depth.of.well.owner.(m)`, Note.on.well.identification, 
         `Groundwater.level.below.surface.(m)`, sample_depth, sample_method, sample_notes, 
         `Geology.according.to.geological.map.(Beets)`, `Other.-.Geology.according.to.geological.map.(Beets)`, 
         Land.use.based.on.own.observations, `Other.-.Land.use.based.on.own.observations`,
         `House./.location.waste.water.collection`, `Well.distance.from.house.(m)`, `Note.on.sewage.(in.the.area)`, 
         Name.owner, Address, `Contact.mail/phone.number:`)

# add 1 geology column and make distinction between east and west CLF
d_meta <- d_meta %>%
  mutate(geology = case_when(
    `Geology.according.to.geological.map.(Beets)` == "Limestones" ~ "Limestones",
    `Geology.according.to.geological.map.(Beets)` == "Limestones_and_Marls" ~ "Limestones",
    `Geology.according.to.geological.map.(Beets)` == "Knip_group" ~ "Knip Group",
    `Geology.according.to.geological.map.(Beets)` == "Midden_Curacao_formation" ~ "Curacao Midden Formation",
    `Geology.according.to.geological.map.(Beets)` == "Curacao_lava_formation" & xcoord >= -69.009065 ~ "Curacao Lava Formation East",
    `Geology.according.to.geological.map.(Beets)` == "Curacao_lava_formation" & xcoord < -69.009065 ~ "Curacao Lava Formation West",
    `Other.-.Geology.according.to.geological.map.(Beets)` == "knip group, intrusives " ~ "Knip Group - intrusive",
    TRUE ~ "Other" )) %>%
  mutate(geology_abr = case_when(
    `Geology.according.to.geological.map.(Beets)` == "Limestones" ~ "L",
    `Geology.according.to.geological.map.(Beets)` == "Limestones_and_Marls" ~ "L",
    `Geology.according.to.geological.map.(Beets)` == "Knip_group" ~ "K",
    `Geology.according.to.geological.map.(Beets)` == "Midden_Curacao_formation" ~ "M",
    `Geology.according.to.geological.map.(Beets)` == "Curacao_lava_formation" & xcoord >= -69.009065 ~ "DO",
    `Geology.according.to.geological.map.(Beets)` == "Curacao_lava_formation" & xcoord < -69.009065 ~ "DW",
    `Other.-.Geology.according.to.geological.map.(Beets)` == "knip group, intrusives " ~ "K - intrusive",
    TRUE ~ "Other" ))

# change classes of land use...
# importing GIS layers
input_GIS <- "C:/Users/mikewit/Documents/SEALINK/GIS/SEALINK/"
cur_zonalmap <- st_read(paste0(input_GIS,
                               "Layers/Landuse/Curacao_EOP.shp")) %>%
  st_transform(crs = 4326) %>%
  mutate(landuse_zonal_map = case_when(
    EOP == 1 ~ "Unknown, Klein Curacao",
    EOP == 3 ~ "Urban areas",
    EOP == 4 ~ "Old city", 
    EOP == 5 ~ "Industry",
    EOP == 6 ~ "Airport",
    EOP == 7 ~ "Touristic areas",
    EOP == 8 ~ "Agriculture",
    EOP == 9 ~ "Conservation areas",
    EOP == 10 ~ "Park areas",
    EOP == 11 ~ "Rural areas", 
    EOP == 12 ~ "Open land",
    EOP == 13 ~ "Inland water", 
    EOP == 14 ~ "Conservation water", 
    EOP == 15 ~ "Inland island",
    EOP == 309 ~ "Undefined land use 1",
    EOP == 3012 ~ "Undefined land use 2",
    TRUE ~ "other" ))
d_zonalmap <- st_drop_geometry(cur_zonalmap)

d_zonalmap %>%
  mutate(area_km = area / 1000000) %>%
  group_by(landuse_zonal_map) %>%
  summarise(tot_area_km2 = round(sum(area_km), digits = 2)) %>%
  rbind(., c("Total", colSums(.[,2]))) %>%
  mutate(`area %` = round(as.numeric(tot_area_km2) / 440.804421*100, digits = 2))
  

pts <- st_as_sf(d_meta %>% 
                  filter(!is.na(xcoord)) %>%
                  select(samplecode, xcoord, ycoord), 
                coords = c("xcoord", "ycoord"),
                crs = 4326, agr = "constant") 

# Intersect sample points with zonal map data
# first check if polygon is valid, should return all true!
#st_is_valid(cur_zonalmap)
cur_zonalmap <- st_make_valid(cur_zonalmap)

pts_lu <- st_intersection(pts, cur_zonalmap %>% select(landuse_zonal_map)) %>%
  st_drop_geometry() 

# add landuse type to metadata file
d_meta <- d_meta %>%
  left_join(., pts_lu)

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

cat92 <- c("Fe", "Mg", "Si", "Na", "Al", "Ca", "K", "PO4", "Mn", "Ti", "Pb", 
           "Cd", "Co", "V", "Zn", "Cu", "Ni", "Cr")
an92 <- c("Cl", "SO4", "Br", "NO3", "NO2")

## data 1977 and 1992
# put in right format
d <- data1977_1992 %>%
  mutate(parameter = ifelse(parameter == "d18O", "δ18O",
                            ifelse(parameter == "dD", "δ2H", parameter))) %>%
  mutate(putcode = putcode,
         samplecode = paste(year, sample, sep = "_"),
         limit_symbol = dl,
         detection_limit = case_when(
           year == 1992 & parameter %in% cat92 ~ 0.04,
           year == 1992 & parameter %in% an92 ~ 0.1,
           year == 1977 & units == "mg/l" ~ 1,
           TRUE ~ NA_real_),
         method = case_when(
           year == 1992 & parameter %in% cat92 ~ "ICP-AES",
           year == 1992 & parameter %in% an92 ~ "IC Dionex QIC",
           TRUE ~ NA_character_),
         units = ifelse(
           parameter %in% c("c1", "c2", "c3", "c4", "clustermember", "pE", "SAR"), NA,
           ifelse(parameter %in% c("δ18O", "δ2H"), "‰", 
                  ifelse(parameter == "RSC", "meq/l", units))),
         notes = "",
         watercode = "GW",
         sampletype = "groundwater",
         subtype = "groundwater",
         xcoord = lon,
         ycoord = lat) %>%
  filter(!is.na(putcode)) %>%
  select(putcode, samplecode, year, parameter, value, limit_symbol, detection_limit,
         units, method, notes, watercode, sampletype, subtype, xcoord, ycoord)

# add data from 1992 for Pb and F which were all <dl (0.04 and 0.1 mg/L respectively)
set <- d %>%
  filter(year == 1992) %>%
  group_by(putcode, samplecode, year, notes, watercode, sampletype, subtype, xcoord, ycoord) %>%
  summarise(parameter = c("Pb", "F"),
            value = c(0.04, 0.1),
            limit_symbol = "<",
            detection_limit = c(0.04, 0.1),
            units = "mg/l",
            method = c("ICP-AES", "IC Dionex QIC")) %>%
  select(putcode, samplecode, year, parameter, value, limit_symbol, detection_limit,
         units, method, notes, watercode, sampletype, subtype, xcoord, ycoord)
d <- rbind(d, set)

## add data from 2021 Jessie ##



## add everything together
data <- rbind(data %>% mutate(putcode = NA, 
                              year = 2021,
                              xcoord = NA,
                              ycoord = NA), d) %>%
  select(putcode, samplecode, year, parameter, value, limit_symbol, detection_limit,
         units, method, notes, watercode, sampletype, subtype, xcoord, ycoord) %>%
  arrange(year, samplecode, parameter)

# quick check dl 1992
data %>%
  filter(year == 1992,
         parameter %in% c(cat92, an92)) %>%
  filter(!parameter %in% c("Cl", "Na", "Ca", "Mg", "SO4")) %>%
  ggplot(., aes(x = parameter, y = value)) +
  geom_boxplot() +
  theme_bw()
  
data %>%
  filter(parameter == "PO4",
         sampletype == "groundwater",
         value < 1000) %>%
  ggplot(., aes(x = as.character(year), y = value, group = year)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 8)) +
  theme_bw()

data %>%
  filter(parameter == "PO4",
         sampletype == "groundwater",
         value < 1000) %>%
  ggplot(., aes(x = value, group = year, fill = as.character(year))) +
  geom_histogram(binwidth = .5, alpha = 0.5, position = "identity") +
  #geom_density(alpha = 0.2) +
  theme_bw()

###############################################################################
# save final database
###############################################################################

# hydrochemistry
openxlsx::write.xlsx(data, paste0(output, "Clean_data/hydrochemistry_curacao.xlsx"))

# metadata file
openxlsx::write.xlsx(d_meta, paste0(output, "Clean_data/metadata_2021.xlsx"))


