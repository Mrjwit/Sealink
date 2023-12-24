#
# Input: hydrochemical dataset of Curacao with metadata file
#         
# Output: Piper plots
# 
# Dependencies: none
#
# source material: https://uc-r.github.io/hc_clustering 
# https://github.com/markolipka?tab=repositories 
# https://gist.github.com/johnDorian/5561272
# Author: Mike Wit
# Date: 15-07-2022
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx)

devtools::source_url("https://github.com/markolipka/ggplot_Piper/blob/master/ggplot_Piper.R?raw=TRUE")
#devtools::install_github("Mrjwit/HydroChemistry")

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/final_merged/" 

# load cleaned data
data <- openxlsx::read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx"))
meta <- openxlsx::read.xlsx(paste0(input, "metadata_2021_2022.xlsx"))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/Output/" 

###############################################################################
# Editing data
###############################################################################

# add metadata
# add geology and land use for 2021-2022
meta <- rbind(metadata %>% select(samplecode, geology, geology_abr, 
                                  landuse_zonal_map, Land.use.based.on.own.observations),
              meta_2020 %>% select(samplecode, geology, geology_abr) %>%
                mutate(landuse_zonal_map = "",
                       Land.use.based.on.own.observations = ""))

data <- data %>%
  left_join(., meta)

#### functions for piperplot ####
# functions for piper diagram https://rstudio-pubs-static.s3.amazonaws.com/542159_ae160ba405044883a58ba3a53e4f7e6d.html
# takes as input a dataframe with cation and anion concentrations in meql and converts them to percentages


# select relevant parameters and convert to meq/l in wide format
an <- c("Cl", "HCO3", "NO3", "SO4")
cat <- c("Na", "Ca", "Mg", "K")

d <- data %>%
  filter(year %in% c(2021, 2022), 
         parameter %in% c(an, cat, "EC")) %>%
  filter(!str_detect(method, "PHOS"),
         units != "mg N/L") %>%                    
  # change mg/l to meq/l for anions
  mutate(meql = case_when(
    parameter == "Cl" ~ value / 35.453,
    parameter == "HCO3" ~ value / 61.0168,
    parameter == "NO3" & limit_symbol != "<" ~ value / 62.0049,
    parameter == "SO4" ~ value / 96.06 * 2,
    parameter == "EC" ~ value,
    TRUE ~ NA_real_ )) %>%
  mutate(meql = case_when(
    parameter == "Na" ~ value / 22.989769,
    parameter == "Ca" ~ value / 40.078 * 2,
    parameter == "Mg" ~ value / 24.305 * 2,
    parameter == "K" ~ value / 39.0983,
    TRUE ~ meql )) %>%
  select(samplecode, geology, parameter, meql, sampletype) %>%
  pivot_wider(names_from = parameter,
              values_from = meql) %>%
  filter(!is.na(Cl)) %>% 
  replace(is.na(.), 0) %>%
  select(samplecode, sampletype, geology, EC, Cl, HCO3, SO4, NO3, Na, Ca, Mg, K)

HydRochemistRy::PiperPlot(d)

# calculate percentages
percents <- toPercent(d)

# should add up to 100%
cation.sums <- apply(percents[, c("Na", "Ca", "Mg", "K")], 1, FUN = sum)
anion.sums <- apply(percents[, c("Cl", "HCO3", "SO4", "NO3")], 1, FUN = sum)

cation.sums
anion.sums

# Transformation
piper_data <- transform_piper_data(Ca = percents$Ca,
                                   Mg = percents$Mg,
                                   Cl = percents$Cl,
                                   SO4 = percents$SO4,
                                   name = percents$samplecode)
d <- toPercent(d)

piper_data <- transform_piper_data(d)

piper_data <- merge(piper_data,
                    percents[, c("samplecode", "sampletype", "geology", "EC")],
                    by.y = "samplecode",
                    by.x = "observation")

ggplot_piper(output = "ggplot") +
  geom_point(data = piper_data, aes(x, y))

## Piper diagram plots
# Default plot
ggplot_piper() +
  geom_point(data = piper_data, aes(x, y))

# per watertype 
ggplot_piper() +
  geom_point(data = piper_data, aes(x, y, colour = factor(sampletype),
                                    shape = sampletype)) +
  scale_colour_manual(name = "sample type",
                      values = c("#F8766D", "#00BFC4", "#00BA38", "#B79F00", "#619CFF", "#C77CFF", "grey")) +
  scale_shape_manual(name = "sample type",
                     values = c(19, 17, 15, 3, 7, 8, 9))

# per EC
ggplot_piper() +
  geom_point(data = piper_data, aes(x, y, size = EC, colour = EC),
             alpha = 0.7) +
  scale_colour_distiller(name = expression("EC"~(mu*S/cm)),
                         breaks = c(1000, 5000, 10000, 20000, 50000),
                         palette = "Spectral",
                         direction = -1) +
  scale_size_continuous(name = expression("EC"~(mu*S/cm)),
                        breaks = c(1000, 5000, 10000, 20000, 50000)) +
  guides(color = guide_legend(), size = guide_legend())

# combination
ggplot_piper() +
  geom_point(data = piper_data, aes(x, y, colour = factor(sampletype),
                                    size = EC),
             alpha = 0.7)

# only groundwater with EC
ggplot_piper() +
  geom_point(data = piper_data %>% filter(sampletype == "groundwater"), 
             aes(x, y, size = EC, colour = EC),
             alpha = 0.7) +
  scale_colour_distiller(name = expression("EC"~(mu*S/cm)),
                         breaks = c(1000, 5000, 10000, 20000, 50000),
                         palette = "Spectral",
                         direction = -1) +
  scale_size_continuous(name = expression("EC"~(mu*S/cm)),
                        breaks = c(1000, 5000, 10000, 20000, 50000)) +
  guides(color = guide_legend(), size = guide_legend())

# only groundwater per geology
ggplot_piper() +
  geom_point(data = piper_data %>% filter(sampletype == "groundwater"),
             aes(x, y, colour = geology)) +
  scale_colour_manual(name = "geology",
                      values = c("yellowgreen", "seagreen4", "lightskyblue2", "purple4",
                                 "coral1", "yellow2", "grey50"))


# only groundwater per geology and EC
ggplot_piper() +
  geom_point(data = piper_data %>% filter(sampletype == "groundwater"),
             aes(x, y, colour = geology, size = EC),
             alpha = 0.7) +
  scale_colour_manual(name = "geology",
                      values = c("yellowgreen", "seagreen4", "lightskyblue2", "purple4",
                                 "coral1", "yellow2", "grey50")) +
  scale_size_continuous(name = expression("EC"~(mu*S/cm)),
                        breaks = c(1000, 2500, 5000, 10000, 25000, 50000)) +
  guides(color = guide_legend(), size = guide_legend())



