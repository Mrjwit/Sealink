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
pacman::p_load(tidyverse, openxlsx, hydrogeo)

devtools::source_url("https://github.com/markolipka/ggplot_Piper/blob/master/ggplot_Piper.R?raw=TRUE")

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/final_merged/" 

# load cleaned data
data <- openxlsx::read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx"))
meta <- openxlsx::read.xlsx(paste0(input, "metadata_2021_2022.xlsx"))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/Output/Statistics/HCA" 

###############################################################################
# Editing data
###############################################################################

# add geology column to dataset
data <- data %>%
  left_join(., meta %>% select(samplecode, geology))

# select relevant parameters and convert to meq/l in wide format
an <- c("Cl", "HCO3", "NO3", "SO4")
cat <- c("Na", "Ca", "Mg", "K")

d <- data %>%
  filter(year == 2021, 
         parameter %in% c(an, cat, "EC_uS")) %>%
  filter(!str_detect(method, "PHOS"),
         units != "mg N/L") %>%                    
  # change mg/l to meq/l for anions
  mutate(meql = case_when(
    parameter == "Cl" ~ value / 35.453,
    parameter == "HCO3" ~ value / 61.0168,
    parameter == "NO3" & limit_symbol != "<" ~ value / 62.0049,
    parameter == "SO4" ~ value / 96.06 * 2,
    parameter == "EC_uS" ~ value,
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
  filter(!is.na(Cl)) 

percents <- toPercent(d)

# functions for piper diagram https://rstudio-pubs-static.s3.amazonaws.com/542159_ae160ba405044883a58ba3a53e4f7e6d.html
# takes as input a dataframe with cation and anion concentrations in meql and converts them to percentages
toPercent <- function (d) {
  totalCations <- d$Ca + d$Mg + d$Na + d$K
  d$Ca <- 100 * (d$Ca/totalCations)
  d$Mg <- 100 * (d$Mg/totalCations)
  d$Na <- 100 * (d$Na/totalCations)
  d$K <- 100 * (d$K/totalCations)
  totalAnions <- d$Cl + d$SO4 + d$HCO3
  d$Cl <- 100 * (d$Cl/totalAnions)
  d$SO4 <- 100 * (d$SO4/totalAnions)
  #d$CO3 <- 100 * (d$CO3/totalAnions)
  d$HCO3 <- 100 * (d$HCO3/totalAnions)
  return(d)
}


# calculate percentages



# 

