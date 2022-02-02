#
# Input: field measurements for Radon
#         
# Output: Cleaned dataset for Radon
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 12-01-2022
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

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/" 

# alkalinity data file
radon <- read.xlsx(paste0(input, "Radon/Radon_measurements.xlsx"),
                   startRow = 1)

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################

# Clean up the radon data
d <- radon %>%
  # add parameter column with Radon
  mutate(parameter = "Rd",
         units = "Bq/l",
         notes = ifelse(is.na(Notes),
                        paste("Sampling method:", Sampling.method),
                        paste(Notes, "Sampling method:", Sampling.method)),
         limit_symbol = "",
         detection_limit = NA,
         method = "RAD7 Big Bottle System") %>%
  # rename columns
  rename(value = `Rn.[Bq/L]`) %>%
  # select only relevant columns
  select(samplecode, parameter, value, limit_symbol, detection_limit, units, method, notes)

# some samples have 2 radon measurements, drop the lowest value
duplos <- d %>%
  group_by(samplecode) %>%
  mutate(samples = n_distinct(value),
         id = row_number()) %>%
  ungroup() %>%
  filter(samples > 1) %>%
  pivot_wider(names_from = id,
              values_from = value) %>%
  mutate(value = ifelse(`1` > `2`, `1`, `2`)) 
  #select(samplecode, parameter, value, units, Notes) 

ggplot(duplos, aes(x = `1`, y = `2`)) +
  geom_point() +
  #scale_x_log10() + 
  #scale_y_log10() +
  geom_abline(intercept = 0) + 
  theme_bw()

# add highest values from double measurements to dataset
d_set <- d %>%
  # remove samples with double measurements
  filter(!samplecode %in% duplos$samplecode) %>%
  # add records from duplos with highest values to dataset
  rbind(., duplos %>% select(samplecode, parameter, value, limit_symbol, detection_limit, units, method, notes))

# Check if every sample has only 1 value
check <- d_set %>%
  group_by(samplecode) %>%
  summarise(measurements = n_distinct(value)) %>%
  filter(measurements > 1)
if(nrow(check) > 0) {
  stop("More than 1 value for Radon in a sample")
}

# Check if every sample has a value for Radon 



###############################################################################
# save data
###############################################################################

write.xlsx(d_set, paste0(output, "Clean_data/radon_clean.xlsx"))


