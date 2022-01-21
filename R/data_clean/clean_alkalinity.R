#
# Input: field measurements of alkalinity titrations
#         
# Output: Cleaned alkalinity file
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
#pacman::p_load(tidyverse, dplyr, ggplot2, openxlsx, readr, ggmap, 
#               sf, tmap, tmaptools, leaflet)
pacman::p_load(tidyverse, openxlsx, ggmap, 
               sf, leaflet, data.table, cowplot, data.table)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/" 

# alkalinity data file
alk <- read.xlsx(paste0(input, "Alkalinity/Alkalinity_titration.xlsx"),
                 startRow = 10)

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################

# Clean up the alkalinity data
d <- alk %>%
  # titrations are in duplicates, select the ones with the highest accuracy
  filter(Select == 1) %>%
  # add parameter column with HCO3
  mutate(parameter = "HCO3",
         `C.[meq/l]` = as.numeric(`C.[meq/l]`),
         `C.[mg/l]` = as.numeric(`C.[mg/l]`)) %>%
  # rename columns
  rename(samplecode = Sample.code,
         "mg/l" = `C.[mg/l]`,
         "meq/l" = `C.[meq/l]`) %>%
  # place different units in long format
  pivot_longer(., cols = c(`mg/l`, `meq/l`, mmol),
               values_to = "value",
               names_to = "units") %>%
  # select only relevant columns 
  select(samplecode, parameter, value, units, Notes) 

# Check if every sample has only 1 value
check <- d %>%
  filter(units == "mg/l") %>%
  group_by(samplecode) %>%
  summarise(measurements = n_distinct(value)) %>%
  filter(measurements > 1)
if(nrow(check) > 0) {
   stop("More than 1 value for alkalinity in a sample")
}

# Check if every sample has a value for alkalinity 
#...


# 


###############################################################################
# save data
###############################################################################

write.xlsx(d, paste0(output, "Clean_data/alkalinity_clean.xlsx"))


