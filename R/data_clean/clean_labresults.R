#
# Input: raw data of the lab analyses (IC, ICP, DA)
#         
# Output: Cleaned lab results
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 21-01-2022
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

# IC data files
IC <- read.xlsx(paste0(input, "Lab/IC/IC-Calculations_v2.1_20180316(+Ac)_Iris_Verstappen.xlsx"),
                sheet = "Report", startRow = 2, check.names = T) 
# NOTE: the results are copied from the 'Check results' tab to the 'Report' tab

# Diluted IC samples
IC_dil <- read.xlsx(paste0(input, "Lab/IC/IC-Calculations_v2.1_20180316(+Ac)_Iris_Verstappen_verdunning.xlsx"),
                    sheet = "Report", startRow = 2, check.names = T) 
# NOTE: the results are copied from the 'Check results' tab to the 'Report' tab

# ICP data file

# DA data file


# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################

#### IC edits and checks ####

oldnames <- names(IC)
newnames <- c("ID", "Fl", "Cl", "NO2", "Br", "NO3", "PO4", "SO4", 
              "dillution_factor", "ID2", "Fl_round", "Cl_round", "NO2_round", 
              "Br_round", "NO3_round", "PO4_round", "SO4_round")

d <- IC %>%
  # rename double column names
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames) %>%
  # remove rows with calibration standards and blank samples
  filter(!str_detect(ID, c("Estim", "St", "Blanco", "QS STD anion")))
  # add units and detection limit column
  mutate(units = "mg/l",
         dl = "") %>%
  # place parameters in long format
  pivot_longer(., cols = c(...),
               values_to = "value",
               names_to = "parameter") %>%
  # select only relevant columns
  select(samplecode, parameter, dl, value, units)

  
## Perform quality control checks -> Maybe seperate QC script better ##

# EC measured vs EC calculated

# 


if(nrow(check) > 0) {
  stop("More than 1 value for .. in a sample")
}


#### DA edits and checks ####

# Alkalinity field titration vs lab

# NO3 field vs lab

# 


#### ICP edits and checks ####



# Check IB <10%

# Check redox conditions

# 




###############################################################################
# save data
###############################################################################

write.xlsx(d, paste0(output, "Clean_data/lab_data.xlsx"))


