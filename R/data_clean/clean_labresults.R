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
pacman::p_load(tidyverse, openxlsx, ggmap, skimr,
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
# ICP <- read.xlsx(paste0(input, ".xlsx"),
#                  sheet = "")

# DA data file
DA <- read.xlsx(paste0(input, "Lab/DA/PO4 & NH4 Iris.xlsx"),
                sheet = "Summary")
DA_alk <- read.xlsx(paste0(input, "Lab/DA/Alkalinity Iris.xlsx"),
                    sheet = "Summary")

# Table to link labcodes of different analysis to samplecodes
lab_table <- read.xlsx("C:/Users/mikewit/Documents/SEALINK/Documents/Lab/vertaaltabel_labmonsters.xlsx",
                       sheet = "TOTAAL")

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################

#### IC edits and checks ####

oldnames <- names(IC)
newnames <- c("ID", "Fl", "Cl", "NO2", "Br", "NO3", "PO4", "SO4", 
              "dilution_factor", "ID2", "Fl_round", "Cl_round", "NO2_round", 
              "Br_round", "NO3_round", "PO4_round", "SO4_round")
remove.list <- paste(c("Estim", "St", "Blanco", "QS STD anion"), collapse = '|')

d_IC <- IC %>%
  # rename double column names
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames) %>%
  # remove dillution_factor and ID2 column, which are redundant in this case 
  select(-c(dilution_factor, ID2)) %>%
  # remove rows with calibration standards and blank samples
  filter(!str_detect(ID, remove.list)) %>%
  # add samplecode to lab numbers 
  left_join(., lab_table %>% filter(analysis == "IC") %>% select(-analysis),
            by = c("ID" = "labcode")) %>%
  # place parameters in long format
  pivot_longer(., cols = Fl:SO4_round,
               values_to = "value",
               names_to = "parameter") %>%
  # add column indicating if values are rounded or not
  # change parameter names so that rounded and unrounded have the same name
  mutate(rounded = ifelse(str_detect(parameter, "round"),
                          "value_round", "value_unrounded"),
         parameter = gsub("_round", "", parameter)) %>%
  # add rounded value to separate column to compare values
  pivot_wider(., 
              names_from = rounded,
              values_from = value) %>%
  # add limit symbol, detection limit values and units
  mutate(limit_symbol = ifelse(str_detect(value_round, "<"), "<", 
                               ifelse(str_detect(value_round, ">"), ">", "")),
         detection_limit = ifelse(str_detect(value_round, "<"),
                                  as.numeric(gsub("<", "", value_round)), NA),
         units = "mg/l") %>%
### OPTION: which values to be used? rounded (with detection limits) or unrounded?
  # use values >detection limit from 'value' and detection limits when <detection limit
  mutate(value = as.numeric(ifelse(limit_symbol == "<",
                                   detection_limit, value_unrounded)) %>%
           round(., digits = 3)) %>%
  # select only relevant columns
  select(samplecode, parameter, value, limit_symbol, detection_limit, units)

## make wide format
# d_IC_wide <- d_IC %>%
#   mutate(parameter = paste(parameter, units)) %>%
#   select(-c(detection_limit, units)) %>%
#   pivot_wider(.,
#               names_from = parameter,
#               values_from = c(value, limit_symbol),
#               names_glue = "{parameter}_{.value})") %>%
#   select(1, 9, 2, 10, 3, 11, 4, 12, 5, 13, 6, 14, 7, 15, 8)

  
## Add results from diluted samples
d_IC_dil <- IC_dil %>%
  # rename double column names
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames) %>%
  # ID2 column, which is redundant
  select(- ID2) %>%
  # remove rows with calibration standards and blank samples
  filter(!str_detect(ID, remove.list)) %>%
  # add samplecode to lab numbers 
  left_join(., lab_table %>% filter(analysis == "IC_dilution") %>% select(-analysis),
            by = c("ID" = "labcode")) %>%
  # place parameters in long format
  pivot_longer(., cols = c(Fl:SO4_round, -dilution_factor),
               values_to = "value",
               names_to = "parameter") %>%
  # add column indicating if values are rounded or not
  # change parameter names so that rounded and unrounded have the same name
  mutate(rounded = ifelse(str_detect(parameter, "round"),
                          "value_round", "value_unrounded"),
         parameter = gsub("_round", "", parameter)) %>%
  # add rounded value to separate column to compare values
  pivot_wider(., 
              names_from = rounded,
              values_from = value) %>%
  # add limit symbol, detection limit values and units
  mutate(limit_symbol = ifelse(str_detect(value_round, "<"),
                               "<", ""),
         detection_limit = ifelse(str_detect(value_round, "<"),
                                  as.numeric(gsub("<", "", value_round)), NA),
         units = "mg/l") %>%
  ### OPTION: which values to be used? rounded (with detection limits) or unrounded?
  # use values >detection limit from 'value' and detection limits when <detection limit
  mutate(value = as.numeric(ifelse(limit_symbol == "<",
                                   detection_limit, value_unrounded)) %>%
           round(., digits = 3)) %>%
  # correct the values using the dilution factors only for values >dl
  mutate(value = ifelse(limit_symbol != "<", value * dilution_factor, value)) %>%
  # select only relevant columns
  select(samplecode, parameter, value, limit_symbol, detection_limit, units)

## Check for differences between diluted samples and regular samples
d <- d_IC %>%
  left_join(., d_IC_dil %>% 
              rename(value_dil = value,
                     limit_symbol_dil = limit_symbol) %>% 
              select(samplecode, parameter, limit_symbol_dil, value_dil)) %>%
  filter(!is.na(value_dil)) %>%
  # percentage deviation between diluted and undiluted sample
  mutate(diff = round((value / value_dil), digits = 4)) %>%
  # determine rules for getting the right values
  mutate(waarde = case_when(
    # select the lowest detection limits
    #value < value_dil & limit_symbol == "<" ~ value,
    limit_symbol == "<" & limit_symbol_dil == "<" & value < value_dil | value == value_dil ~ value,
    limit_symbol == "<" & limit_symbol_dil == "<" & value > value_dil ~ value_dil,
    # for values >dl of Cl and SO4 in undiluted samples get diluted values
    limit_symbol == ">" & limit_symbol_dil != ">" ~ value_dil,
    # Br seems to not be measured correctly in undiluted samples -> <dl, 
    # whereas in diluted sample Br is >dl
    limit_symbol == "<" & limit_symbol_dil != "<" ~ value_dil,
    limit_symbol_dil == "<" & limit_symbol != "<" ~ value,
    # if both the undiluted and diluted sample measure >dl, then pick the undiluted sample
    limit_symbol == "" & limit_symbol_dil == "" ~ value,
    TRUE ~ NA_real_ )) %>%
  view()

## Perform quality control checks -> Maybe separate QC script better ##

# EC measured vs EC calculated

# 


# if(nrow(check) > 0) {
#   stop("More than 1 value for .. in a sample")
# }


#### DA edits and checks ####

d_DA <- DA %>%
  # select relevant columns
  select(Sample.ID, Results, Units) %>%
  # add samplecode to lab numbers 
  left_join(., lab_table %>% filter(analysis == "DA") %>% select(-analysis),
            by = c("Sample.ID" = "labcode")) %>%
  # remove rows with calibration standards and blanc samples
  filter(!is.na(samplecode)) %>%
  # add parameter column
  mutate(parameter = ifelse(str_detect(Units, "P"),
                            "PO4", "NH4"),
         Units.2 = "mg/l") %>%
  # Adjust units? Convert mg/l P to mg/l PO4 and mg/l N to mg/l NH4
  mutate(Results.2 = ifelse(str_detect(Units, "P"),
                            Results / (30.973762 / 94.9714),
                            Results / (14.0067 / 18.039))) 
  # place different units and corresponding values in long format
d <- rbind(d_DA %>% select(samplecode, parameter, Results, Units),
           d_DA %>% select(samplecode, parameter, Results.2, Units.2) %>% rename(Results = Results.2,
                                                                           Units = Units.2)) %>%
  rename(value = Results,
         units = Units) %>%
  arrange(samplecode, parameter) %>%
  # assume negative values to be <dl
  mutate(limit_symbol = ifelse(value < 0,
                               "<", ""),
         detection_limit = case_when(
                             parameter == "NH4" & units == "mg N/L " ~ 0.04,
                             parameter == "NH4" & units == "mg/l" ~ 0.04 / (14.0067 / 18.039),
                             parameter == "PO4" & units == "mg P/L " ~ 0.013,
                             parameter == "PO4" & units == "mg/l" ~ 0.013 / (30.973762 / 94.9714),
                             TRUE ~ NA_real_) %>% round(digits = 4)) 

###
### Change negative values and values < dl ???
### 




# # checks
# d %>%
#   group_by(parameter, units) %>%
#   summarise(mean = mean(value),
#             sd = sd(value),
#             min = min(value),
#             p25 = quantile(value, 0.25),
#             p50 = quantile(value, 0.50),
#             p75 = quantile(value, 0.75),
#             max = max(value)) %>%
#   view()


  # reorder columns
d_DA <- d %>%
  select(samplecode, parameter, value, limit_symbol, detection_limit, units)

# Clean up DA alkalinity set
d_ALK <- DA_alk %>%
  # select relevant columns
  select(Sample.ID, Results, Units) %>%
  # add samplecode to lab numbers 
  left_join(., lab_table %>% filter(analysis == "DA_alk") %>% select(-analysis),
            by = c("Sample.ID" = "labcode")) %>%
  # remove rows with calibration standards and blanks 
  filter(!is.na(samplecode)) %>%
  # add parameter column
  mutate(parameter = "HCO3_lab",
         units = "mg/l",
         value = Results,
         limit_symbol = "",
         detection_limit = NA) %>%
  select(samplecode, parameter, value, limit_symbol, detection_limit, units)
  

#### ICP edits and checks ####



# Check IB <10%

# Check redox conditions (O2, NO3, Fe, Mn, NH4, SO4)

# 


# Combine all labresults
d <- rbind(d_IC, d_DA, d_ALK, d_ICP)

###############################################################################
# save data
###############################################################################

write.xlsx(d, paste0(output, "Clean_data/lab_data.xlsx"))
#write.xlsx(d_wide, paste0(output, "Clean_data/lab_data_wide.xlsx"))


