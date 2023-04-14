#
# Input: Hand made DA results of multiple runs        
#
# Output: Cleaned lab results merged together in long and wide format
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 28-03-2023
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

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/" 

DA <- openxlsx::read.xlsx(paste0(input, "Lab/DA/Second_fieldwork/DA_analysis_second_fieldwork_Oct22_Jan23.xlsx"),
                          sheet = "Final", detectDates = T)

output <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/" 

###############################################################################
# edit data
###############################################################################

d <- DA %>%
  rename(value_NH4 = NH4,
         value_PO4 = PO4,
         method_NH4 = method.NH4,
         method_PO4 = method.PO4,
         analysisdate_NH4 = analysisdate.NH4,
         analysisdate_PO4 = analysisdate.PO4) %>%
  #select(samplecode:analysisdate_PO4) %>%
  # put data in long format with parameter, methods and notes column
  pivot_longer(-c(samplecode, status),
               names_to = c(".value", "parameter"),
               names_sep = "_") %>%
  # change concentrations from NH4-N and PO4-P to mg/L
  mutate(value = case_when(
    parameter == "NH4" ~ value / (14.0067 / 18.039),
    parameter == "PO4" ~ value / (30.973762 / 94.9714),
    TRUE ~ NA_real_)) %>%
  # change names of methods
  mutate(method = case_when(
    method == "NH4-1" ~ "NH4 1",
    method == "NH4-10" ~ "NH4 10",
    method == "PO4-0.1" ~ "o-PHOS 0.1",
    method == "PO4-1" ~ "o-PHOS 1",
    method == "PO4-5" ~ "o-PHOS 5",
    method == "PO4-10" ~ "o-PHOS 10",
    method == "PO4-20" ~ "o-PHOS 20",
    TRUE ~ "" )) %>%
  # add detection limit in mg/L
  mutate(detection_limit = case_when(
    method == "NH4 1" ~ 0.041 / (14.0067 / 18.039), # based on duplicate test with 0.2 mg N/l standard, should be done with 0.1 mg N/l!
    method == "NH4 10" ~ 0.2 / (14.0067 / 18.039), # should be 0.2 or 0.04?
    method == "o-PHOS 0.1" ~ 0.01 / (30.973762 / 94.9714), # this is based on lower range, should be based on duplicate test
    method == "o-PHOS 1" ~ 0.027 / (30.973762 / 94.9714), # based on duplicate test with 0.1 mg P/l standard
    method == "o-PHOS 5" ~ 0.5 / (30.973762 / 94.9714), # this is based on lower range, should be based on duplicate test 
    method == "o-PHOS 10" ~ 1.2 / (30.973762 / 94.9714), # this is based on lower range, should be based on duplicate test 
    method == "o-PHOS 20" ~ 2.0 / (30.973762 / 94.9714), # this is based on lower range, should be based on duplicate test 
  )) %>% 
  # add limit symbol, sd, units
  mutate(limit_symbol = ifelse(value < detection_limit, "<", ""), 
         sd = NA,
         units = "mg/l",
         method = paste("DA", method),
         notes = status) %>%
  # change values < dl to dl
  mutate(value = ifelse(value <= detection_limit, detection_limit, value)) %>%
    select(samplecode, parameter, value, sd, limit_symbol, detection_limit, units, method, notes)  

## make dataset in wide format
d_wide <- d %>%
  mutate(parameter = paste(parameter, units)) %>%
  mutate(value = paste0(limit_symbol, value)) %>%
  select(-c(detection_limit, limit_symbol, sd, units, method, notes)) %>%
  pivot_wider(., 
              names_from = parameter, 
              values_from = value)

# 
# if(nrow(check) > 0) {
#  stop("More than 1 value for .. in a sample")
# }

###############################################################################
# save data
###############################################################################

openxlsx::write.xlsx(d, paste0(output, "/Second_fieldwork/DA_Oct-Jan_2023.xlsx"))
openxlsx::write.xlsx(d_wide, paste0(output, "/Second_fieldwork/DA_Oct-Jan_2023_wide.xlsx"))





