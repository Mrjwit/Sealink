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
pacman::p_load(tidyverse, openxlsx)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/" 

# alkalinity data file 1st fieldwork 2021-2022
alk <- read.xlsx(paste0(input, "Alkalinity/Alkalinity_titration.xlsx"),
                 sheet = "Field titration", startRow = 10)
# alkalinity data file 2nd fieldwork 2022-2023
alk2 <- read.xlsx(paste0(input, "Alkalinity/Alkalinity_titration.xlsx"),
                  sheet = "Field titration 2nd", startRow = 10) %>%
  select(1:3, 5:10, 13:15) 

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################

# Clean up the alkalinity data first fieldwork
d <- alk %>%
  # titrations are in duplicates, select the ones with the highest accuracy
  filter(Select == 1) %>%
  # add parameter column with HCO3
  mutate(parameter = "HCO3",
         `C.[meq/l]` = as.numeric(`C.[meq/l]`),
         `C.[mg/l]` = as.numeric(`C.[mg/l]`),
         `mmol/l` = as.numeric(`mmol/L`)) %>%
  # rename columns
  rename("mg/l" = `C.[mg/l]`,
         "meq/l" = `C.[meq/l]`) %>%
  # place different units in long format
  pivot_longer(., cols = c(`mg/l`, `meq/l`, `mmol/l`),
               values_to = "value",
               names_to = "units") %>%
  # add limit symbol, detection limit column and method
  mutate(limit_symbol = "",
         detection_limit = NA,
         sd = NA,
         method = "Field titration") %>%
  # select only relevant columns 
  select(samplecode, parameter, value, sd, limit_symbol, detection_limit, units, method, notes) 

# Clean up the alkalinity data second fieldwork
d2 <- alk2 %>%
  # add calculated HCO3 concentration for RW008 using measured pH = 6.24
  mutate(`C.[mg/l]` = case_when(
    samplecode == "RW008" ~ 10^(-11.24+6.24)*1000,
    TRUE ~ `C.[mg/l]`),
         notes = case_when(
    samplecode == "RW008" ~ paste(notes, "value is calculated using pH from field", sep = "; "),
    TRUE ~ notes)) %>%
  # remove rows that are no samples and remove 0 values
  filter(!samplecode %in% c("test blanc"),
         !is.na(samplecode),
         `C.[mg/l]` != 0) %>%
  # select only relevant columns for now
  select(samplecode, `C.[mg/l]`, notes) %>%
  # titrations are in duplicates, take the average and add notes together
  group_by(samplecode) %>%
  mutate(avg = mean(`C.[mg/l]`),
         notes = ifelse(length(unique(notes)) > 1, 
                        paste(notes, collapse = "; "), notes)) %>%
  ungroup() %>%
  # select only 1 row per sample
  select(-`C.[mg/l]`) %>%
  distinct() %>%
  rename(`mg/l` = avg) %>%
  # add mmol
  mutate(`mmol/l` = `mg/l` / 61.02) %>%
  # place different units in long format
  pivot_longer(., cols = c(`mg/l`, `mmol/l`),
               values_to = "value",
               names_to = "units") %>%
  # add parameter, limit symbol, detection limit, sd and method
  mutate(parameter = "HCO3",
         limit_symbol = "",
         detection_limit = NA,
         sd = NA,
         method = "Field titration") %>%
  # select only relevant columns 
  select(samplecode, parameter, value, sd, limit_symbol, detection_limit, units, method, notes) 

# merge two datasets together
dat <- rbind(d, d2)

# Check if every sample has only 1 value
check <- dat %>%
  filter(units == "mg/l") %>%
  group_by(samplecode) %>%
  summarise(measurements = n_distinct(value)) %>%
  filter(measurements > 1)

if(nrow(check) > 0) {
   stop("More than 1 value for alkalinity in a sample")
}

# Check if every sample has a value for alkalinity 


# 


###############################################################################
# save data
###############################################################################

write.xlsx(dat, paste0(output, "Clean_data/alkalinity_clean.xlsx"))


