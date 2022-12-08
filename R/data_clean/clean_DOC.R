#
# Input: DOC analysis from UvA
#         
# Output: Cleaned DOC file
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 01-11-2022
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

# alkalinity data file
DOC1 <- read.xlsx(paste0(input, "lab/DOC_UvA/2022_DOC_total.xlsx"),
                  sheet = "Run1")
DOC2 <- read.xlsx(paste0(input, "lab/DOC_UvA/2022_DOC_total.xlsx"),
                  sheet = "Run2")

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################

# Select columns and combine datasets
d <- rbind(DOC1, DOC2) %>%
  rename(samplecode = Sample.ID)

# Calculate average value and SD
d <- d %>% 
  group_by(samplecode) %>%
  mutate(avg = mean(Conc.),
         sd = sd(Conc.)) %>%
  ungroup() %>%
  # only unique rows
  filter(!is.na(AVG)) %>%
  # add sampletype
  mutate(sampletype = substr(samplecode, start = 1, stop = 2))

# plot gw
ggplot(d %>% filter(sampletype == "GW"), 
       aes(x = samplecode, y = avg)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd),
                width = 0.8) +
  theme_bw()

# plot all
ggplot(d, aes(x = samplecode, y = avg)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd),
                width = 0.2) +
  theme_bw() +
  facet_wrap(facets = 'sampletype', scales = "free")

# put into format of other datasets


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
         "meq/l" = `C.[meq/l]`,
         notes = Notes) %>%
  # place different units in long format
  pivot_longer(., cols = c(`mg/l`, `meq/l`, mmol),
               values_to = "value",
               names_to = "units") %>%
  # add limit symbol, detection limit column and method
  mutate(limit_symbol = "",
         detection_limit = NA,
         method = "Field titration") %>%
  # select only relevant columns 
  select(samplecode, parameter, value, limit_symbol, detection_limit, units, method, notes) 

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


