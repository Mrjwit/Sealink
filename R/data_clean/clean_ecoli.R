#
# Input: field measurements of E.coli Petrifilms
#         
# Output: Cleaned E.coli file
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
ecoli <- read.xlsx(paste0(input, "E.coli/E.coli.xlsx"),
                   startRow = 2)

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################

# Clean up the E.coli data
d <- ecoli %>%
  # some plates were incubated with dilutions, select the ones with the highest accuracy
  filter(Select == 1) %>%
  # add parameter column with E.coli
  mutate(parameter = "E.coli",
         units = "CFU/100 ml") %>%
  # rename columns
  rename(samplecode = Sample.nr,
         value = E..coli,
         notes = Notes) %>%
  # select only relevant columns
  select(samplecode, parameter, value, units, notes)

# check duplo values
duplos <- d %>%
  filter(str_detect(notes, "duplo")) %>%
  view()

d_set <- d %>%
  # select samples with duplo measurements
  filter(samplecode %in% duplos$samplecode) %>%
  # add column with duplos
  mutate(duplo = ifelse(!str_detect(notes, "duplo"),
                        "origineel", "duplo")) %>%
  mutate(duplo = ifelse(is.na(notes), "origineel", duplo)) %>%
  select(-notes) %>%
  pivot_wider(values_from = value,
              names_from = duplo) %>%
  # add average value from duplo's
  mutate(value = (origineel + duplo) / 2,
         notes = "value averaged from duplos")

ggplot(d_set, aes(x = origineel, y = duplo)) +
  geom_point() +
  scale_x_log10() + 
  scale_y_log10() +
  geom_abline(intercept = 0) + 
  theme_bw()

# add average duplo values to final file
d <- d %>%
  filter(!samplecode %in% duplos$samplecode) %>%
  rbind(., d_set %>% select(samplecode, parameter, value, units, notes)) %>%
  # add columns that are present in other dataset to merge later
  mutate(limit_symbol = "",
         detection_limit = NA,
         method = "Petrifilm plate") %>%
  select(samplecode, parameter, value, limit_symbol, detection_limit, units, method, notes) %>%
  arrange(samplecode)
  
# Check if every sample has only 1 value
check <- d %>%
  group_by(samplecode) %>%
  summarise(measurements = n_distinct(value)) %>%
  filter(measurements > 1)
if(nrow(check) > 0) {
  stop("More than 1 value for E.coli in a sample")
}

# Check if every sample has a value for E.coli 



###############################################################################
# save data
###############################################################################

write.xlsx(d, paste0(output, "Clean_data/ecoli_clean.xlsx"))


