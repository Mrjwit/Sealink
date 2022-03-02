#
# Input: Raw output of isotopes 2H and 18O analysis
#         
# Output: Cleaned file of isotopes analysis
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 09-02-2022
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

# isotopes data file
d1 <- read.xlsx(paste0(input, "Lab/Isotopes/W-6362--W-6381_2H_BriefSummary.xlsx")) # samples 
d2 <- read.xlsx(paste0(input, "Lab/Isotopes/W-6392--W-6401_2H_BriefSummary.xlsx"))
d3 <- read.xlsx(paste0(input, "Lab/Isotopes/W-6427--W-6451_2H_BriefSummary.xlsx"))

O1 <- read.xlsx(paste0(input, "Lab/Isotopes/W-6362--W-6381_18O_BriefSummary.xlsx"))
O2 <- read.xlsx(paste0(input, "Lab/Isotopes/W-6392--W-6401_18O_BriefSummary.xlsx"))
O3 <- read.xlsx(paste0(input, "Lab/Isotopes/W-6427--W-6451_18O_BriefSummary.xlsx"))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# merge data
###############################################################################

deuterium <- rbind(d1, d2, d3)

oxygen18 <- rbind(O1, O2, O3)

###############################################################################
# edit data
###############################################################################
# checks
# there are 2 values, put them next to each other in separate columns to check  
# Deuterium
d2H <- deuterium %>%
  rename(samplecode = Sample.ID,
         parameter = Isotope,
         value = MeanDeltaOfInjs) %>%
  select(samplecode, parameter, value) %>%
  group_by(samplecode) %>%
  mutate(namen = paste0("waarde", row_number())) %>%
  ungroup() %>%
  pivot_wider(names_from = namen,
              values_from = value)

ggplot(d2H, aes(x = waarde1, y = waarde2)) +
  geom_point() +
  geom_abline(slope = 1, linetype = "dashed") +
  scale_x_continuous(name = expression(paste(delta ^{2}, "H", "(\u2030)", " value 1"))) +
  scale_y_continuous(name = expression(paste(delta ^{2}, "H", "(\u2030)", " value 2"))) +
  coord_cartesian(xlim = c(-15, -4),
                  ylim = c(-15, -4)) +
  theme_bw()

# 18O
d18O <- oxygen18 %>%
  rename(samplecode = Sample.ID,
         parameter = Isotope,
         value = MeanDeltaOfInjs) %>%
  select(samplecode, parameter, value) %>%
  group_by(samplecode) %>%
  mutate(namen = paste0("waarde", row_number())) %>%
  ungroup() %>%
  pivot_wider(names_from = namen,
              values_from = value)

ggplot(d18O, aes(x = waarde1, y = waarde2)) +
  geom_point() +
  geom_abline(slope = 1, linetype = "dashed") +
  scale_x_continuous(name = expression(paste(delta ^{18}, "O", "(\u2030)", " value 1"))) +
  scale_y_continuous(name = expression(paste(delta ^{18}, "O", "(\u2030)", " value 2"))) +
  coord_cartesian(xlim = c(-3, -0.5),
                  ylim = c(-3, -0.5)) +
  theme_bw()

# select relevant information and put in right format
d2H <- deuterium %>%
  rename(samplecode = Sample.ID,
         parameter = Isotope,
         value = MeanDeltaOfAnalyses,
         Std = StdDevOfDeltaOfInjs) %>%
  select(samplecode, parameter, value, Std) %>%
  # get average of std of 2 sets of 5 injections per sample
  group_by(samplecode) %>%
  filter(!is.na(Std)) %>%
  mutate(namen = paste0("Std", row_number())) %>%
  ungroup() %>%
  pivot_wider(names_from = namen,
              values_from = Std) %>%
  mutate(std = rowMeans(select(., 4:ncol(.)), na.rm=T) %>% round(digits = 2),
         limit_symbol = "",
         detection_limit = NA,
         units = "‰",
         method = "IA",
         notes = ifelse(is.na(value), "Value is ignored after analysis", "")) %>%
  # select relevant columns and put in right format
  select(samplecode, parameter, value, std, limit_symbol, detection_limit, units, method, notes) %>%
  unique()

d18O <- oxygen18 %>%
  rename(samplecode = Sample.ID,
         parameter = Isotope,
         value = MeanDeltaOfAnalyses,
         Std = StdDevOfDeltaOfInjs) %>%
  select(samplecode, parameter, value, Std) %>%
  # get average of std of 2 sets of 5 injections per sample
  group_by(samplecode) %>%
  filter(!is.na(Std)) %>%
  mutate(namen = paste0("Std", row_number())) %>%
  ungroup() %>%
  pivot_wider(names_from = namen,
              values_from = Std) %>%
  mutate(std = rowMeans(select(., 4:ncol(.)), na.rm=T) %>% round(digits = 2),
         limit_symbol = "",
         detection_limit = NA,
         units = "‰",
         method = "IA",
         notes = ifelse(is.na(value), "Value is ignored after analysis", "")) %>%
  # select relevant columns and put in right format
  select(samplecode, parameter, value, std, limit_symbol, detection_limit, units, method, notes) %>%
  unique()

# merge data together
d <- rbind(d2H, d18O) %>%
  arrange(samplecode)

###############################################################################
# save data
###############################################################################

write.xlsx(d, paste0(output, "Clean_data/isotopes_clean.xlsx"))





