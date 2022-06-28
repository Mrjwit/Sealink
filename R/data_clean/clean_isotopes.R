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
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/Lab/Isotopes/" 

# isotopes Summary data files
d1 <- read.xlsx(paste0(input, "W-6362--W-6381_2H_BriefSummary.xlsx")) # samples 
d2 <- read.xlsx(paste0(input, "W-6392--W-6401_2H_BriefSummary.xlsx"))
d3 <- read.xlsx(paste0(input, "W-6427--W-6451_2H_BriefSummary.xlsx"))
d4 <- 

O1 <- read.xlsx(paste0(input, "W-6362--W-6381_18O_BriefSummary.xlsx"))
O2 <- read.xlsx(paste0(input, "W-6392--W-6401_18O_BriefSummary.xlsx"))
O3 <- read.xlsx(paste0(input, "W-6427--W-6451_18O_BriefSummary.xlsx"))
O4 <- read.xlsx(paste0(input, "W-6572--W-6581_18O_BriefSummary.xlsx"))

# isotopes raw data files
dr1 <- read.xlsx(paste0(input, "W-6362--W-6381_2H.xlsx")) # samples 
dr2 <- read.xlsx(paste0(input, "W-6392--W-6401_2H.xlsx"))
dr3 <- read.xlsx(paste0(input, "W-6427--W-6451_2H.xlsx"))
dr4 <- read.xlsx(paste0(input, "W-6572--W-6581_2H.xlsx"))

Or1 <- read.xlsx(paste0(input, "W-6362--W-6381_18O.xlsx"))
Or2 <- read.xlsx(paste0(input, "W-6392--W-6401_18O.xlsx"))
Or3 <- read.xlsx(paste0(input, "W-6427--W-6451_18O.xlsx"))
Or4 <- read.xlsx(paste0(input, "W-6572--W-6581_18O.xlsx"))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# merge data
###############################################################################

# using summary data
deuterium <- rbind(d1, d2, d3) %>% 
  select_if(., function(x){any(!is.na(x))})
oxygen18 <- rbind(O1, O2, O3) %>%
  select_if(., function(x){any(!is.na(x))})

# using raw data
deuterium_raw <- rbind(dr1, dr2, dr3, dr4) %>%
  select_if(., function(x){any(!is.na(x))})
oxygen18_raw <- rbind(Or1, Or2, Or3, Or4) %>%
  select_if(., function(x){any(!is.na(x))})

###############################################################################
# edit data
###############################################################################
#### using summary data ####
# checks
# there are 2 values, put them next to each other in separate columns to check  
# Deuterium
d2H <- deuterium %>%
  rename(samplecode = Sample.ID,
         parameter = Isotope,
         value = MeanDeltaOfInjs) %>%
  select(samplecode, parameter, value) %>%
  group_by(samplecode) %>%
  filter(!is.na(value)) %>%
  mutate(namen = paste0("waarde", row_number())) %>%
  ungroup() %>%
  pivot_wider(names_from = namen,
              values_from = value)

ggplot(d2H, aes(x = waarde1, y = waarde2)) +
  geom_point() +
  geom_abline(slope = 1, linetype = "dashed") +
  geom_smooth(method = 'lm', formula = y~x) +
  scale_x_continuous(name = expression(paste(delta ^{2}, "H", "(\u2030)", " value 1"))) +
  scale_y_continuous(name = expression(paste(delta ^{2}, "H", "(\u2030)", " value 2"))) +
  # coord_cartesian(xlim = c(-15, -4),
  #                 ylim = c(-15, -4)) +
  theme_bw()

# 18O
# there are sometimes 3 values
# first: filter out the values where no std are present (these values are based on 1 measurement)
# second: filter out the values with the highest std?
# second: filter out the values based on the lowest amount of values (normally 5)
d18O <- oxygen18 %>%
  rename(samplecode = Sample.ID,
         parameter = Isotope,
         value = MeanDeltaOfInjs,
         std = StdDevOfDeltaOfInjs) %>%
  select(samplecode, parameter, value, std) %>%
  group_by(samplecode) %>%
  filter(!is.na(std)) %>%
  mutate(namen = paste0("waarde", row_number())) %>%
  ungroup() %>%
  select(-std) %>%
  pivot_wider(names_from = namen,
              values_from = value)

ggplot(d18O, aes(x = waarde1, y = waarde2)) +
  geom_point() +
  geom_abline(slope = 1, linetype = "dashed") +
  #geom_smooth(method = 'lm', formula = y~x) +
  scale_x_continuous(name = expression(paste(delta ^{18}, "O", "(\u2030)", " value 1"))) +
  scale_y_continuous(name = expression(paste(delta ^{18}, "O", "(\u2030)", " value 2"))) +
  # coord_cartesian(xlim = c(-3, -0.5),
  #                 ylim = c(-3, -0.5)) +
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
d1 <- rbind(d2H, d18O) %>%
  arrange(samplecode)

#### Using raw data instead of summary ####

# count nr of good measurements per sample
dH <- deuterium_raw %>%
  select(Sample.ID, Analysis_Date_Time, Isotope, Final_Delta, MeanDeltaOfInjs, StdDevOfDeltaOfInjs,
         Ignore_Analysis) %>%
  # filter out wrong analysis (T)
  filter(Ignore_Analysis != "TRUE") %>%
  group_by(Sample.ID) %>%
  dplyr::summarise(nr.measurements = n(),
                   parameter = "d2H",
                   value = mean(Final_Delta),
                   std = sd(Final_Delta),
                   std_avg = mean(StdDevOfDeltaOfInjs))

dO <- oxygen18_raw %>%
  select(Sample.ID, Analysis_Date_Time, Isotope, Final_Delta, MeanDeltaOfInjs, StdDevOfDeltaOfInjs,
         Ignore_Analysis) %>%
  # filter out wrong analysis (T)
  filter(Ignore_Analysis != "TRUE") %>%
  group_by(Sample.ID) %>%
  dplyr::summarise(nr.measurements = n(),
                   parameter = "d18O",
                   value = mean(Final_Delta),
                   std = sd(Final_Delta),
                   std_avg = mean(StdDevOfDeltaOfInjs))

# combine dH and dO
d <- rbind(dH, dO)

# wide format for scatterplot
d_wide <- d %>%
  select(Sample.ID, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value) %>%
  mutate(watercode = substr(Sample.ID, start = 1, stop = 2)) 
d_wide$sampletype <- d_wide$watercode %>% 
  recode("AI" = "air",
         "de" = "demi-water",
         "GW" = "groundwater",
         "SR" = "surface runoff",
         "SW" = "surfacewater", 
         "SP" = "springwater",
         "WW" = "wastewater",
         "RW" = "rainwater",
         "TW" = "tapwater",
         "SE" = "seawater")

# determine line from TW001 to GW008
d_wide %>% 
  filter(Sample.ID %in% c("TW001", "GW051", "GW066", "GW041", "GW008")) %>%
  view()

# plot
ggplot(d_wide %>% filter(sampletype != "demi-water"), aes(x = `d18O`, y = `d2H`)) +
  geom_abline(aes(slope = 8.02, intercept = 9.47, colour = "GMWL"),
              linetype = "dashed", show.legend = TRUE) +
  geom_abline(aes(slope = 5.575918, intercept = 1.815, colour = "TWML"),
              linetype = "dashed", show.legend = T) +
  geom_abline(aes(slope = 7.937, intercept = 6.928, colour = "LMWL?"),
              linetype = "dashed", show.legend = T) +
  geom_point(aes(colour = sampletype)) +
  # geom_abline(aes(slope = , intercept = , colour = "LMWL"),
  #             linetype = "dashed", show.legend = TRUE) +
  #scale_fill_manual(name = "", values = c("blue")) +
  #scale_colour_manual(name = "", values = c("red", "steelblue", "green", "purple")) +
  #geom_text(hjust = 0, vjust = 2) +
  # scale_colour_brewer(name = "", palette = "Set1",
  #                     breaks = c("GMWL", "LMWL?", "TWML", "groundwater", "surface runoff", "tapwater"),
  #                     guide = guide_legend(
  #                       override.aes = list(pch = c(NA, NA, NA, 16, 16, 16), linetype = c(2, 2, 2, 0, 0, 0))
  #                     )) +
  scale_colour_manual(name = "", values = c("red", "lightblue", "purple", "steelblue", "orange", "purple"),
                     breaks = c("GMWL", "LMWL?", "TWML", "groundwater", "surface runoff", "tapwater"),
                     guide = guide_legend(
                       override.aes = list(pch = c(NA, NA, NA, 16, 16, 16), linetype = c(2, 2, 2, 0, 0, 0))
                      )) +
  scale_x_continuous(name = expression(paste(delta ^{18}, "O", "(\u2030)", " vs VSMOW"))) +
  scale_y_continuous(name = expression(paste(delta ^{2}, "H", "(\u2030)", " vs VSMOW"))) +
  coord_cartesian(xlim = c(-3.1, 1.76),
                  ylim = c(-20, 15)) +
  theme_bw()

###############################################################################
# save data
###############################################################################

write.xlsx(d1, paste0(output, "Clean_data/isotopes_clean.xlsx"))





