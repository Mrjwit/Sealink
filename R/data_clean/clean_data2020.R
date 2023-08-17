#
# Input: hydrochemistry datafile 2020 (Jessie)
#         
# Output: Compatible data format other hydrochemical data
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 03-04-2022
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, cowplot)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Students/Jessie/Data_overdracht/" 

# load cleaned data
#data <- openxlsx::read.xlsx(paste0(input, "Overdracht6_Hydrochemistry_2020_fast_new_19072022.xlsx"))
data <- openxlsx::read.xlsx(paste0(input, "Overdracht14_Hydrochemistry_Complete2020_RStudio_03_08_2022.xlsx")) # this file was used in R-scripts

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/2020/" 

###############################################################################
# Editing data
###############################################################################

varsmeasure4 = c("B", "Br", "Ca", "Cl", "EC_uS", "F", "Fe", "NO2","SO4", "Mg", "Mn", "SO4",
                 "HCO3","K", "Na", "NH4", "NO3", "PO4", "pH", "Si", "Temp", "NO3_field", 
                 "Turbidity", "Al", "Zn", "Ti", "Cd", "Ni", "V")

## units, detection limits and method columns are not unique
# add parameter name to these columns as identifier to put into long format later
colnames(data)[22:25] <- c("value_alkalinity", "limitsymbol_alkalinity", "units_alkalinity", "method_alkalinity")
colnames(data)[26:29] <- c("value_pH", "limitsymbol_pH", "units_pH", "method_pH")
colnames(data)[30:33] <- c("value_EC", "limitsymbol_EC", "units_EC", "method_EC")
colnames(data)[35:38] <- c("value_Ag", "limitsymbol_Ag", "units_Ag", "method_Ag")
colnames(data)[40:43] <- c("value_Al", "limitsymbol_Al", "units_Al", "method_Al")
colnames(data)[45:48] <- c("value_As", "limitsymbol_As", "units_As", "method_As")
colnames(data)[50:53] <- c("value_B", "limitsymbol_B", "units_B", "method_B")
colnames(data)[55:58] <- c("value_Ba", "limitsymbol_Ba", "units_Ba", "method_Ba")
colnames(data)[60:63] <- c("value_Be", "limitsymbol_Be", "units_Be", "method_Be")
colnames(data)[c(64,66,67,68)] <- c("value_Br", "limitsymbol_Br", "units_Br", "method_Br")
colnames(data)[70:73] <- c("value_Ca", "limitsymbol_Ca", "units_Ca", "method_Ca")
colnames(data)[75:78] <- c("value_Cd", "limitsymbol_Cd", "units_Cd", "method_Cd")
colnames(data)[80:83] <- c("value_Cl", "limitsymbol_Cl", "units_Cl", "method_Cl")
colnames(data)[105:108] <- c("value_Fe", "limitsymbol_Fe", "units_Fe", "method_Fe") # Fe --> recalculated concentrations from dilutions seem off! Also, better to take Fe56 instead of Fe57
colnames(data)[110:113] <- c("value_F", "limitsymbol_F", "units_F", "method_F")
colnames(data)[114:117] <- c("value_HCO3", "limitsymbol_HCO3", "units_HCO3", "method_HCO3")
colnames(data)[119:122] <- c("value_K", "limitsymbol_K", "units_K", "method_K")
colnames(data)[124:127] <- c("value_Li", "limitsymbol_Li", "units_Li", "method_Li")
colnames(data)[129:132] <- c("value_Mg", "limitsymbol_Mg", "units_Mg", "method_Mg")  # Mg24 taken instead of Mg26 what Jessie did
colnames(data)[139:142] <- c("value_Mn", "limitsymbol_Mn", "units_Mn", "method_Mn")
colnames(data)[144:147] <- c("value_Mo", "limitsymbol_Mo", "units_Mo", "method_Mo")
colnames(data)[149:152] <- c("value_Na", "limitsymbol_Na", "units_Na", "method_Na")
colnames(data)[184:187] <- c("value_NH4", "limitsymbol_NH4", "units_NH4", "method_NH4") # multiple NH4 columns?
colnames(data)[159:162] <- c("value_Ni", "limitsymbol_Ni", "units_Ni", "method_Ni")
colnames(data)[164:167] <- c("value_NO2", "limitsymbol_NO2", "units_NO2", "method_NO2")
colnames(data)[169:172] <- c("value_NO3", "limitsymbol_NO3", "units_NO3", "method_NO3")
colnames(data)[174:177] <- c("value_Turbidity", "limitsymbol_Turbidity", "units_Turbidity", "method_Turbidity")
colnames(data)[189:192] <- c("value_NO3field", "limitsymbol_NO3field", "units_NO3field", "method_NO3field")
colnames(data)[194:196] <- c("value_NO2field", "units_NO2field", "method_NO2field")
colnames(data)[198:201] <- c("value_P", "limitsymbol_P", "units_P", "method_P")
colnames(data)[203:206] <- c("value_Pb", "limitsymbol_Pb", "units_Pb", "method_Pb")
colnames(data)[208:211] <- c("value_PO4", "limitsymbol_PO4", "units_PO4", "method_PO4")
colnames(data)[213:216] <- c("value_PO4_IC", "limitsymbol_PO4_IC", "units_PO4_IC", "method_PO4_IC")
colnames(data)[218:221] <- c("value_S", "limitsymbol_S", "units_S", "method_S")
colnames(data)[223:226] <- c("value_Sb", "limitsymbol_Sb", "units_Sb", "method_Sb")
colnames(data)[228:231] <- c("value_Se", "limitsymbol_Se", "units_Se", "method_Se")
colnames(data)[233:236] <- c("value_Si", "limitsymbol_Si", "units_Si", "method_Si")
colnames(data)[238:241] <- c("value_SO4", "limitsymbol_SO4", "units_SO4", "method_SO4")
colnames(data)[242:245] <- c("value_Temp", "limitsymbol_Temp", "units_Temp", "method_Temp")
colnames(data)[247:250] <- c("value_Ti", "limitsymbol_Ti", "units_Ti", "method_Ti")
colnames(data)[252:255] <- c("value_V", "limitsymbol_V", "units_V", "method_V")
colnames(data)[257:260] <- c("value_Zn", "limitsymbol_Zn", "units_Zn", "method_Zn")
colnames(data)[262:265] <- c("value_SAR", "limitsymbol_SAR", "units_SAR", "method_SAR")

## Sr is missing??


# select relevant columns hydrochemical data
d <- data %>%
  select(putcode, samplecode, year, sampletype, xcoord, ycoord, 22:33, 35:38, 40:43, 45:48, 50:53, 55:58, 60:63,
         64, 66:68, 70:73, 75:78, 80:83, 105:108, 110:113, 114:117, 119:122, 124:127,
         129:132, 139:142, 144:147, 149:152, 184:187, 159:162, 164:167, 169:172, 174:177, 
         189:192, 194:196, 198:201, 203:206, 208:211, 213:216, 218:221, 223:226, 228:231, 
         233:236, 238:241, 242:245, 247:250, 252:255, 257:260, 262:265) %>%
  # limit_symbol column is missing for NO2_field measurements, so add it
  mutate(limitsymbol_NO2field = NA) %>%
  # the column types are inconsistent between numeric and characters
  # change all to characters now in order to pivot_longer, then change afterwards
  mutate(across(everything(), as.character)) %>%
  # put into long format
  pivot_longer(cols = c(starts_with("value"), starts_with("limitsymbol"),
                        starts_with("units"), starts_with("method")),
               names_to = c(".value", "parameter"), 
               names_pattern = "(.*?)_(.*)") %>%
  # change column types to numeric for concentrations
  mutate(value = as.numeric(value)) %>% 
  # rename limit_symbol column
  rename(limit_symbol = limitsymbol) %>% 
  # adjust wrong units and empty methods
  mutate(units = case_when(
    parameter == "EC" ~ "uS/cm",
    parameter == "alkalinity" ~ "mg/L as CaCO3",
    parameter == "HCO3" ~ "mg/L as HCO3",
    parameter == "pH" ~ "",
    parameter %in% c("NH4", "NO2", "NO2field") ~ "mg/L",
    TRUE ~ units)) %>%
  # remove NA from limit_symbol and method
  mutate(limit_symbol = ifelse(is.na(limit_symbol), "", limit_symbol),
         method = ifelse(is.na(method), "", method)) %>%
  # add other relevant columns so that if can be merged with other hydrochemical datasets
  mutate(year = 2020,
         sd = NA,
         detection_limit = NA,
         notes = "",
         watercode = "GW",
         subtype = "groundwater") %>%
  select(putcode, samplecode, year, parameter, value, sd, limit_symbol, detection_limit, units, method, notes, 
         watercode, sampletype, subtype, xcoord, ycoord)

# metadata 
d_meta <- data %>%
  select(putcode, wellname, samplecode, Tewaii.Code, sampletype, clustercode, year, xcoord, ycoord, geology, 
         )


# put hydrochemistry data in wide format including coordinates for GIS
d_wide <- d %>%
  # merge parameter and units
  mutate(parameter = paste0(parameter, " [", units, "]")) %>%
  # combine limit symbol with values?
  mutate(value = paste0(limit_symbol, value)) %>%
  left_join(., data %>% select(samplecode, xcoord, ycoord)) %>%
  select(putcode, samplecode, xcoord, ycoord, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value)
  
# wide format only numeric (without < signs)
d_wide_num <- d %>%
  # merge parameter and units
  mutate(parameter = paste0(parameter, " [", units, "]")) %>%
  left_join(., data %>% select(samplecode, xcoord, ycoord)) %>%
  select(putcode, samplecode, xcoord, ycoord, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value) %>%
  # replace NA with -99
  mutate_at(c(5:47), ~replace_na(., -99))

# save results
openxlsx::write.xlsx(d, paste0(output, "hydrochemistry2020.xlsx"))
openxlsx::write.xlsx(d_meta, paste0(output, "metadata2020.xlsx"))
write.csv(d_wide, paste0(output, "hydrochemistry2020_wide.csv"))
write.csv(d_wide_num, paste0(output, "hydrochemistry2020_wide_numeric.csv"))


## Checks
# P, PO4_DA and PO4_IC
dat <- d %>%
  filter(parameter %in% c("P", "PO4", "PO4_IC")) %>%
  select(putcode, samplecode, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value)

p1 <- ggplot(dat, aes(x = PO4, y = PO4_IC)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, linetype = "dashed") +
  scale_x_continuous("Discrete Analyzer") +
  scale_y_continuous("Ion Chromatogrpahy") +
  theme_bw()

p2 <- ggplot(dat, aes(x = PO4, y = PO4_IC)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, linetype = "dashed") +
  scale_x_continuous("Discrete Analyzer") +
  scale_y_continuous("Ion Chromatogrpahy") +
  coord_cartesian(xlim = c(0, 1.5),
                  ylim = c(0, 1.5)) +
  theme_bw()

p3 <- ggplot() + ggtitle("PO4 in mg/L")

plot_grid(p3, plot_grid(p1, p2),
          ncol = 1, rel_heights = c(0.1, 0.9))
