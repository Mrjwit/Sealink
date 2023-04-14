#
# Input: raw data of the lab analyses of Ion Chromotography (IC)
#        uses high-low calibration method (Swagatam) data format and
#        anions + cations ancat Mike data format
#
# Output: Cleaned lab results from IC merged together in long and wide format
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 06-03-2023
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
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/Lab/IC/" 

## IC data files high-low calibration method anions only
IC <- openxlsx::read.xlsx(paste0(input, "Second_fieldwork/IC-Calculations_v2.1_20180316(+Ac)_Start_14-02-2023.xlsx"),
                          sheet = "Report", startRow = 2, check.names = T) 
# NOTE: the results are copied from the 'Check results' tab to the 'Report' tab

## IC data files anion + cations 
IC2 <- read.csv(paste0(input, "Second_fieldwork/AnCat Mike - 06-03-2023.csv"),
                       sep = ";") 

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################

#### high-low calibration anions only IC edits and checks ####

oldnames <- names(IC)
newnames <- c("samplecode", "F", "Cl", "NO2", "Br", "NO3", "PO4", "SO4", 
              "dilution_factor", "ID2", "F_round", "Cl_round", "NO2_round", 
              "Br_round", "NO3_round", "PO4_round", "SO4_round")
remove.list <- paste(c("Estim", "St", "Blanco", "QS STD anion", "MW"), collapse = '|')

d_IC <- IC %>%
  # rename double column names
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames) %>%
  # remove ID2 column, which is redundant in this case 
  select(-ID2) %>%
  # remove rows with calibration standards and blank samples
  filter(!str_detect(samplecode, remove.list)) %>%
  # place parameters in long format
  pivot_longer(., cols = c(`F`:SO4_round, -dilution_factor),
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
  # add limit symbol, detection limit values, sd and units
  mutate(limit_symbol = ifelse(str_detect(value_round, "<"),"<", 
                               ifelse(str_detect(value_round, ">"), ">", "")),
         detection_limit = ifelse(str_detect(value_round, "<"),
                                  gsub("<", "", value_round) %>% 
                                    gsub(",", ".", .) %>% 
                                    as.numeric(), 
                                  NA),
         sd = NA,
         units = "mg/l") %>%
  ### OPTION: which values to be used? rounded (with detection limits) or unrounded?
  # use values >detection limit from 'value' and detection limits when <detection limit
  mutate(value = as.numeric(ifelse(limit_symbol == "<",
                                   detection_limit, value_unrounded)) %>%
           round(., digits = 3)) %>%
  # correct the values using the dilution factors only for values >dl
  mutate(value = ifelse(limit_symbol != "<", value * dilution_factor, value)) %>%
  # select only relevant columns
  select(samplecode, parameter, value, sd, limit_symbol, detection_limit, units)

## Separate dilutions and undiluted samples to compare
diluted <- d_IC %>%
  filter(str_detect(samplecode, "dil")) %>%
  mutate(samplecode = gsub("_dil", "", samplecode))

undiluted <- d_IC %>%
  filter(!str_detect(samplecode, "dil"))

# compare results and decide which concentrations to keep
d <- undiluted %>%
  #select(samplecode, parameter, value, limit_symbol) %>%
  left_join(., diluted %>% 
              rename(value_dil = value,
                     limit_symbol_dil = limit_symbol) %>% 
              select(samplecode, parameter, limit_symbol_dil, value_dil)) %>%
  # remove samples without dilutions
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
  # adjust limit symbols
  mutate(dt = case_when(
    limit_symbol == limit_symbol_dil ~ limit_symbol,
    limit_symbol == ">" & limit_symbol_dil != ">" ~ limit_symbol_dil,
    limit_symbol == "<" & limit_symbol_dil != "<" ~ limit_symbol_dil,
    is.na(limit_symbol_dil) ~ limit_symbol,
    TRUE ~ "" )) 

# make plots
ggplot(d, aes(value, value_dil, colour = limit_symbol)) +
  geom_abline(slope = 1, linetype = "dashed", colour = "steelblue") +
  geom_point() +
  scale_y_continuous("diluted samples") +
  scale_x_continuous("undiluted samples") +
  theme_bw() +
  facet_wrap(facets = "parameter", scales = "free")

# Put final value back to database and add relevant detection limits
dat <- d %>%
  mutate(detection_limit = case_when(
    dt == "<" & is.na(detection_limit) ~ value,
    dt == "<" & detection_limit > waarde ~ waarde,
    TRUE ~ detection_limit )) %>%
  mutate(waarde = ifelse(is.na(waarde), value, waarde)) %>%
  select(samplecode, parameter, waarde, sd, dt, detection_limit, units) %>%
  rename(limit_symbol = dt,
         value = waarde)

d_IC <- rbind(dat, undiluted %>% filter(!samplecode %in% dat$samplecode)) %>%
  arrange(samplecode) %>%
  mutate(method = "IC",
         notes = "")


#### anion + cation method only IC edits and checks ####
oldnames <- names(IC2)
newnames <- c("start", "samplecode", "sampletype", "method", "user", "dilution_factor", 
              "F", "Cl", "NO2", "Br", "NO3", "PO4", "SO4", "Na", "NH4", "K", "Mg", "Ca")
remove.list <- paste(c("Estim", "ST", "Blank", "QC anion", "QC Kation"), collapse = '|')

#### Calculate LOD from blanks using the formula to calculate concentrations from peak areas ####
blanks <- IC2 %>%
  # rename double column names
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames) %>%
  # remove redundant columns 
  select(-c(start, sampletype, method, user)) %>%
  # select only blanks
  filter(samplecode == "Blank") %>%
  # replace empty cells with zero
  replace(is.na(.), 0) %>%
  # calculate LOD per element
  pivot_longer(., cols = `F`:Ca,
               names_to = "parameter",
               values_to = "value") %>%
  # change zero concentrations to d/y-intercept from calibration line formula
  mutate(value = case_when(
    parameter == "F" & value == 0 ~ 0.031,
    parameter == "Cl" & value == 0 ~ 0.019,
    parameter == "NO2" & value == 0 ~ 0.029,
    parameter == "Br" & value == 0 ~ 0.021,
    parameter == "NO3" & value == 0 ~ 0.028,
    parameter == "PO4" & value == 0 ~ 0.016,
    parameter == "SO4" & value == 0 ~ 0.033,
    parameter == "Na" & value == 0 ~ 0.011,
    parameter == "NH4" & value == 0 ~ 0.008,
    parameter == "K" & value == 0 ~ 0.0045,
    parameter == "Mg" & value == 0 ~ 0.033,
    parameter == "Ca" & value == 0 ~ 0.044,
    TRUE ~ value)) %>%
  group_by(parameter) %>%
  summarise(avg = mean(value),
            sd = sd(value)) %>%
  mutate(LOD = avg + 3*sd)

#### for anion-cation method ####
d_IC2 <- IC2 %>%
  # rename double column names
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames) %>%
  # remove redundant columns 
  select(-c(start, sampletype, method, user)) %>%
  # remove rows with calibration standards and blank samples
  filter(!str_detect(samplecode, remove.list)) %>%
  # place parameters in long format
  pivot_longer(., cols = c(`F`:Ca, -dilution_factor),
               values_to = "value",
               names_to = "parameter") %>%
  # make values numeric
  mutate(value = gsub(",", ".", value) %>% as.numeric()) %>%
  # add detection_limts??
  #
  #
  # add limit symbol, detection limit values, sd and units
  mutate(limit_symbol = ifelse(value < 0 | is.na(value), "<", ""), 
         detection_limit = 1.0,
         sd = NA,
         units = "mg/l",
         method = "IC",
         notes = "") %>%
  # correct the values using the dilution factors, change NA to dl of 1
  # and make negative concentrations positive
  mutate(value = case_when(
    value < 0 ~ abs(value) * dilution_factor,
    is.na(value) ~ 1.0,
    TRUE ~ value * dilution_factor)) %>%
  # select only relevant columns
  select(samplecode, parameter, value, sd, limit_symbol, detection_limit, units, method, notes)

## Check detection limits
ggplot(d_IC2, aes(limit_symbol, value)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(facets = "parameter", scales = "free")

#### Combine all IC datasets ####
d_IC <- rbind(d_IC, d_IC2) %>%
  arrange(samplecode)

## make wide format  -> make wide format for all lab results
d_IC_wide <- d_IC %>%
  mutate(parameter = paste(parameter, units)) %>%
  mutate(value = paste0(limit_symbol, value)) %>%
  select(-c(detection_limit, limit_symbol, sd, units, method, notes)) %>%
  pivot_wider(., 
              names_from = parameter, 
              values_from = value)
  # select(-c(detection_limit, sd, units, method, notes)) %>%
  # pivot_wider(.,
  #             names_from = parameter,
  #             values_from = c(value, limit_symbol),
  #             names_glue = "{parameter}_{.value})") %>%
  # select(....)

# To check for which samples needs to be remeasured due to high dl
d_IC_wide %>% select(1:8) %>% filter(if_any(everything(), ~ str_detect(., "<"))) %>% view()

# 
# if(nrow(check) > 0) {
#  stop("More than 1 value for .. in a sample")
# }



###############################################################################
# save data
###############################################################################

openxlsx::write.xlsx(d_IC, paste0(output, "Clean_data/Second_fieldwork/IC_Oct-Jan_2023.xlsx"))
openxlsx::write.xlsx(d_IC_wide, paste0(output, "Clean_data/Second_fieldwork/IC_Oct-Jan_2023_wide.xlsx"))


