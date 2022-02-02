#
# Input: raw data of the lab analyses of Ion Chromotography (IC), 
# Ion Induced Plasma Mass Spectromotry (ICP-MS), Discrete Analyzer (DA),
# Isotope Analyzer (IA) from TUD and Dissolved Organic Carbon Analyzer (DOC) 
# from UvA
#         
# Output: Cleaned lab results merged together in long and wide format
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
ICP <- read.xlsx(paste0(input, "Lab/ICP/20220124_Mike_Iris_all_elements.xlsx"),
                 sheet = "summary")
ICP_dil_fac <- read.xlsx(paste0(input, "Lab/ICP/ICP preperations data.xlsx"),
                         sheet = "ICP-MS handed in", startRow = 17)

# DA data file
DA <- read.xlsx(paste0(input, "Lab/DA/PO4 & NH4 Iris.xlsx"),
                sheet = "Summary")
DA_alk <- read.xlsx(paste0(input, "Lab/DA/Alkalinity Iris.xlsx"),
                    sheet = "Summary")

# Isotope data file


# DOC data file


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
  #filter(!is.na(value_dil)) %>%
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
    TRUE ~ "" )) 

d_IC <- d %>%
  mutate(detection_limit = ifelse(dt == "<" & detection_limit > waarde,
                                  waarde, detection_limit),
         waarde = ifelse(is.na(waarde), value, waarde),
         method = "IC") %>%
  select(samplecode, parameter, waarde, dt, detection_limit, units, method) %>%
  rename(limit_symbol = dt,
         value = waarde)

## make wide format  -> make wide format for all lab results
# d_IC_wide <- d_IC %>%
#   mutate(parameter = paste(parameter, units)) %>%
#   select(-c(detection_limit, units)) %>%
#   pivot_wider(.,
#               names_from = parameter,
#               values_from = c(value, limit_symbol),
#               names_glue = "{parameter}_{.value})") %>%
#   select(1, 9, 2, 10, 3, 11, 4, 12, 5, 13, 6, 14, 7, 15, 8)

# if(nrow(check) > 0) {
#   stop("More than 1 value for .. in a sample")
# }


#### DA PO4 and NH4 edits and checks ####

# add dl and ds based on analysis!!

d_DA <- DA %>%
  # select relevant columns
  select(Sample.ID, Results, Units, Test) %>%
  # add samplecode to lab numbers 
  left_join(., lab_table %>% filter(analysis == "DA") %>% select(-analysis),
            by = c("Sample.ID" = "labcode")) %>%
  # remove rows with calibration standards and blanc samples
  filter(!is.na(samplecode)) %>%
  # add parameter column
  mutate(parameter = ifelse(str_detect(Units, "P"),
                            "PO4", "NH4"),
         Units.2 = "mg/l") %>%
  # Adjust units: Convert mg/l P to mg/l PO4 and mg/l N to mg/l NH4
  mutate(Results.2 = ifelse(str_detect(Units, "P"),
                            Results / (30.973762 / 94.9714),
                            Results / (14.0067 / 18.039))) 

# place different units and corresponding values in long format
d <- rbind(d_DA %>% select(samplecode, parameter, Results, Units, Test),
           d_DA %>% select(samplecode, parameter, Results.2, Units.2, Test) %>% 
             rename(Results = Results.2, Units = Units.2)) %>%
  rename(value = Results,
         units = Units) %>%
  arrange(samplecode, parameter) %>%
  # add detection limits based on the test used  
  ### Still need to adjust these !!! using detectielimieten or rapportagegrenzen?? ###
  mutate(detection_limit = case_when(
                            Test == "NH4 10" ~ 1.0, # should be 0.2 or 0.04?
                            Test == "o-PHOS 1" ~ 0.1, # should be .. or 0.013?
                            Test == "o-PHOS 5" ~ 0.5, # should be ... 
                            Test == "o-PHOS 20" ~ 1.0, # should be ...
                            TRUE ~ NA_real_ )) %>%
    # assume negative values to be <dl and add detection limits based on DA analysis method manual
  # where dl NH4 = 0.04 and dl PO4 = 0.013 -> CHECK!! 
  mutate(limit_symbol = ifelse(value < detection_limit,
                               "<", ""),
         # change detection limit for mg/l units
         detection_limit = case_when(
                             parameter == "NH4" & units == "mg N/L " ~ detection_limit,
                             parameter == "NH4" & units == "mg/l" ~ detection_limit / (14.0067 / 18.039),
                             parameter == "PO4" & units == "mg P/L " ~ detection_limit,
                             parameter == "PO4" & units == "mg/l" ~ detection_limit / (30.973762 / 94.9714),
                             TRUE ~ NA_real_) %>% round(digits = 4),
         # set negative values to 0, 
        ## still change values < dl to the dl ???
         value = ifelse(value < 0, 0, value),
         method = "DA") 

###
### Change negative values and values < dl ???
### 

d %>%
  mutate(check = ifelse(value < detection_limit, 1, 0)) %>%
  filter(check == 1) 

## compare PO4 from IC and DA
check <- d %>% 
  filter(parameter == "PO4", units == "mg/l") %>%
  rename(value_DA = value) %>%
  select(samplecode, parameter, value_DA, units) %>%
  left_join(., d_IC %>% 
                  filter(parameter == "PO4") %>%
              rename(value_IC = value) %>%
              select(samplecode, parameter, value_IC))

ggplot(check, aes(x = value_DA, y = value_IC)) +
  geom_point() +
  geom_abline(intercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(-0.2, 30),
                     name = "PO4 DA [mg/l]") +
  scale_y_continuous(limits = c(-0.2, 30),
                     name = "PO4 IC [mg/]") +
  theme_bw()


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


# DA uses multiple tests for PO4. Make sure only 1 value is available per sample
check <- d %>%
  filter(units == "mg/l") %>%
  group_by(samplecode, parameter) %>%
  summarise(nr.measurements = length(value),
            Test = ifelse(n_distinct(Test) == 1, Test,
                          paste(unique(Test), collapse = ", "))) %>%
  filter(nr.measurements > 1) 

if(nrow(check) > 0) {
  stop(paste("More than 1 value for", unique(check$parameter), "in a sample"))
}

# reorder columns into final format
d_DA <- d %>%
  select(samplecode, parameter, value, limit_symbol, detection_limit, units, method)

# Clean up DA alkalinity set
d_ALK <- DA_alk %>%
  # select relevant columns
  select(Sample.ID, Results, Units, Test) %>%
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
         method = "DA") %>%
  mutate(detection_limit = case_when(
    Test == "ALKALIN 250" ~ 8, # this value is not right yet!!
    Test == "ALKALIN 550" ~ 16, # this value is from the Alkalinity 0-500 mg/l range
    TRUE ~ NA_real_ )) %>%
  select(samplecode, parameter, value, limit_symbol, detection_limit, units, method)
  

#### ICP edits and checks ####

oldnames <- names(ICP)
newnames <- c("labcode", "Li", "Be", "B", "Na", "Mg", "Mg26", "Al",
              "Si", "P", "S", "K", "Ca", "Ti", "V", "Cr", "Mn",
              "Fe56", "Fe57", "Cu", "Zn", "Co", "Ni", "As", "Se",
              "Mo", "Ag", "Cd", "Sb", "Ba", "Pb")
remove.list <- paste(c("SRM", "ERM", "mg/"), collapse = '|')

# values with #Name indicates that the detector is overloaded
# values with x indicates that the value is above the calibration line
# values with b indicates that the value is below the detection limit ??

d <- ICP %>%
  # rename column names
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames) %>%
  # remove rows with calibration standards and blank samples
  filter(!str_detect(labcode, remove.list)) %>%
  # add samplecode to lab numbers 
  left_join(., lab_table %>% filter(analysis == "ICP") %>% select(-analysis),
            by = c("labcode" = "labcode")) %>%
  # add dilution factor 
  left_join(., ICP_dil_fac %>% select(Sample.ID, Dilution),
            by = c("labcode" = "Sample.ID")) %>%
  # convert all elements to characters so they can be placed in one column
  mutate_at(c(2:31), as.character) %>%
  # add Fe56 and Fe57 isotopes together
  mutate(Fe56_dt = ifelse(str_detect(Fe56, "b"), "<", ""),
         Fe56_v = parse_number(Fe56),
         Fe57_dt = ifelse(str_detect(Fe57, "b"), "<", ""),
         Fe57_v = parse_number(Fe57)) %>%
  mutate(Fe = case_when(
    Fe56_dt != "<" & Fe57_dt != "<" ~ as.numeric(Fe56_v) + as.numeric(Fe57_v),
    Fe56_dt != "<" & Fe57_dt == "<" ~ as.numeric(Fe56_v),
    Fe56_dt == "<" & Fe57_dt != "<" ~ as.numeric(Fe57_v),
    Fe56_dt == "<" & Fe57_dt == "<" ~ min(as.numeric(Fe56_v), as.numeric(Fe57_v)),
    TRUE ~ NA_real_ ) %>% as.character()) %>%
  # remove help columns
  select(-c(Fe56_dt, Fe56_v, Fe57_dt, Fe57_v)) %>%
  # place parameters in long format
  pivot_longer(., cols = c(Li:Fe, -samplecode, -Dilution),
               values_to = "value",
               names_to = "parameter") %>%
  # add limit symbol, detection limit values and units
  mutate(limit_symbol = ifelse(str_detect(value, "b"), "<", 
                               ifelse(str_detect(value, "x"), ">", "")),
         units = "ug/l") %>%
  # change values to numeric and correct for dilution
  mutate(value_dil = parse_number(value),
         value = parse_number(value) * Dilution) %>%
  # change units for Ca, Fe, K, Mg, Na, P, S, en Si
  mutate(value = ifelse(parameter %in% c("Ca", "Fe", "K", "Mg", "Mg26", "Na", "P", "S", "Si"),
                        value / 1000, value),
         units = ifelse(parameter %in% c("Ca", "Fe", "K", "Mg", "Mg26", "Na", "P", "S", "Si"),
                        "mg/l", units),
         detection_limit = ifelse(limit_symbol == "<", value, NA),
         method = "ICP-MS") %>%
  # select only relevant columns
  select(samplecode, parameter, value, value_dil, limit_symbol, detection_limit, units, method)


## Some quick checks, move elsewhere later!
# Dilution factors in histogram
# ggplot(d %>% select(samplecode, Dilution) %>% unique(),
#        aes(x = Dilution)) +
#   geom_histogram(binwidth = 10) +
#   scale_x_continuous(name = "Dilution factor") +
#   theme_bw()
# 
# ggplot(d %>% select(samplecode, Dilution) %>% unique(),
#        aes(y = Dilution)) +
#   geom_boxplot()
# 
# d %>% 
#   select(samplecode, Dilution) %>% 
#   unique() %>%
#   summary()

# boxplot per parameter to see from which parameters to change units.
calc_boxplot_stat <- function(x) {
  coef <- 1.5
  n <- sum(!is.na(x))
  # calculate quantiles
  stats <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
  names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
  iqr <- diff(stats[c(2, 4)])
  # set whiskers
  outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
  if (any(outliers)) {
    stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
  }
  return(stats)
}

ggplot(d %>% mutate(parameter = paste0(parameter, " [", units, "]")), 
       aes(x = parameter, y = value, groep = parameter)) +
  #geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.data = calc_boxplot_stat, geom = "boxplot") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") +
  theme_bw() +
  facet_wrap(facets = "parameter", scales = "free")
# boxplots with only values >dl
ggplot(d %>% mutate(parameter = paste0(parameter, " [", units, "]")) %>%
         filter(limit_symbol != "<"), 
       aes(x = parameter, y = value, groep = parameter)) +
  #geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.data = calc_boxplot_stat, geom = "boxplot") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") +
  theme_bw() +
  facet_wrap(facets = "parameter", scales = "free")

ggplot(d %>% filter(units != "mg/l"),
         #mutate(parameter = paste0(parameter, " [", units, "]")) %>% 
       aes(x = parameter, y = value, groep = parameter)) +
  #geom_boxplot(outlier.shape = NA) +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  stat_summary(fun.data = calc_boxplot_stat, geom = "boxplot", fill = "lightgrey", width = 0.6) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "concentration [ug/l]") +
  coord_cartesian(ylim = c(0, 300)) +
  theme_bw() 
  #facet_wrap(facets = "parameter", scales = "free")

# how many samples >dl and <cl per parameter
check_limits <- d %>%
  group_by(parameter) %>%
  summarise(samples = n(),
            "< cal. line [%]" = round(length(value[limit_symbol == "<"]) / n() * 100, digits = 1),
            "> cal. line [%]" = round(length(value[limit_symbol == ">"]) / n() * 100, digits = 1),
            "value ok [%]" = round(length(value[limit_symbol == ""]) / n() * 100, digits = 1)) %>%
  view()

d %>% 
  group_by(parameter) %>%
  summarise(n.dl = n_distinct(value[limit_symbol == "<"])) %>%
  view()

# make wide format ICP
d_ICP_wide <- d %>%
  # adjust values < and > dl
  mutate(parameter = paste(parameter, units),
         value = paste(limit_symbol, value)) %>%
  select(samplecode, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value) 
    
d_ICP <- d %>% select(-value_dil)  


# Combine all labresults 
d <- rbind(d_IC, d_DA, d_ALK, d_ICP) %>%
  arrange(samplecode, parameter) %>%
  mutate(notes = "")

# create wide format
d_wide <- d %>%
  mutate(parameter = paste0(parameter, " [", units, "]"),
         value = paste(limit_symbol, value)) %>%
  select(samplecode, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value) 

###############################################################################
# save data
###############################################################################

write.xlsx(d, paste0(output, "Clean_data/lab_data_long.xlsx"))
write.xlsx(d_wide, paste0(output, "Clean_data/lab_data_wide.xlsx"))
write.xlsx(d_ICP_wide, paste0(output, "Clean_data/lab_ICP_wide.xlsx"))

