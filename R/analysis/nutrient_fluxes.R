#
# Input: recharge
#         
# Output: Figures
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
pacman::p_load(tidyverse, openxlsx, cowplot, scales, ggh4x)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/final_merged/" 

# load cleaned data
data <- read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx"))
metadata <- openxlsx::read.xlsx(paste0(input, "metadata_2021_2022.xlsx"))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/Output/" 

###############################################################################
# Editing data
###############################################################################

# SO4sea <- data %>% filter(sampletype == "seawater", parameter == "SO4") %>% pull(value) %>% mean()
# Clsea <- data %>% filter(sampletype == "seawater", parameter == "Cl") %>% pull(value) %>% mean()

# add metadata
data <- data %>%
  left_join(., metadata %>% select(samplecode, geology, geology_abr, 
                                   landuse_zonal_map, Land.use.based.on.own.observations))

# overview for DOC analysis UvA
d <- data %>%
  filter(year == 2021, parameter %in% c("DOC", "EC", "NO3", "NH4", "PO4")) %>%
  select(samplecode, sampletype, year, parameter, value, sd) %>%
  group_by(sampletype, parameter) %>%
  summarise(n = n(),
            min = min(value, na.rm = T),
            med = median(value, na.rm = T),
            avg = mean(value, na.rm = T),
            max = max(value, na.rm = T))

d <- data %>%
  filter(year == 2021, parameter %in% c("EC", "NH4", "NO3", "PO4", "DOC")) %>%
  select(samplecode, sampletype, subtype, year, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value)

ggplot(d %>% filter(sampletype %in% c("groundwater", "surface runoff", "surface water", "wastewater")), 
       aes(y = DOC, x = PO4)) +
  geom_point() +
  facet_wrap(facets = "sampletype", scales = "free")

ggplot(d %>% filter(sampletype %in% c("groundwater", "surface runoff", "surface water", "wastewater")), 
       aes(y = DOC, x = NO3)) +
  geom_point() +
  facet_wrap(facets = "sampletype", scales = "free")

write.xlsx(d, paste0("C:/Users/mikewit/Documents/SEALINK/Data/",
                     "Raw_data/Lab/DOC_UvA/2023/Overview_samples_EC_2022.xlsx"))

###############################################################################
# Analysis
###############################################################################

# Overview

### Calculate TIN, TIP

d <- data %>%
  filter(year == 2022, parameter %in% c("NH4", "NO3", "NO2")) %>%
  select(samplecode, sampletype, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value) %>%
  mutate(`TIN umol` = ((NH4/18.04)+(NO2/46.0055)+(NO3/62.0049))*1000) %>%
  view()

d <- data %>%
  filter(year == 2021, parameter %in% c("NH4", "NO3", "NO2", "PO4", "DOC")) %>%
  select(samplecode, sampletype, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value) %>%
  mutate(`TIN umol` = ((NH4/18.04)+(NO2/46.0055)+(NO3/62.0049))*1000)

ggplot(d %>% filter(!sampletype %in% c("rainwater", "tapwater")), aes(x = DOC/12*1000, y = `TIN umol`)) +
  geom_point() +
  labs(x = "DOC umol") +
  facet_wrap(facets = "sampletype", scales = "free") +
  theme_bw()


###
# Nutrient statistics
###

data %>%
  filter(year %in% c(2021, 2022),
         !sampletype %in% c("rainwater", "tapwater", "seawater"),
         parameter %in% c("DOC")) %>%
  mutate(subtype = ifelse(subtype == "spring", "groundwater", subtype)) %>%
         #parameter %in% c("EC", "pH", "NH4", "NO3", "PO4", "DOC", "E.coli")) %>%
  group_by(subtype, parameter) %>%
  summarise(n = n(),
            min = min(value, na.rm = T),
            median = median(value, na.rm = T),
            avg = mean(value, na.rm = T),
            max  = max(value, na.rm = T),
            sd = sd(value, na.rm = T)) %>%
  view()

###
# Nutrient concentrations water types
###

### Just nutrients
i <- "subtype"
d <- data %>%
  filter( year %in% c(2021, 2022),
          #sampletype %in% c("groundwater", "surface runoff"),
          !sampletype %in% c("rainwater", "tapwater", "seawater"),
          parameter %in% c("EC", "pH", "NH4", "NO3", "PO4", "DOC")) %>%
  # change EC from uS/cm to mS/cm
  mutate(value = ifelse(parameter == "EC", value / 1000, value),
         units = ifelse(parameter == "EC", "mS/cm", units),
         parameter = ifelse(parameter == "DOC", "**DOC", parameter)) %>%
  # change highest E.coli values for visibility
  mutate(value = ifelse(parameter == "E.coli" & samplecode %in% c("SW001", "SR002"),
                        29999999, value)) %>%
  # add spring subtype to groundwater
  mutate(subtype = ifelse(subtype == "spring", "groundwater", subtype)) %>%
  mutate(units = ifelse(parameter == "E.coli", "CFU/dl", units)) %>%
  group_by(!!as.symbol(i), parameter) %>%
  mutate(num = paste0("n=", n_distinct(samplecode))) %>%
  ungroup() %>%
  # change number of samples for DOC /E.coli to match others for plotting purposes
  # this is ugly, change later!
  mutate(num = case_when(
    parameter %in% c("**DOC", "E.coli") & subtype == "untreated wastewater" ~ "n=6",
    parameter %in% c("**DOC", "E.coli") & subtype == "treated wastewater" ~ "n=6",
    parameter %in% c("**DOC", "E.coli") & subtype == "surfacewater" ~ "n=8",
    parameter %in% c("**DOC", "E.coli") & subtype == "surface runoff" ~ "n=12",
    parameter %in% c("**DOC", "E.coli") & subtype == "rooi discharge" ~ "n=21",
    parameter %in% c("**DOC", "E.coli") & subtype == "groundwater" ~ "n=135",
    TRUE ~ num)) %>%
  mutate(parameter = paste0(parameter, " [", units, "]")) %>%
  mutate(parameter = factor(parameter, levels = c("EC [mS/cm]", 
                                                  "pH [-]",
                                                  "NH4 [mg/l]", 
                                                  "NO3 [mg/l]", 
                                                  "PO4 [mg/l]", 
                                                  "**DOC [mg/l]",
                                                  "E.coli [CFU/dl]")))

# vertical boxplots
ggplot(d %>% mutate(myaxis = paste0(!!as.symbol(i), "\n", num)), 
       aes(x = value, y = myaxis, fill = !!as.symbol(i))) +
  geom_boxplot() +
  scale_y_discrete("") +
  scale_x_continuous("") +
  facet_wrap(~ parameter, scales = "free_x", nrow = 1) +
  # facetted_pos_scales(y = list(COL == 2 ~ scale_y_discrete(guide = "none"),
  #                              COL == 3 ~ scale_y_discrete(guide = "none"),
  #                              COL == 4 ~ scale_y_discrete(guide = "none"),
  #                              COL == 5 ~ scale_y_discrete(guide = "none"),
  #                              COL == 6 ~ scale_y_discrete(guide = "none"),
  #                              COL == 7 ~ scale_y_discrete(guide = "none"))) +
  #scale_x_continuous("", limits = quantile(d$value, c(0.1, 0.9), na.rm = T)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Composition of different water types",
       subtitle = "Boxplots for nutrients and EC, 2021-2022. **DOC only for 2021")
ggsave(paste0(output, "/Figures/Hydrochemistry/Nutrients/nutrient_watertypes.png"),
       width = 10, height = 4)


## zoom-in for EC and NO3 concentrations for better visability
## without high numbers for better visuality
dat <- d %>%
  mutate(value = case_when(
    # high EC values
    parameter == "EC [mS/cm]" & value > 15 ~ NA_real_,
    parameter == "NO3 [mg/l]" & value > 210 ~ NA_real_,
    TRUE ~ value))

# vertical boxplots without high numbers for better visuality
ggplot(dat %>% mutate(myaxis = paste0(!!as.symbol(i), "\n", num)), 
       aes(x = value, y = myaxis, fill = !!as.symbol(i))) +
  geom_boxplot() +
  scale_y_discrete("") +
  scale_x_continuous("") +
  facet_wrap(~ parameter, scales = "free_x", nrow = 1) +
  # facetted_pos_scales(y = list(COL == 2 ~ scale_y_discrete(guide = "none"),
  #                              COL == 3 ~ scale_y_discrete(guide = "none"),
  #                              COL == 4 ~ scale_y_discrete(guide = "none"),
  #                              COL == 5 ~ scale_y_discrete(guide = "none"),
  #                              COL == 6 ~ scale_y_discrete(guide = "none"),
  #                              COL == 7 ~ scale_y_discrete(guide = "none"))) +
  #scale_x_continuous("", limits = quantile(d$value, c(0.1, 0.9), na.rm = T)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Composition of different water types",
       subtitle = "Boxplots nutrients and EC zoom-in, 2021-2022. **DOC only for 2021")
ggsave(paste0(output, "/Figures/Hydrochemistry/Nutrients/nutrient_watertypes_zoomin.png"),
       width = 10, height = 4)

###
# Nutrient ratios
###

# selecting data
i <- "subtype"
d <- data %>%
  filter( year %in% c(2021, 2022),
          # select only groundwater, wastewater, surface runoff
          !sampletype %in% c("rainwater", "tapwater", "seawater"),
          parameter %in% c("NH4", "NO3", "PO4", "DOC")) %>%
  # change EC from uS/cm to mS/cm
  mutate(value = ifelse(parameter == "EC", value / 1000, value),
         units = ifelse(parameter == "EC", "mS/cm", units),
         parameter = ifelse(parameter == "DOC", "**DOC", parameter)) %>%
  # change highest E.coli values for visibility
  mutate(value = ifelse(parameter == "E.coli" & samplecode %in% c("SW001", "SR002"),
                        29999999, value)) %>%
  # add spring subtype to groundwater
  mutate(subtype = ifelse(subtype == "spring", "groundwater", subtype)) %>%
  mutate(units = ifelse(parameter == "E.coli", "CFU/dl", units)) %>%
  group_by(!!as.symbol(i), parameter) %>%
  mutate(num = paste0("n=", n_distinct(samplecode))) %>%
  ungroup() %>%
  # change number of samples for DOC /E.coli to match others for plotting purposes
  # this is ugly, change later!
  mutate(num = case_when(
    parameter %in% c("**DOC", "E.coli") & subtype == "untreated wastewater" ~ "n=6",
    parameter %in% c("**DOC", "E.coli") & subtype == "treated wastewater" ~ "n=6",
    parameter %in% c("**DOC", "E.coli") & subtype == "surfacewater" ~ "n=8",
    parameter %in% c("**DOC", "E.coli") & subtype == "surface runoff" ~ "n=12",
    parameter %in% c("**DOC", "E.coli") & subtype == "rooi discharge" ~ "n=21",
    parameter %in% c("**DOC", "E.coli") & subtype == "groundwater" ~ "n=135",
    TRUE ~ num)) %>%
  mutate(parameter = paste0(parameter, " [", units, "]")) %>%
  # calculate nutrient molar ratios
  select(samplecode, subtype, num, year, parameter, value) %>%
  pivot_wider(names_from = parameter, 
              values_from = value) %>% 
  mutate(`N:P` = ((`NH4 [mg/l]`/18.04)+(`NO3 [mg/l]`/62.0049))/(`PO4 [mg/l]`/94.97),
         `C:P` = "",
         `C:N:P` = "") %>%
  pivot_longer(cols = `**DOC [mg/l]`:`N:P`,
               names_to = "parameter",
               values_to = "value") %>%
  mutate(parameter = factor(parameter, levels = c("EC [mS/cm]", 
                                                  "pH [-]",
                                                  "NH4 [mg/l]", 
                                                  "NO3 [mg/l]", 
                                                  "PO4 [mg/l]", 
                                                  "**DOC [mg/l]",
                                                  "E.coli [CFU/dl]",
                                                  "N:P")))

# vertical boxplots all data
ggplot(d %>% mutate(myaxis = paste0(!!as.symbol(i), "\n", num)), 
       aes(x = value, y = myaxis, fill = !!as.symbol(i))) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_discrete("") +
  scale_x_continuous("") +
  facet_wrap(~ parameter, scales = "free", nrow = 1) +
  facetted_pos_scales(y = list(COL == 2 ~ scale_y_discrete(guide = "none"),
                               COL == 3 ~ scale_y_discrete(guide = "none"),
                               COL == 4 ~ scale_y_discrete(guide = "none"),
                               COL == 5 ~ scale_y_discrete(guide = "none"),
                               COL == 6 ~ scale_y_discrete(guide = "none"),
                               COL == 7 ~ scale_y_discrete(guide = "none"),
                               COL == 8 ~ scale_y_discrete(guide = "none"))) +
  #scale_x_continuous("", limits = quantile(d$value, c(0.1, 0.9), na.rm = T)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Composition of different water types",
       subtitle = "Boxplots for nutrients and ratios, 2021-2022. **DOC only from 2021")
ggsave(paste0(output, "/Figures/Hydrochemistry/Nutrients/nutrient_ratios_watertypes.png"),
       width = 10, height = 4)

# Inzoom for NO3 / N:P
ggplot(d %>% mutate(myaxis = paste0(!!as.symbol(i), "\n", num)) %>%
         mutate(value = case_when(
           parameter == "NO3 [mg/l]" & value > 210 ~ NA_real_,
           parameter == "N:P" & value > 100 ~ NA_real_,
           TRUE ~ value)), 
       aes(x = value, y = myaxis, fill = !!as.symbol(i))) +
  geom_boxplot() +
  scale_y_discrete("") +
  scale_x_continuous("") +
  facet_wrap(~ parameter, scales = "free_x", nrow = 1) +
  # facetted_pos_scales(y = list(COL == 2 ~ scale_y_discrete(guide = "none"),
  #                              COL == 3 ~ scale_y_discrete(guide = "none"),
  #                              COL == 4 ~ scale_y_discrete(guide = "none"),
  #                              COL == 5 ~ scale_y_discrete(guide = "none"),
  #                              COL == 6 ~ scale_y_discrete(guide = "none"),
  #                              COL == 7 ~ scale_y_discrete(guide = "none"),
  #                              COL == 8 ~ scale_y_discrete(guide = "none"))) +
  #scale_x_continuous("", limits = quantile(d$value, c(0.1, 0.9), na.rm = T)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Composition of different water types",
       subtitle = "Boxplots for nutrients and ratios, 2021-2022. **DOC only from 2021")
ggsave(paste0(output, "/Figures/Hydrochemistry/Nutrients/nutrient_ratios_watertypes_inzoom.png"),
       width = 10, height = 4)

ggplot(d %>% mutate(myaxis = paste0(!!as.symbol(i), "\n", num)) %>%
         filter(parameter == "N:P"), 
       aes(x = value, y = myaxis, fill = !!as.symbol(i))) +
  geom_boxplot() +
  coord_cartesian(xlim = c(0, 250)) +
  labs(x = "N:P", y = "",
       title = "N:P ratio for different water types, zoom-in") +
  theme_bw() +
  theme(legend.position = "none")

# statistics for N:P ratio per subtype
d %>% filter(parameter == "N:P") %>%
  mutate(myaxis = paste0(!!as.symbol(i), "\n", num)) %>%
  group_by(myaxis) %>%
  summarise(min = min(value, na.rm = T),
            avg = mean(value, na.rm = T),
            med = median(value, na.rm = T),
            max = max(value, na.rm = T))

d %>% filter(parameter == "N:P") %>% 
  mutate(myaxis = paste0(!!as.symbol(i), "\n", num)) %>% 
  ggplot(., aes(x = myaxis, y = value)) + 
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 200))

## without high numbers for better visuality
d <- d %>%
  mutate(value = case_when(
    # high EC values
    parameter == "EC [mS/cm]" & value > 15 ~ NA_real_,
    parameter == "NO3 [mg/l]" & value > 210 ~ NA_real_,
    TRUE ~ value))
 
# vertical boxplots without high numbers for better visuality
ggplot(d %>% mutate(myaxis = paste0(!!as.symbol(i), "\n", num)), 
       aes(x = value, y = myaxis, fill = !!as.symbol(i))) +
  geom_boxplot() +
  scale_y_discrete("") +
  scale_x_continuous("") +
  facet_wrap(~ parameter, scales = "free", nrow = 1) +
  facetted_pos_scales(y = list(COL == 2 ~ scale_y_discrete(guide = "none"),
                               COL == 3 ~ scale_y_discrete(guide = "none"),
                               COL == 4 ~ scale_y_discrete(guide = "none"),
                               COL == 5 ~ scale_y_discrete(guide = "none"),
                               COL == 6 ~ scale_y_discrete(guide = "none"),
                               COL == 7 ~ scale_y_discrete(guide = "none"))) +
  #scale_x_continuous("", limits = quantile(d$value, c(0.1, 0.9), na.rm = T)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Composition of different water types",
       subtitle = "Boxplots nutrients and EC zoom-in, 2021-2022. DOC only from 2021")
ggsave(paste0(output, "/Figures/Hydrochemistry/Nutrients/nutrient_watertypes_zoomin.png"),
       width = 12, height = 10)





