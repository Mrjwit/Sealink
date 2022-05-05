#
# Input: all cleaned datafiles
#         
# Output: PHREEQC output files of SI
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
pacman::p_load(tidyverse, openxlsx, devtools,
               cowplot, scales, ggpubr,
               phreeqc)

#install_github("vqv/ggbiplot")

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/" 

# load cleaned data
data <- openxlsx::read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx"))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# Data editing
###############################################################################

## change dataset into PHREEQC input data
# change concentrations to mmol/l
# change parameter names to PHREEQC names
# put data into wide format

d <- data %>%
  # only use 2021 data
  filter(year == 2021) %>%
  # first use only main ions and field parameters
  filter(parameter %in% c("Temp", "pH", "HCO3", "DO", "Cl", "Br", "NO3",
                          "SO4", "PO4", "F", "Na", "K", "Ca", "Mg", "Si",
                          "Fe", "Mn", "Al", "As", "Ba", "B", "Cd", "Cu", "Ni",
                          "Se", "Zn", "V", "Cr", "Mo")) %>%
  # for PO4 only use DA measurements and only mg/l units
  filter(units != "mg P/L") %>%
  mutate(remove = ifelse(method == "IC" & parameter == "PO4", 1, 0)) %>%
  filter(remove == 0) %>%
  # change concentrations of ug/l to mg/l
  mutate(value = ifelse(units == "ug/l", value / 1000, value)) %>%
  # change concentrations to mmol/l
  mutate(mmol = case_when(
    parameter == "Al" ~ value / 26.981539,
    parameter == "As" ~ value / 74.9216,
    parameter == "B" ~ value / 10.811,
    parameter == "Ba" ~ value / 137.327,
    parameter == "Br" ~ value / 79.904,
    parameter == "Ca" ~ value / 40.078,
    parameter == "Cd" ~ value / 112.411,
    parameter == "Cl" ~ value / 35.453,
    parameter == "Cr" ~ value / 51.9961,
    parameter == "Cu" ~ value / 63.546,
    parameter == "DO" ~ value / 15.999,
    parameter == "F" ~ value / 18.99843,
    parameter == "Fe" ~ value / 55.845,
    parameter == "HCO3" ~ value / 61.0168,
    parameter == "K" ~ value / 39.0983,
    parameter == "Mg" ~ value / 24.305,
    parameter == "Mn" ~ value / 54.938044,
    parameter == "Mo" ~ value / 95.95,
    parameter == "Na" ~ value / 22.989769,
    parameter == "Ni" ~ value / 58.6934,
    parameter == "NO3" ~ value / 62.0049,
    parameter == "PO4" ~ value / 94.971,
    parameter == "Se" ~ value / 78.96,
    parameter == "Si" ~ value / 28.0855,
    parameter == "SO4" ~ value / 96.06,
    parameter == "V" ~ value / 50.9415,
    parameter == "Zn" ~ value / 65.38,
    TRUE ~ value )) %>%
  # change values < dl to zero
  mutate(mmol = ifelse(limit_symbol == "<", 0, mmol)) %>%
  # change names to PHREEQC names
  mutate(name = case_when(
    parameter == "HCO3" ~ "Alkalinity", 
    parameter == "DO" ~ "O(0)",
    parameter == "NO3" ~ "N(5)",
    parameter == "SO4" ~ "S(6)",
    parameter == "PO4" ~ "P",
    TRUE ~ parameter )) %>%
  # add samplenr column
  group_by(samplecode) %>%
  mutate(Number = cur_group_id()) %>%
  ungroup() %>%
  select(samplecode, Number, name, mmol) %>%
  # convert to wide format
  pivot_wider(names_from = "name",
              values_from = "mmol") %>%
  # reorder columns 
  rename(Description = samplecode) %>%
  select(Description, Number, Temp, pH, Alkalinity, `O(0)`, Cl, Br, `N(5)`, `S(6)`, P, `F`,
         Na, K, Ca, Mg, Si, Fe, Mn, Al, As, Ba, B, Cd, Cu, Ni, Se, Zn, V, Cr, Mo)

# Save PHREEQC input file to copy into PHREEQC file and run 
openxlsx::write.xlsx(d, paste0(output, "Output/Phreeqc/curacao_2021_mmol.xlsx"))

###############################################################################
# Examine SI
###############################################################################

# import SI PHREEQC output file
SI <- read.csv("C:/Users/mikewit/Documents/SEALINK/Phreeqc/Curacao_SIs_v1_results.txt",
               sep = "\t")
# SI PRHEEQC output with values <dl as 0
SI2 <- read.csv("C:/Users/mikewit/Documents/SEALINK/Phreeqc/Curacao_SIs_v2_results.txt",
               sep = "\t")
  
d_SI <- SI2 %>%
  pivot_longer(cols = c(SI_Calcite:logpP_CO2.g.),
               names_to = "mineral",
               values_to = "SI") %>%
  # remove SI where one parameter is missing and values are -99
  filter(SI != -99.99000000) %>%
  dplyr::rename(samplecode = Description) %>%
  mutate(samplecode = gsub(" ", "", samplecode, fixed = T)) %>%
  mutate(mineral = gsub("SI_", "", mineral)) %>%
  mutate(formula = case_when(
    mineral == "Anhydrite" ~ "CaSO4",
    mineral == "Aragonite" ~ "CaCO3",
    mineral == "Barite" ~ "BaSO4",
    mineral == "Calcite" ~ "CaCO3",
    mineral == "Dolomite" ~ "CaMg(CO3)2",
    mineral == "FeOOH.a." ~ "FeOOH (a)",
    mineral == "Fluorapatite" ~ "Ca5(PO4)3F",
    mineral == "Fluorite" ~ "CaF2",
    mineral == "Gibsite" ~ "Al(OH)3",
    mineral == "Goethite" ~ "FeOOH",
    mineral == "Gypsum" ~ "CaSO4.2H2O",
    mineral == "Halite" ~ "NaCl",
    mineral == "Hydroxyapatite" ~ "Ca5(PO4)3(OH)",
    mineral == "Illite" ~ "K0.6Mg0.25Al2.3SI3.5O10(OH)2",
    mineral == "K.feldspar" ~ "KAlSi3O8",
    mineral == "K.mica" ~ "KAl3Si3O10(OH)2",
    mineral == "Kaolinite" ~ "Al2Si2O5(OH)4",
    mineral == "logpP_CO2.g." ~ "pCO2",
    mineral == "Magnesite" ~ "MgCO3",
    mineral == "Manganite" ~ "MnOOH",
    mineral == "Quartz" ~ "SiO2",
    mineral == "Rhodochrosite" ~ "MnCO3",
    mineral == "Siderite" ~ "FeCO3",
    mineral == "SiO2.a." ~ "SiO2 (a)",
    mineral == "Vivianite" ~ "Fe3(PO4)2:8H2O",
    mineral == "Witherite" ~ "BaCO3",
    TRUE ~ "missing" ))

# Overview of SI statistics per mineral
d_SI %>%
  group_by(mineral, formula) %>%
  dplyr::summarise(n = n(),
            `% supersat` = round(length(samplecode[SI > 0]) / n() * 100, digits = 0),
            `% undersat` = 100 - `% supersat`,
            min = min(SI),
            p10 = quantile(SI, 0.1),
            p50 = median(SI),
            avg = mean(SI),
            p90 = quantile(SI, 0.9),
            max = max(SI)) %>%
  view()
  
# Boxplot of SI
ggplot(d_SI, aes(x = mineral, y = SI)) +
  geom_boxplot() +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Saturation indices") +
  theme_bw() +
  facet_wrap(~mineral, scales = "free")

# Everything
ggplot(d_SI, aes(x = reorder(samplecode, -SI), y = SI, group = mineral, color = mineral)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(name = "Saturation indices",
                     limits = c(-10, 10)
  ) +
  scale_x_discrete(name = "") +
  annotate("text", x = 65, y = 8.5, label = "Supersaturated", size = 6) +
  annotate("text", x = 12, y = -9, label = "Subsaturated", size = 6) +
  theme_bw() +
  ggtitle("Saturation indices for carbonate minerals") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank())

# For Carbonates
set <- c("Aragonite", "Calcite", "Dolomite", "Rhodochrosite", "Siderite", "Magnesite", "Witherite")

ggplot(d_SI %>% filter(mineral %in% set), aes(x = reorder(samplecode, -SI), 
                                              y = SI, color = mineral, shape = mineral)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(name = "Saturation indices"
                     #limits = c(-10, 10)
                     ) +
  scale_x_discrete(name = "") +
  annotate("text", x = 60, y = 3, label = "Supersaturated", size = 6) +
  annotate("text", x = 10, y = -3, label = "Subsaturated", size = 6) +
  theme_bw() +
  ggtitle("Saturation indices for carbonate minerals") +
  theme(axis.ticks.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.x = element_text(angle = 90, size = 7),
        legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank())
  
# For Sulphates
set <- c("Anhydrite", "Barite", "Gypsum", "Fluorite")

ggplot(d_SI %>% filter(mineral %in% set), aes(x = reorder(samplecode, -SI), 
                                              y = SI, color = mineral, shape = mineral)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(name = "Saturation indices"
                     #limits = c(-5, 1.0)
  ) +
  scale_x_discrete(name = "") +
  annotate("text", x = 65, y = 0.7, label = "Supersaturated", size = 6) +
  annotate("text", x = 15, y = -4.5, label = "Subsaturated", size = 6) +
  theme_bw() +
  ggtitle("Saturation indices for sulphate minerals") +
  theme(axis.ticks.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.x = element_text(angle = 90, size = 7),
        legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank())

# Phosphates
set <- c("Fluorapatite", "Hydroxyapatite", "Vivianite")

ggplot(d_SI %>% filter(mineral %in% set), aes(x = reorder(samplecode, -SI), 
                                              y = SI, color = mineral, shape = mineral,
                                              label = samplecode)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(name = "Saturation indices"
                     #limits = c(-5, 1.0)
  ) +
  scale_x_discrete(name = "") +
  annotate("text", x = 45, y = 6, label = "Supersaturated", size = 6) +
  annotate("text", x = 15, y = -2.5, label = "Subsaturated", size = 6) +
  geom_text(aes(label = ifelse(SI < -5, as.character(samplecode), "")), hjust = 1.2, vjust = 0.5) +
  theme_bw() +
  ggtitle("Saturation indices for phosphate minerals") +
  theme(axis.ticks.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.x = element_text(angle = 90, size = 7),
        legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank())

# Silicates
set <- c("Quartz", "SiO2.a.", "Illite", "K.feldspar", "K.mica", "Kaolinite")

ggplot(d_SI %>% filter(mineral %in% set), aes(x = reorder(samplecode, -SI), 
                                              y = SI, color = mineral, shape = mineral,
                                              label = samplecode
                                              )) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(name = "Saturation indices"
                     #limits = c(-5, 1.0)
  ) +
  scale_x_discrete(name = "") +
  #annotate("text", x = 65, y = 0.9, label = "Supersaturated", size = 6) +
  annotate("text", x = 60, y = 10, label = "Supersaturated", size = 6) +
  annotate("text", x = 15, y = -1.3, label = "Subsaturated", size = 6) +
  #annotate("text", x = 72, y = -3.2, label = "GW021", size = 4) +
  #annotate("text", x = 73, y = -3.5, label = "GW030", size = 4) +
  geom_text(aes(label = ifelse(SI < -1, as.character(samplecode), "")), hjust = 1.2, vjust = 0.5) +
  theme_bw() +
  ggtitle("Saturation indices for silicate minerals") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank())

# Fe-, Mn-, Mg-, Al-hydroxides
## These mineral SI are strongly related to O2 levels and thus O2 measurements in the field
# As the O2 measurements were mostly carried out in open wells, these are probabaly not accurate!
set <- c("FeOOH.a.", "Fluorite", "Gibsite", "Goethite", "Manganite")

ggplot(d_SI %>% filter(mineral %in% set), aes(x = samplecode, y = SI, color = mineral, shape = mineral)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(name = "Saturation indices"
                     #limits = c(-5, 1.0)
  ) +
  scale_x_discrete(name = "") +
  annotate("text", x = 65, y = 8.5, label = "Saturated", size = 6) +
  annotate("text", x = 15, y = -4.5, label = "Subsaturated", size = 6) +
  theme_bw() +
  ggtitle("Saturion indices for hydroxide minerals") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank())

# Mn minerals
set <- c("Manganite", "Rhodochrosite")

ggplot(d_SI %>% filter(mineral %in% set), aes(x = reorder(samplecode, -SI), 
                                              y = SI, color = mineral, shape = mineral)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(name = "Saturation indices"
                     #limits = c(-5, 1.0)
  ) +
  scale_x_discrete(name = "") +
  annotate("text", x = 65, y = 1, label = "Saturated", size = 6) +
  annotate("text", x = 15, y = -4.5, label = "Subsaturated", size = 6) +
  theme_bw() +
  ggtitle("Saturation indices for Mn-minerals") +
  theme(axis.ticks.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.x = element_text(angle = 90, size = 7),
        legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank())

# Calcite vs pCO2
set <- c("Manganite", "Rhodochrosite")

ggplot(d_SI %>% filter(mineral %in% set), aes(x = reorder(samplecode, -SI), 
                                              y = SI, color = mineral, shape = mineral)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(name = "Saturation indices"
                     #limits = c(-5, 1.0)
  ) +
  scale_x_discrete(name = "") +
  annotate("text", x = 65, y = 1, label = "Saturated", size = 6) +
  annotate("text", x = 15, y = -4.5, label = "Subsaturated", size = 6) +
  theme_bw() +
  ggtitle("Saturation indices for Mn-minerals") +
  theme(axis.ticks.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.x = element_text(angle = 90, size = 7),
        legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank())

###
# Check Fe and Mn concentrations
###

d <- data %>%
  filter(year == 2021) %>%
  filter(parameter %in% c("Fe", "Mn")) %>%
  mutate(value = ifelse(units == "ug/l", value / 1000, value)) %>%
  select(samplecode, parameter, value)

d$samplecode <- factor(d$samplecode, levels=d[order(d$value), "samplecode"])

fct_reorder(myaxis, order)

ggplot(d, aes(x = reorder(samplecode, -value), y = value, color = parameter)) + 
  geom_point() +
  scale_x_discrete(name = "") +
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.direction = "horizontal", legend.position = "bottom", legend.title = element_blank())

d <- data %>%
  filter(year == 2021) %>%
  filter(parameter %in% c("Fe", "Mn")) %>%
  mutate(value = ifelse(units == "ug/l", value / 1000, value)) %>%
  select(samplecode, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value)

ggplot(d, aes(x = Fe, y = Mn)) +
  geom_point() +
  geom_abline(linetype = "dashed") +
  theme_bw()
  
###############################################################################
# Run example 2 and plot results
###############################################################################

# load the PHREEQC.dat database
phrLoadDatabaseString(phreeqc.dat)

# run example 2
phrRunString(ex2)

# retrieve selected output as a list of data.frame
so <- phrGetSelectedOutput()

# plot the results
attach(so$n1)
title <- "Gypsum-Anhydrite Stability"
xlabel <- "Temperature, in degrees Celcius"
ylabel <- "Saturation index"

plot(temp.C., si_gypsum, main = title, xlab = xlabel, ylab = ylabel,
     col = "darkred", xlim = c(25, 75), ylim = c(-0.4, 0.0))
points(temp.C., si_anhydrite, col = "darkgreen")
legend("bottomright", c("Gypsum", "Anhydrite"),
       col = c("darkred", "darkgreen"), pch = c(1, 1))












