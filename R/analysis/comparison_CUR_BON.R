#
# Input: groundwater quality data Curacao + Bonaire
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
pacman::p_load(tidyverse, openxlsx, cowplot, scales, ggh4x, ggpubr)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/final_merged/" 
input_B <- "C:/Users/mikewit/Documents/SEALINK/Start-up Materials/Hydrochemistry Data ABC Islands/"

# load cleaned data Curacao
data <- read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx"))
metadata <- openxlsx::read.xlsx(paste0(input, "metadata_2021_2022.xlsx"))

## load data Bonaire 
# 2005
bon_2005 <- read.xlsx(paste0(input_B, "Bonaire/Grondwater data Bonaire Borst en de Haas, 2005.xlsx")) %>%
  mutate(nr = paste0("n=57"))

# 2024
bon_2024 <- read.xlsx(paste0("C:/Users/mikewit/Documents/SEALINK/Data/",
                             "Raw_data/Bonaire/Metadata_samples_Bonaire_Jan_2024.xlsx"),
                      sheet = "grondwaterkwaliteit") %>%
  filter(str_detect(samplecode, "BGW")) %>%
  mutate(NO3 = as.numeric(gsub("<", "", NO3))) %>%
  mutate(PO4 = as.numeric(gsub("<", "", PO4))) %>%
  mutate(nr = paste0("n=9"))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/Output/Figures/Bonaire/"

###############################################################################
# Editing data
###############################################################################

# Boxplots Bonaire (2005) + 1992 ??
p1 <- ggplot(bon_2005, aes(x = nr, y = `Nitrate.(mg/l)`)) +
  geom_boxplot(fill = "lightgrey") +
  scale_x_discrete("") +
  theme_bw()

p2 <- ggplot(bon_2005, aes(x = nr, y = `Nitrate.(mg/l)`)) +
  geom_boxplot(fill = "lightgrey") +
  coord_cartesian(ylim = c(0, 50)) +
  scale_x_discrete("") +
  theme_bw()

plt <- ggpubr::ggarrange(p1, p2, labels = "AUTO")
annotate_figure(plt, top = ggpubr::text_grob("Groundwater nitrate concentrations Bonaire, 2005 (Borst & de Haas)", 
                                      color = "black", face = "bold", size = 12))
ggsave(paste0(output, "Bonaire_groundwater_NO3_2005.png"))

# Boxplots Curacao (1992 + 2020-2023) 
d <- data %>%
  filter(subtype == "groundwater", parameter %in% c("NO3", "NH4", "PO4"))

p1 <- ggplot(d %>% filter(parameter == "NO3"), aes(x = as.factor(year), y = value)) +
  geom_boxplot(fill = "lightgrey") +
  scale_x_discrete("") +
  scale_y_continuous("NO3 (mg/L)") +
  theme_bw()

p2 <- ggplot(d %>% filter(parameter == "NO3"), aes(x = as.factor(year), y = value)) +
  geom_boxplot(fill = "lightgrey") +
  coord_cartesian(ylim = c(0, 200)) +
  scale_x_discrete("") +
  scale_y_continuous("") +
  theme_bw()

plt <- ggpubr::ggarrange(p1, p2, labels = "AUTO")
annotate_figure(plt, top = ggpubr::text_grob("Groundwater nitrate concentrations Curacao, 1992-2023", 
                                             color = "black", face = "bold", size = 12))
ggsave(paste0("C:/Users/mikewit/Documents/SEALINK/Data/Output/Figures/Hydrochemistry/Nutrients/",
              "NO3_boxplots_groundwater_1992-2023.png"))

# Curacao 2020-2023 grouped
d <- data %>%
  filter(subtype == "groundwater", parameter %in% c("NO3", "NH4", "PO4")) %>%
  mutate(year = case_when(
    year %in% c(2020, 2021, 2022, 2023) ~ "2020-2023",
    TRUE ~ as.character(year)))

p1 <- ggplot(d %>% filter(parameter == "NO3"), aes(x = as.factor(year), y = value)) +
  geom_boxplot(fill = "lightgrey") +
  scale_x_discrete("") +
  scale_y_continuous("NO3 (mg/L)") +
  theme_bw()

p2 <- ggplot(d %>% filter(parameter == "NO3"), aes(x = as.factor(year), y = value)) +
  geom_boxplot(fill = "lightgrey") +
  coord_cartesian(ylim = c(0, 250)) +
  scale_x_discrete("") +
  scale_y_continuous("") +
  theme_bw()

plt <- ggpubr::ggarrange(p1, p2, labels = "AUTO")
annotate_figure(plt, top = ggpubr::text_grob("Groundwater nitrate concentrations Curacao, 1992-2023", 
                                             color = "black", face = "bold", size = 12))
ggsave(paste0("C:/Users/mikewit/Documents/SEALINK/Data/Output/Figures/Hydrochemistry/Nutrients/",
              "NO3_boxplots_groundwater_1992-2020-2023.png"))

## Combine Bonaire and Curacao datasets to compare NO3 in groundwater
bon05 <- bon_2005 %>%
  rename(samplecode = No,
         NO3 = `Nitrate.(mg/l)`) %>%
  mutate(EC = as.numeric(`ECtop.(µS/cm)`)) %>%
  select(samplecode, NO3, EC, pH) %>%
  pivot_longer(cols = NO3:pH,
               names_to = "parameter",
               values_to = "value") %>%
  mutate(year = paste0(2005, "\n", "n=57"),
         island = "Bonaire")

bon24 <- bon_2024 %>%
  mutate(EC = as.numeric(EC)) %>%
  select(samplecode, NO3, EC, PO4) %>%
  pivot_longer(cols = NO3:PO4,
               names_to = "parameter",
               values_to = "value") %>%
  mutate(year = paste0(2024, "\n", "n=9"),
         island = "Bonaire")

cur <- data %>%
  filter(subtype == "groundwater", parameter %in% c("EC", "NO3", "NH4", "PO4")) %>%
  mutate(year = case_when(
    year %in% c(2020, 2021, 2022, 2023) ~ paste0("2020-2023", "\n", "n=265"),
    year == 1992 ~ paste0(1992, "\n", "n=97"),
    TRUE ~ as.character(year)),
    island = "Curacao") %>%
  select(samplecode, year, parameter, value, island)

d <- rbind(bon05, bon24, cur) %>%
# add median NO3 value for Bonaire in 1995
  rbind(., tibble(samplecode = "GWB_1995",
                      parameter = c("EC", "NO3"),
                      value = c(4.3, 4.6),
                      year = paste0(1995, "\n", "n=98*"),
                      island = "Bonaire")) %>%
  mutate(value = case_when(
    parameter == "EC" & island == "Bonaire" ~ value * 1000,
    TRUE ~ value))

# plot boxplots
p1 <- ggplot(d %>% filter(parameter == "NO3"), 
       aes(x = year, y = value, fill = island)) +
  geom_boxplot() +
  scale_x_discrete("") +
  scale_y_continuous("NO3 (mg/L)") +
  theme_bw()

p2 <- ggplot(d %>% filter(parameter == "NO3"), 
             aes(x = year, y = value, fill = island)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_discrete("") +
  scale_y_continuous("") +
  theme_bw()

plt <- ggpubr::ggarrange(p1, p2, labels = "AUTO", common.legend = T, legend = "bottom")
annotate_figure(plt, top = ggpubr::text_grob("Groundwater nitrate concentrations Bonaire and Curacao", 
                                             color = "black", face = "bold", size = 12))
ggsave(paste0(output, "Bonaire_vs_Curacao_groundwater_NO3.png"),
       width = 8, height = 6)

# EC
p1 <- ggplot(d %>% filter(parameter == "EC"), 
             aes(x = year, y = value, fill = island)) +
  geom_boxplot() +
  scale_x_discrete("") +
  scale_y_continuous("EC (uS/cm)") +
  theme_bw()

p2 <- ggplot(d %>% filter(parameter == "EC"), 
             aes(x = year, y = value, fill = island)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 15000)) +
  scale_x_discrete("") +
  scale_y_continuous("") +
  theme_bw()

plt <- ggpubr::ggarrange(p1, p2, labels = "AUTO", common.legend = T, legend = "bottom")
annotate_figure(plt, top = ggpubr::text_grob("Groundwater salinity Bonaire and Curacao", 
                                             color = "black", face = "bold", size = 12))
ggsave(paste0(output, "Bonaire_vs_Curacao_groundwater_EC.png"),
       width = 8, height = 6)

d %>% filter(parameter == "NO3", !is.na(value)) %>%
  group_by(island, year) %>%
  summarise(n = n(),
            min = min(value, na.rm = T),
            med = median(value, na.rm = T),
            avg = mean(value, na.rm = T),
            max = max(value, na.rm = T),
            sd = sd(value, na.rm = T))

## test of significance ##
# boxplot comparing param by year
my_comparisons <- list(c("2005\nn=57", "2020-2023\nn=265"))

# ggplot(d %>% mutate(myaxis = paste0(parameter, "\n", num)), 
#        aes(x = myaxis, y = value)) +

# Whole island
p1 <- ggplot(d %>% filter(parameter == "NO3",
                            year %in% c("2005\nn=57", "2020-2023\nn=265")), 
             aes(x = year, y = value, fill = island)) +
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot(width = 0.6) +
  scale_x_discrete("") +
  scale_y_continuous("NO3 (mg/L)") +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test") +
  theme_bw() +
  theme(legend.position = "bottom") 

p2 <- ggplot(d %>% filter(parameter == "NO3",
                    year %in% c("2005\nn=57", "2020-2023\nn=265")), 
       aes(x = year, y = value, fill = island)) +
  stat_boxplot(geom = "errorbar", width = 0.1) +
  geom_boxplot(width = 0.6) +
  scale_x_discrete("") +
  scale_y_continuous("NO3 (mg/L)") +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test") +
  coord_cartesian(ylim = c(0, 220)) +
  theme_bw() 

plt <- ggpubr::ggarrange(p1, p2, labels = "AUTO", common.legend = T, legend = "bottom")
annotate_figure(plt, top = ggpubr::text_grob("Groundwater nitrate concentrations Bonaire and Curacao", 
                                             color = "black", face = "bold", size = 12))
ggsave(paste0(output, "Bon_vs_Cur_groundwater_NO3_wilcox_test.png"),
       width = 8, height = 6)

gapminder %>%
  filter(continent %in% c("Africa", "Europe")) %>%
  t.test(lifeExp ~ continent, data = .,
         alternative = "two.sided")

d %>%
  filter(parameter == "NO3",
         year %in% c("2005\nn=57", "2020-2023\nn=265")) %>%
  t.test(value ~ island, data = .,
         alternative = "two.sided")

d %>%
  filter(parameter == "NO3",
         year %in% c("2005\nn=57", "2020-2023\nn=265")) %>%
  wilcox.test(value ~ island, data = .,
              alternative = "two.sided")

