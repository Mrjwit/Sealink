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
pacman::p_load(tidyverse, openxlsx, cowplot, scales, ggpattern)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/final_merged/" 

# load cleaned data
data <- read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx"))
metadata <- openxlsx::read.xlsx(paste0(input, "metadata_2021_2022.xlsx"))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# Editing data
###############################################################################

SO4sea <- data %>% filter(sampletype == "seawater", parameter == "SO4") %>% pull(value) %>% mean()
Clsea <- data %>% filter(sampletype == "seawater", parameter == "Cl") %>% pull(value) %>% mean()

d <- data %>%
  filter(year > 2020) %>%
  mutate(parameter = paste(parameter, units)) %>%
  select(putcode, samplecode, parameter, value, watercode, sampletype, subtype) %>%
  # put in wide format
  pivot_wider(names_from = parameter,
              values_from = value) %>%
  # calculate ratios
  mutate(`B/Cl` = (`B ug/l`/1000) / `Cl mg/l`,
         `Cl/B` = (`Cl mg/l` / (`B ug/l`/1000)),
         `Ca/Cl` = `Ca mg/l` / `Cl mg/l`,
         `SO4/Cl` = `SO4 mg/l` / `Cl mg/l`,
         `SO4 cons/prod` = `SO4 mg/l` - (`Cl mg/l` * SO4sea/Clsea))
  # pivot_longer(cols = 6:98,
  #              names_to = "parameter",
  #              values_to = "value")

# add metadata
d <- d %>%
  left_join(., metadata %>% select(samplecode, geology, geology_abr, 
                                   landuse_zonal_map, Land.use.based.on.own.observations))
###############################################################################
# Analysis
###############################################################################

## Recharge signal from desalinized drinking water
# Using stable water isotopes and B/Cl ratios
d %>%
  select(samplecode, `B ug/l`, `Cl mg/l`, `B/Cl`, `SO4/Cl`) %>%
  view()

summ <- d %>% 
  group_by(sampletype, subtype) %>%
  summarise(n = n(),
            min_B = min(`B ug/l`, na.rm = T),
            max_B = max(`B ug/l`, na.rm = T),
            avg_B = mean(`B ug/l`, na.rm = T),
            med_B = median(`B ug/l`, na.rm = T),
            avg_Cl = mean(`Cl mg/l`, na.rm = T),
            med_Cl = median(`Cl mg/l`, na.rm = T),
            `avg_B/Cl` = mean(`B/Cl`, na.rm = T),
            `med_B/Cl` = median(`B/Cl`, na.rm = T),
            `avg_Cl/B` = mean(`Cl/B`, na.rm = T),
            `med_Cl/B` = median(`Cl/B`, na.rm = T))

p1 <- ggplot(data = d, aes(x = `B/Cl`, y = fct_rev(sampletype))) +
  geom_boxplot(fill = "lightgrey") +
  labs(y = "") +
  theme_bw() 

p2 <- ggplot(data = d, aes(x = `B/Cl`, y = subtype)) +
  geom_boxplot() +
  theme_bw() 

sl <- ((summ[6,8]-summ[1,8])/(summ[6,6]-summ[1,6]))
intc <- summ[6,8] - summ[6,6] * sl

p3 <- ggplot(data = d %>% filter(!sampletype %in% c("rainwater", "seawater")), 
             aes(x = `Cl mg/l`, y = `B/Cl`)) +
  geom_point(aes(color = sampletype)) +
  geom_smooth(formula = y ~ x) +
  geom_abline(intercept = intc$`med_B/Cl`, slope = sl$`med_B/Cl`, linetype = "dashed") +
  coord_cartesian(xlim = c(0, 1000)) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.65))

cowplot::plot_grid(p1, p3, ncol = 1, labels = "AUTO")

# Ca/Cl 
p1 <- ggplot(data = d, aes(x = `Ca/Cl`, y = fct_rev(sampletype))) +
  geom_boxplot(fill = "lightgrey") +
  labs(y = "") +
  theme_bw() 

p3 <- ggplot(data = d, 
             aes(x = `Cl mg/l`, y = `Ca/Cl`)) +
  geom_point(aes(color = sampletype)) +
  geom_smooth(formula = y ~ x) +
  #geom_abline(intercept = intc$`med_B/Cl`, slope = sl$`med_B/Cl`, linetype = "dashed") +
  coord_cartesian(xlim = c(0, 1000)) +
  theme_bw() 
  #theme(legend.position = c(0.9, 0.65))

cowplot::plot_grid(p1, p3, ncol = 1, labels = "AUTO")

# SO4/Cl --> Lowest SO4/Cl ratio for RO drinking water
p1 <- ggplot(data = d, aes(x = `SO4/Cl`, y = fct_rev(sampletype))) +
  geom_boxplot(fill = "lightgrey") +
  labs(y = "") +
  theme_bw() 

p3 <- ggplot(data = d, 
             aes(x = `Cl mg/l`, y = `SO4/Cl`)) +
  geom_point(aes(color = sampletype)) +
  geom_smooth(formula = y ~ x) +
  #geom_abline(intercept = intc$`med_B/Cl`, slope = sl$`med_B/Cl`, linetype = "dashed") +
  coord_cartesian(xlim = c(0, 1000)) +
  theme_bw() 
#theme(legend.position = c(0.9, 0.65))

cowplot::plot_grid(p1, p3, ncol = 1, labels = "AUTO")

p1 <- ggplot(data = d %>% filter(sampletype == "groundwater"), 
       aes(x = `Cl mg/l`, y = `SO4 cons/prod`)) +
  geom_point(aes(colour = geology), alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "SO4 consumed(-) / produced(+) mg/L",
       title = "Groundwater sulphate consumption/production") +
  theme_bw() 

p2 <- ggplot(data = d %>% filter(sampletype == "groundwater"), 
             aes(x = `Cl mg/l`, y = `SO4 cons/prod`)) +
  geom_point(aes(colour = geology), alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_cartesian(xlim = c(0, 4000), ylim = c(-500, 1500)) +
  labs(y = "SO4 consumed(-)/produced(+) mg/L",
       title = "Groundwater sulphate consumption/production") +
  theme_bw() 

ggplot(d %>% filter(sampletype == "groundwater"),
             aes(x = `SO4 cons/prod`, y = geology)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_boxplot(fill = "lightgrey") +
  theme_bw()

cowplot::plot_grid(p1, p2, ncol = 1, labels = "AUTO")

## Recharge signal from rainwater 
# Using stable water isotopes
