#
# Input: climate data
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
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/rainfall_and_temp/" 

# load cleaned data
rain <- openxlsx::read.xlsx(paste0(input, "Temperature-and-rainfall-information-2020.xlsx"),
                            sheet = "climexp.knmi")
rain_hist <- openxlsx::read.xlsx(paste0(input, "Temperature-and-rainfall-information-2020.xlsx"),
                                 sheet = "before 1951", rows = 1:845)
clim <- openxlsx::read.xlsx(paste0(input, "Temperature-and-rainfall-information-2020.xlsx"),
                            sheet = "Climatology", rows = 2:27)

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# Editing data
###############################################################################

rain %>% filter(Year > 1970)  %>%
  summarise(min_P = min(Total, na.rm = T),
            p_5 = quantile(Total, 0.05, na.rm = T),
            p_10 = quantile(Total, 0.1, na.rm = T),
            p50 = median(Total, na.rm = T),
            avg = mean(Total, na.rm = T),
            p90 = quantile(Total, 0.9, na.rm = T),
            p95 = quantile(Total, 0.95, na.rm = T),
            max = max(Total, na.rm = T))

rain <- rain %>%
  mutate(`rainfall stations` = ifelse(Year < 1951, "island station average", "Hato airport"),
         dataset = ifelse(Year %in% c(1993, 1994, 1998, 2023), "incomplete", "complete"),
         sampling = ifelse(Year %in% c(1976, 1977, 1991, 1992, 2020, 2021, 2022), "groundwater sampling", "no sampling"))

# climate in long format for plotting
clim <- clim %>%
  mutate(ELEMENT = paste(ELEMENT, X2)) %>%
  select(-X2) %>%
  mutate(JAN = as.numeric(JAN)) %>%
  pivot_longer(cols = JAN:AVG,
               names_to = "month",
               values_to = "value") %>%
  pivot_wider(names_from = ELEMENT,
              values_from = value) %>%
  mutate(month = factor(month,
                        levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", "AVG"))) %>%
  mutate(`Av. Monthly evaporation mm` = case_when(
    month == "FEB" ~ `Av. Daily evaporation mm` * 28,
    month %in% c("APR", "JUN", "SEP", "NOV") ~ `Av. Daily evaporation mm` * 30,
    month %in% c("JAN", "MAR", "MAY", "JUL", "AUG", "OCT", "DEC") ~ `Av. Daily evaporation mm` * 31,
    TRUE ~ `Av. Daily evaporation mm` * 365))

###############################################################################
# Creating plots
###############################################################################

# create plot theme
theme_mw <- function(){
  font <- "Georgia"
  theme_bw() %+replace% # use theme_bw as base and replace elements 
  
    theme(
      #text elements
      plot.title = element_text(
        family = font,
        size = 14,
        face = 'bold',
        hjust = 0,
        vjust = 2),
      plot.subtitle = element_text(
        family = font,
        size = 12,
        hjust = 0,
        vjust = 2),
      axis.title = element_text(
        family = font,
        size = 10),
      axis.text = element_text(
        family = font,
        size = 9)
    )
}

# A.Climatogram Curacao period 1981 - 2010
ylim.prim <- c(0, 125) # axis limits for precipitation
ylim.sec <- c(20, 35) # axis limits for temperature

# makes the necessary scaling calculations based on above limits for dual axis
b1 <- diff(ylim.prim)/diff(ylim.sec)
a1 <- ylim.prim[1] - b1*ylim.sec[1]

p1 <- ggplot(data = clim %>% filter(month != "AVG")) +
  geom_col(aes(x = month, y = `Av. Montly rainfall mm`), fill = "steelblue", colour = "black") +
  geom_line(aes(x = month, y = a1 + b1 * `Av. Air temperature °C`, group = 1), colour = "orange2", size = 1) +
  geom_line(aes(x = month, y = a1 + b1 * `Av. Maximum temperature °C`, group = 1), colour = "orange2", size = 0.7, linetype = "dashed") +
  geom_line(aes(x = month, y = a1 + b1 * `Av. Minimum temperature °C`, group = 1), colour = "orange2", size = 0.7, linetype = "dashed") +
  scale_y_continuous("Av. monthly precipitation [mm]", sec.axis = sec_axis(~ (. - a1)/b1, name  = "Av. air temperature [°C]")) +
  labs(x = "", 
       title = "Climatological data Curacao",
       subtitle = "period 1981-2010") +
  theme_mw() +
  theme(axis.line.y.left = element_line(color = "steelblue"), 
        axis.ticks.y.left = element_line(color = "steelblue"),
        axis.text.y.left = element_text(color = "steelblue"), 
        axis.title.y.left = element_text(color = "steelblue"),
        axis.line.y.right = element_line(color = "orange2"), 
        axis.ticks.y.right = element_line(color = "orange2"),
        axis.text.y.right = element_text(color = "orange2"), 
        axis.title.y.right = element_text(color = "orange2"))

# B.Precipitation and evaporation
ylim.prim <- c(0, 125) # axis limits for precipitation
ylim.sec <- c(0, 10) # axis limits for evaporation

# makes the necessary scaling calculations based on above limits for dual axis
b2 <- diff(ylim.prim)/diff(ylim.sec)
a2 <- ylim.prim[1] - b2*ylim.sec[1]

p2 <- ggplot(data = clim %>% filter(month != "AVG")) +
  geom_col(aes(x = month, y = `Av. Montly rainfall mm`), fill = "steelblue", colour = "black") +
  geom_line(aes(x = month, y = a2 + b2 * `Av. Daily evaporation mm`, group = 1), colour = "firebrick", size = 1) +
  scale_y_continuous("Av. monthly precipitation [mm]", sec.axis = sec_axis(~ (. - a2)/b2, name  = "Av. daily evaporation [mm]")) +
  labs(x = "", 
       title = "Climatological data Curacao",
       subtitle = "period 1981-2010") +
  theme_mw() +
  theme(axis.line.y.left = element_line(color = "steelblue"), 
        axis.ticks.y.left = element_line(color = "steelblue"),
        axis.text.y.left = element_text(color = "steelblue"), 
        axis.title.y.left = element_text(color = "steelblue"),
        axis.line.y.right = element_line(color = "firebrick"), 
        axis.ticks.y.right = element_line(color = "firebrick"),
        axis.text.y.right = element_text(color = "firebrick"), 
        axis.title.y.right = element_text(color = "firebrick"))

# just P and E
p2 <- ggplot(data = clim %>% filter(month != "AVG")) +
  geom_area(aes(x = month, y = `Av. Monthly evaporation mm`, group = 1), fill = "firebrick", size = 1, alpha = 0.4) +
  geom_line(aes(x = month, y = `Av. Monthly evaporation mm`, group = 1), colour = "firebrick", size = 1) +
  geom_area(aes(x = month, y = `Av. Montly rainfall mm`, group = 1), fill = "steelblue", alpha = 0.7) +
  geom_line(aes(x = month, y = `Av. Montly rainfall mm`, group = 1), colour = "steelblue", size = 1) +
  labs(x = "", y = "Av. monthly precipitation and evaporation [mm]",
       title = "Water balance Curacao",
       subtitle = "period 1981-2010") +
  theme_mw() 

# P, E and Temp
ylim.prim <- c(0, 250) # axis limits for P + E
ylim.sec <- c(20, 35) # axis limits for temperature

# makes the necessary scaling calculations based on above limits for dual axis
b1 <- diff(ylim.prim)/diff(ylim.sec)
a1 <- ylim.prim[1] - b1*ylim.sec[1]

ggplot(data = clim %>% filter(month != "AVG")) +
  geom_col(aes(x = month, y = `Av. Montly rainfall mm`), fill = "steelblue", colour = "black") +
  geom_line(aes(x = month, y = `Av. Monthly evaporation mm`, group = 1), colour = "firebrick", size = 1) +
  geom_line(aes(x = month, y = a1 + b1 * `Av. Air temperature °C`, group = 1), colour = "orange2", size = 1) +
  geom_line(aes(x = month, y = a1 + b1 * `Av. Maximum temperature °C`, group = 1), colour = "orange2", size = 0.7, linetype = "dashed") +
  geom_line(aes(x = month, y = a1 + b1 * `Av. Minimum temperature °C`, group = 1), colour = "orange2", size = 0.7, linetype = "dashed") +
  scale_y_continuous("Av. monthly precipitation and evaporation [mm]", sec.axis = sec_axis(~ (. - a1)/b1, name  = "Av. air temperature [°C]")) +
  labs(x = "", 
       title = "Climatological data Curacao",
       subtitle = "period 1981-2010") +
  theme_mw() 

# C.long term precipitation trend

p3 <- ggplot(data = rain) +
  geom_col_pattern(aes(x = Year, y = Total, fill = `rainfall stations`, pattern = dataset),
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.01,
                   pattern_key_scale_factor = 0.6) +
  geom_line(aes(x = Year, y = `30-year.average.reference.period`, colour = "aforegoing 30-year average"), size = 1.1) +
  # plot arrows above columns to indicate sampling years
  geom_segment(aes(x = 1976, y = 480, xend = 1976, yend = 430), 
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = 1977, y = 335, xend = 1977, yend = 285), 
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = 1991, y = 461, xend = 1991, yend = 411), 
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = 1992, y = 570, xend = 1992, yend = 520), 
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = 2021, y = 690, xend = 2021, yend = 640), 
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = 2022, y = 837, xend = 2022, yend = 787), 
               arrow = arrow(length = unit(0.2, "cm"))) +
  # plot arrows below columns
  # geom_segment(aes(x = 1976.5, y = -60, xend = 1976.5, yend = -10), 
  #              arrow = arrow(length = unit(0.2, "cm"))) +
  # geom_segment(aes(x = 1991.5, y = -60, xend = 1991.5, yend = -10), 
  #              arrow = arrow(length = unit(0.2, "cm"))) +
  # geom_segment(aes(x = 2021, y = -60, xend = 2021, yend = -10), 
  #              arrow = arrow(length = unit(0.2, "cm"))) +
  scale_fill_manual(values = c("steelblue", "forestgreen")) +
  scale_pattern_manual(values = c(incomplete = "stripe", complete = "none")) +
  scale_colour_manual(name = "", values = c("#F8766D"), 
                      breaks = c("aforegoing 30-year average")) + 
  labs(x = "", y = "Precipitation [mm]",
       title = "Annual rainfall Curacao",
       subtitle = "with indicated groundwater monitoring campaigns") +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  theme_mw() +
  theme(axis.line.y.left = element_line(color = "black"), 
        axis.ticks.y.left = element_line(color = "black"))

# 3.1 groundwater monitoring campaigns colored
ggplot(data = rain %>% filter(Year > 1970)) +
  geom_col_pattern(aes(x = Year, y = Total, fill = sampling, pattern = dataset),
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.01,
                   pattern_key_scale_factor = 0.6) +
  geom_line(aes(x = Year, y = `30-year.average.reference.period`, colour = "aforegoing static average"), size = 1.1) +
  geom_line(aes(x = Year, y = `30-year.moving.average.adjusted`, colour = "moving average"), size = 1.1) +
  scale_fill_manual(values = c("forestgreen", "steelblue")) +
  scale_pattern_manual(values = c(incomplete = "stripe", complete = "none")) +
  scale_colour_manual(name = "30-year average", values = c("gold", "#F8766D"), 
                      breaks = c("moving average", "aforegoing static average")) + 
  labs(x = "", y = "Precipitation [mm]",
       title = "Annual and long-term average rainfall Curacao",
       subtitle = "with highlighted groundwater monitoring campaigns") +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  theme_mw() +
  theme(axis.line.y.left = element_line(color = "black"), 
        axis.ticks.y.left = element_line(color = "black"))
ggsave(paste0(output, "Output/Figures/Rainfall/rainfall_groundwater_sampling.png"))

#cowplot::plot_grid(p1, p2, p3, labels = "AUTO")

cowplot::plot_grid(plot_grid(p1, p2, cols = 2, labels = c("A", "B")), 
                   #NULL, 
                   plot_grid(p3, labels = "C"), nrow = 2) #rel_heights = c(1, -0.1, 1)
ggsave(paste0(output, "Output/Figures/Rainfall/climate_Curacao.png"),
       width = 12, height = 10)

## Precipitation
# historic data (< 1951)
rain_hist %>%
  group_by(Year) %>%
  summarise(nr_stations = n(),
            Total_all = mean(Total, na.rm = T),
            min_P = ifelse(is.na(mean(Total, na.rm = T)), NA, min(Total, na.rm = T)),
            max_P = ifelse(is.na(mean(Total, na.rm = T)), NA, max(Total, na.rm = T)),
            sd = sd(Total, na.rm = T)) %>%
  ggplot(.) +
  geom_col(aes(x = Year, y = Total_all, fill = nr_stations), colour = "black") +
  geom_errorbar(aes(x = Year, ymin = Total_all - sd, ymax = Total_all + sd),
                width = 0.8, colour = "darkgrey",
                position = position_dodge(0.9)) +
  geom_smooth(aes(x = Year, y = Total_all)) +
  # geom_line(aes(x = Year, y = min_P), colour = "darkgrey", linetype = "dashed") +
  # geom_line(aes(x = Year, y = max_P), colour = "darkgrey", linetype = "dashed") +
  scale_fill_continuous(name = "rain stations") +
  labs(x = "", y = "Precipitation [mm]",
       title = "Annual average rainfall ") +
  theme_mw()
ggsave(paste0(output, "Output/Figures/Rainfall/rainfall_Curacao_before_1950.png"),
       width = 12, height = 10)

# long term trend
rain_hist %>%
  group_by(Year) %>%
  summarise(nr_stations = n(),
            Total_all = mean(Total, na.rm = T),
            min_P = ifelse(is.na(mean(Total, na.rm = T)), NA, min(Total, na.rm = T)),
            max_P = ifelse(is.na(mean(Total, na.rm = T)), NA, max(Total, na.rm = T)),
            sd = sd(Total, na.rm = T)) %>%
  rbind(., rain %>% filter(Year > 1950) %>%
          select(Year, Total) %>%
          mutate(nr_stations = 1,
                 Total_all = Total,
                 min_P = NA,
                 max_P = NA,
                 sd = NA) %>%
          select(-Total)) %>%
  mutate(`10yearmean` = zoo::rollmeanr(Total_all, 10, fill = NA),
         `20yearmean` = zoo::rollmeanr(Total_all, 20, fill = NA),
         `30yearmean` = zoo::rollmeanr(Total_all, 30, fill = NA)) %>%
  ggplot(.) +
  geom_col(aes(x = Year, y = Total_all, fill = nr_stations), colour = "black") +
  geom_errorbar(aes(x = Year, ymin = Total_all - sd, ymax = Total_all + sd),
                width = 0.8, colour = "darkgrey",
                position = position_dodge(0.9)) +
  geom_smooth(aes(x = Year, y = Total_all)) +
  geom_line(aes(x = Year, y = `10yearmean`, colour = "10-year")) +
  geom_line(aes(x = Year, y = `20yearmean`, colour = "20-year")) +
  geom_line(aes(x = Year, y = `30yearmean`, colour = "30-year")) +
  # geom_line(aes(x = Year, y = min_P), colour = "darkgrey", linetype = "dashed") +
  # geom_line(aes(x = Year, y = max_P), colour = "darkgrey", linetype = "dashed") +
  scale_fill_continuous(name = "rain stations") +
  scale_colour_manual(name = "long term average", values = c("forestgreen", "gold3", "#F8766D"), 
                      breaks = c("10-year", "20-year", "30-year")) +
  #scale_colour_continuous(name = "long term average") +
  labs(x = "", y = "Precipitation [mm]",
       title = "Historic annual average rainfall Curacao",
       subtitle = "Rainfall before 1950 (E. Houtepen) is island-average and after 1950 is from Hato airport only") +
  theme_mw() 
ggsave(paste0(output, "Output/Figures/Rainfall/rainfall_Curacao_1830-2022.png"),
       width = 12, height = 10)

# long term trend only Hato Airport
rain_hist %>%
  filter(Location == "Hato") %>%
  select(Year, Location, Total) %>%
  rbind(., rain %>% filter(Year > 1950) %>%
          select(Year, Total) %>%
          mutate(Location = "Hato")) %>%
  mutate(`10yearmean` = zoo::rollmeanr(Total, 10, fill = NA),
         `20yearmean` = zoo::rollmeanr(Total, 20, fill = NA),
         `30yearmean` = zoo::rollmeanr(Total, 30, fill = NA)) %>%
  ggplot(.) +
  geom_col(aes(x = Year, y = Total), fill = "steelblue",colour = "black") +
  geom_smooth(aes(x = Year, y = Total)) +
  geom_line(aes(x = Year, y = `10yearmean`, colour = "10-year")) +
  geom_line(aes(x = Year, y = `20yearmean`, colour = "20-year")) +
  geom_line(aes(x = Year, y = `30yearmean`, colour = "30-year")) +
  scale_colour_manual(name = "long term average", values = c("forestgreen", "gold3", "#F8766D"), 
                      breaks = c("10-year", "20-year", "30-year")) +
  labs(x = "", y = "Precipitation [mm]",
       title = "Historic annual average rainfall Curacao",
       subtitle = "From Hato airport only") +
  theme_mw() 

# Rainfall differences between locations
  
  
# long term trend
# Add nr of included rainfall stations for <1950 ##
ggplot(data = rain) +
  geom_col_pattern(aes(x = Year, y = Total, fill = `rainfall stations`, pattern = dataset),
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.01,
                   pattern_key_scale_factor = 0.6) +
  geom_line(aes(x = Year, y = `30-year.average.reference.period`, colour = "aforegoing average"), size = 1.1) +
  geom_line(aes(x = Year, y = `30-year.moving.average.adjusted`, colour = "moving average"), size = 1.1) +
  geom_smooth(aes(x = Year, y = Total), method = "lm") +
  scale_fill_manual(values = c("steelblue", "forestgreen")) +
  scale_pattern_manual(values = c(incomplete = "stripe", complete = "none")) +
  scale_colour_manual(name = "30-year average", values = c("gold", "#F8766D"), 
                      breaks = c("moving average", "aforegoing average")) + 
  labs(x = "", y = "Precipitation [mm]",
       title = "Annual rainfall Curacao",
       subtitle = "with indicated groundwater monitoring campaigns") +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  theme_mw() +
  theme(axis.line.y.left = element_line(color = "black"), 
        axis.ticks.y.left = element_line(color = "black"))

# monthly rainfall + duration in hours





