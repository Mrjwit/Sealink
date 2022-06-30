#
# Input: all cleaned datafiles
#         
# Output: Boxplots and statistics
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 11-04-2022
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, ggmap, xlsx, scales,
               sf, leaflet, data.table, cowplot, data.table)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/" 

# load cleaned data
data <- openxlsx::read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx"))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/Output/Statistics/" 

###############################################################################
# Data analysis
###############################################################################

#### Tables ####
# Total overview all parameters and including values < dl
d1 <- data %>%
  filter(year == 2021,
         !is.na(value)) %>%
  mutate(parameter = paste0(parameter, " [", units, "]")) %>%
  group_by(parameter) %>%
  summarise(n=n(),
            '% <dl' = round(length(value[limit_symbol == "<"]) / length(value) * 100, digits = 1), 
            min = min(value, na.rm = T),
            p10 = quantile(value, 0.1, na.rm = T),
            p50 = quantile(value, 0.5, na.rm = T),
            avg = mean(value, na.rm = T),
            p90 = quantile(value, 0.9, na.rm = T),
            max = max(value, na.rm = T),
            dl = paste(unique(detection_limit), collapse = ", "),
            method = paste(unique(method), collapse = ", ")) 

#xlsx::write.xlsx(d1, file = paste0(output, "statistics_all_parameters.xlsx"), sheetName = "TOTAL")

d2 <- data %>%
  filter(year == 2021,
         !is.na(value),
         limit_symbol != "<") %>%
  mutate(parameter = paste0(parameter, " [", units, "]")) %>%
  group_by(parameter) %>%
  summarise(n=n(),
            '% <dl' = round(length(value[limit_symbol == "<"]) / length(value) * 100, digits = 1), 
            min = min(value, na.rm = T),
            p10 = quantile(value, 0.1, na.rm = T),
            p50 = quantile(value, 0.5, na.rm = T),
            avg = mean(value, na.rm = T),
            p90 = quantile(value, 0.9, na.rm = T),
            max = max(value, na.rm = T),
            dl = paste(unique(detection_limit), collapse = ", "),
            method = paste(unique(method), collapse = ", ")) 

#xlsx::write.xlsx(d2, file = paste0(output, "statistics_all_parameters.xlsx"), sheetName = "> dl", append = T)

#### Boxplots ####

d <- data %>%
  filter(year == 2021, !is.na(value))

# add geology and land use
d <- d %>%
  left_join(., metadata %>% select(samplecode, geology, geology_abr, 
                                   landuse_zonal_map, Land.use.based.on.own.observations))

## per individual element






## boxplots log-scale

# Field measurements
sample_size <- d %>%
  group_by(parameter, method) %>% summarise(num = n())
d <- d %>%
  left_join(sample_size) %>%
  mutate(myaxis = factor(paste0("n=", num)))

ggplot(d %>% filter(parameter %in% c("pH", "Eh", "EC_uS", "Temp", "DO", "DO_sat", "E.coli", "HCO3_field")) %>%
         mutate(parameter = paste0(parameter, " [", units, "]")), 
       aes(x = myaxis, y = value)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot(fill = "lightgrey", width = 0.6) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", trans = "log10") +
  theme_bw() +
  facet_wrap(~ parameter, scales = "free")
#ggsave(paste0(output, "figures/boxplot_fieldparameters.png"))

# parameters in mg/l
d <- data %>%
  filter(year == 2021, !is.na(value))
sample_size <- d %>%
  filter(units == "mg/l",
         !parameter %in% c("HCO3_field", "HCO3_lab", "NO2_field", "NO3_field")) %>%
  group_by(parameter) %>% 
  dplyr::summarise(num = n())
d <- d %>%
  left_join(sample_size) %>%
  mutate(myaxis = factor(paste0("n=", num)))

ggplot(d %>% filter(units == "mg/l", 
                    !parameter %in% c("HCO3_field", "HCO3_lab", "NO2_field", "NO3_field")) %>%
         mutate(myaxis = paste0(parameter, "\n", num)), 
       aes(x = myaxis, y = value)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot(fill = "lightgrey", width = 0.6) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "[mg/L]", trans = 'log10',
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)),
                     sec.axis = sec_axis(~.,
                                         labels = trans_format("log10", math_format(10^.x)))) +
  theme_bw() + 
  annotation_logticks(sides = "lr")
#ggsave(paste0(output, "figures/boxplot_parameters_mgl.png"))

# parameters in ug/l
d <- data %>%
  filter(year == 2021, !is.na(value),
         sampletype == "groundwater")
sample_size <- d %>%
  filter(units == "ug/l") %>%
  group_by(parameter) %>% 
  dplyr::summarise(num = n())
d <- d %>%
  left_join(sample_size) %>%
  mutate(myaxis = factor(paste0("n=", num)))
ggplot(d %>% filter(units == "ug/l") %>%
         mutate(myaxis = paste0(parameter, "\n", num)), 
       aes(x = myaxis, y = value)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot(fill = "lightgrey", width = 0.6) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "[ug/L]", trans = 'log10',
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)),
                     sec.axis = sec_axis(~.,
                                        labels = trans_format("log10", math_format(10^.x)))) +
  theme_bw() + 
  annotation_logticks(sides = "lr")
#ggsave(paste0(output, "figures/groundwater_boxplot_parameters_ugl.png"))

# all metals in ug/l
# metals excluding Ag and Cd as they are all <dl
metals <- c("Al", "As", "B", "Ba", "Be", "Ca", "Co", "Cr", "Cu", "Fe", "K", "Li", 
            "Mg", "Mn", "Mo", "Na", "Ni", "Pb", "Sb", "Se", "Si", "Ti", "V", "Zn")

d <- data %>%
  filter(year == 2021, !is.na(value),
         sampletype == "groundwater",
         # select only metals
         parameter %in% metals) %>%
  mutate(value = ifelse(units == "mg/l", value * 1000, value))
  
sample_size <- d %>%
  filter(parameter %in% metals) %>%
  group_by(parameter) %>% 
  dplyr::summarise(num = n())
d <- d %>%
  left_join(sample_size) %>%
  mutate(myaxis = factor(paste0("n=", num)))
ggplot(d %>% mutate(myaxis = paste0(parameter, "\n", num)), 
       aes(x = myaxis, y = value)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot(fill = "lightgrey", width = 0.6) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "[ug/L]", trans = 'log10',
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)),
                     sec.axis = sec_axis(~.,
                                         labels = trans_format("log10", math_format(10^.x)))) +
  theme_bw() + 
  annotation_logticks(sides = "lr")
#ggsave(paste0(output, "figures/groundwater_boxplot_metals_ugl.png"))

d <- data %>%
  filter(year == 2021, !is.na(value),
         sampletype %in% c("groundwater", "surfacewater", "wastewater"),
         # select only metals
         parameter %in% metals) %>%
  mutate(value = ifelse(units == "mg/l", value * 1000, value))

sample_size <- d %>%
  filter(parameter %in% metals) %>%
  group_by(parameter) %>% 
  dplyr::summarise(num = n())
d <- d %>%
  left_join(sample_size) %>%
  mutate(myaxis = factor(paste0("n=", num)))
ggplot(d %>% mutate(myaxis = paste0(parameter, "\n", num)), 
       aes(x = myaxis, y = value, fill = sampletype)) +
  #stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot(width = 0.6) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "[ug/L]", trans = 'log10',
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)),
                     sec.axis = sec_axis(~.,
                                         labels = trans_format("log10", math_format(10^.x)))) +
  theme_bw() + 
  annotation_logticks(sides = "lr")
ggsave(paste0(output, "figures/groundwater_wastewater_boxplot_metals_ugl.png"))


# metals per geology
d <- data %>%
  filter(year == 2021, !is.na(value),
         sampletype == "groundwater",
         parameter %in% metals) %>%
  mutate(value = ifelse(units == "mg/l", value * 1000, value))
# add geology and land use
d <- d %>%
  left_join(., metadata %>% select(samplecode, geology, geology_abr, 
                                   landuse_zonal_map, Land.use.based.on.own.observations))

sample_size <- d %>%
  group_by(parameter, geology) %>% 
  dplyr::summarise(num = n())
d <- d %>%
  left_join(sample_size) %>%
  mutate(myaxis = factor(paste0("n=", num)))
ggplot(d %>% mutate(myaxis = paste0(parameter, "\n", num)), 
       aes(x = parameter, y = value, fill = geology)) +
  #stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot(width = 0.6) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "[ug/L]", trans = 'log10',
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)),
                     sec.axis = sec_axis(~.,
                                         labels = trans_format("log10", math_format(10^.x)))) +
  theme_bw() + 
  theme(legend.position = c(0.86, 0.83)) +
  annotation_logticks(sides = "lr") 
  #facet_wrap(facets = "geology", nrow = 6)


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

# # zonder tellingen in boxplot:
# # voor data met mg/l
d <- data %>%
  filter(method == "ICP-MS",
         !is.na(value)) %>%
  mutate(limit_symbol = factor(limit_symbol,
                               levels = c("<", "", ">"),
                               labels = c("< detection limit",
                                          "good measurement",
                                          "> calibration line")))
# p1 <- ggplot(d %>% filter(units == "mg/l"),
#              aes(x = parameter, y = value, fill = limit_symbol)) +
#   geom_boxplot() +
#   scale_fill_discrete(name = "") +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "conc. [mg/l]") +
#   theme_bw()
# p2 <- ggplot(d %>% filter(units == "mg/l"),
#              aes(x = parameter, y = value, fill = limit_symbol)) +
#   geom_boxplot() +
#   scale_fill_discrete(name = "") +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "") +
#   coord_cartesian(ylim = c(0, 750)) +
#   theme_bw() 
# 
# p3 <- ggplot(d %>% filter(units == "mg/l"),
#              aes(x = parameter, y = value, fill = limit_symbol)) +
#   geom_boxplot() +
#   scale_fill_discrete(name = "") +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "conc. [mg/l]") +
#   coord_cartesian(ylim = c(0, 100)) +
#   theme_bw() 
# 
# p4 <- ggplot(d %>% filter(units == "mg/l"),
#              aes(x = parameter, y = value, fill = limit_symbol)) +
#   geom_boxplot() +
#   scale_fill_discrete(name = "") +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "") +
#   coord_cartesian(ylim = c(0, 15)) +
#   theme_bw() 
# 
# ggpubr::ggarrange(p1, p2, p3, p4,
#                   labels = "AUTO",
#                   common.legend = T,
#                   legend = "bottom")
# 
# grobs <- ggplotGrob(p1)$grobs
# legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
# 
# pgrid <- plot_grid(p1, p2, labels = "AUTO")
# plot_grid(pgrid, legend, ncol = 2, rel_widths = c(1, 0.1))
# 
# # voor data met ug/l
# d <- data %>%
#   filter(method == "ICP-MS",
#          parameter != "Mg26") %>%
#   mutate(limit_symbol = factor(limit_symbol, 
#                                levels = c("<", "", ">"), 
#                                labels = c("< detection limit",
#                                           "good measurement",
#                                           "> calibration line")))
# p1 <- ggplot(d %>% filter(units == "ug/l",
#                           !parameter %in% c("Fe56", "Fe57")),
#              aes(x = parameter, y = value, fill = limit_symbol)) +
#   geom_boxplot() +
#   scale_fill_discrete(name = "") +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "[ug/l]") +
#   coord_cartesian(ylim = c(0, 1600)) +
#   theme_bw()
# p2 <- ggplot(d %>% filter(units == "ug/l",
#                           !parameter %in% c("Fe56", "Fe57")),
#              aes(x = parameter, y = value, fill = limit_symbol)) +
#   geom_boxplot() +
#   scale_fill_discrete(name = "") +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "[ug/l]") +
#   coord_cartesian(ylim = c(0, 250)) +
#   theme_bw() 
# p3 <- ggplot(d %>% filter(units == "ug/l",
#                           !parameter %in% c("Fe56", "Fe57")),
#              aes(x = parameter, y = value, fill = limit_symbol)) +
#   geom_boxplot() +
#   scale_fill_discrete(name = "") +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "[ug/l]") +
#   coord_cartesian(ylim = c(0, 50)) +
#   theme_bw() 
# 
# ggpubr::ggarrange(p1, p2, p3,
#                   labels = "AUTO",
#                   common.legend = T,
#                   legend = "bottom",
#                   allign = "hv",
#                   nrow = 3)
# 
# grobs <- ggplotGrob(p1)$grobs
# legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

# Per individual element
sample_size <- d %>%
  group_by(parameter, limit_symbol) %>% summarise(num = n())
d <- d %>%
  left_join(sample_size) %>%
  filter(!is.na(value)) %>%
  mutate(myaxis = factor(paste0("n=", num)),
         order = case_when(
           limit_symbol == "< detection limit" ~ 1,
           limit_symbol == "good measurement" ~ 2,
           limit_symbol == "> calibration line" ~ 3,
           TRUE ~ 99
         ))

ggplot(d %>% 
         #filter(parameter %in% c("Na", "Ca", "B", "S")) %>%
         mutate(parameter = paste0(parameter, " [", units, "]")), 
       aes(x = fct_reorder(myaxis, order), y = value, fill = limit_symbol)) +
  geom_boxplot() +
  #stat_summary(fun.data = calc_boxplot_stat, geom = "boxplot") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "", trans = "log10") +
  theme_bw() +
  theme(legend.position = "top",
        #axis.text.x = element_blank(),
        legend.title = element_blank()) +
  facet_wrap(facets = "parameter", scales = "free")
ggsave(file = paste0(output, "figures/boxplot_metals_detectionlimits.pdf"), 
       width = 210, height = 297, units = "mm")

# Per individual element without outliers with nr
ggplot(d %>% 
         filter(parameter %in% c("Na", "Ca", "B", "S")) %>%
         mutate(parameter = paste0(parameter, " [", units, "]")),
       aes(x = fct_reorder(myaxis, order), y = value, fill = limit_symbol)) +
  stat_summary(fun.data = calc_boxplot_stat, geom = "boxplot") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") +
  theme_bw() +
  theme(legend.position = "top",
        #axis.text.x = element_blank(),
        legend.title = element_blank()) +
  facet_wrap(~parameter, scales = "free")
ggsave(file = paste0(output, "Output/Figures/Checks/detection_limits/metals_detectionlimits_no_outliers.pdf"), 
       width = 210, height = 297, units = "mm")


# boxplot per watertype


## per sampletype




## per geology


## per year

ggplot(data = set,  
       aes(x = factor(year),
           y = value)) +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  geom_boxplot(fill = "lightgrey", width = 0.6) +
  #geom_point(alpha = 0.4) +
  #stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") + 
  theme_bw() +
  facet_wrap(~param, scales = "free")




















