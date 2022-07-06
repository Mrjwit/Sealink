#
# Input: hydrochemical dataset of Curacao with metadata file
#         
# Output: Boxplots, statistics and analysis of metals
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 04-07-2022
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, scales, ggtext)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/" 

# load cleaned data
data <- openxlsx::read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx"))
metadata <- openxlsx::read.xlsx(paste0(input, "metadata_2021.xlsx"))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/Output/Statistics/" 

###############################################################################
# Descriptive statistics
###############################################################################

# all metals 
metals <- c("Ag", "Al", "As", "B", "Ba", "Be", "Ca", "Cd", "Co", "Cr", "Cu", "Fe", "K", 
            "Li", "Mg", "Mn", "Mo", "Na", "Ni", "Pb", "Sb", "Se", "Si", "Ti", "V", "Zn")

# create empty workbook
wb <- createWorkbook()

# Total overview all metals and including values < dl
d1 <- data %>%
  filter(year == 2021, parameter %in% metals, !is.na(value)) %>%
mutate(parameter = paste0(parameter, " [", units, "]")) %>%
  group_by(parameter) %>%
  summarise(n=n(),
            '% <dl' = round(length(value[limit_symbol == "<"]) / length(value) * 100, digits = 1), 
            min = min(value, na.rm = T),
            p10 = round(quantile(value, 0.1, na.rm = T), digits = 2),
            p50 = round(quantile(value, 0.5, na.rm = T), digits = 2),
            avg = round(mean(value, na.rm = T), digits = 2),
            p90 = round(quantile(value, 0.9, na.rm = T), digits = 2),
            max = round(max(value, na.rm = T), digits = 2),
            dl = paste(unique(detection_limit), collapse = ", "),
            method = paste(unique(method), collapse = ", ")) 
addWorksheet(wb, "2021 incl <dl")
writeData(wb, "2021 incl <dl", d1)

# only values > dl
d1 <- data %>%
  filter(year == 2021, parameter %in% metals, !is.na(value), limit_symbol != "<") %>%
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
addWorksheet(wb, "2021 excl <dl")
writeData(wb, "2021 excl <dl", d1)

# only values < dl
d1 <- data %>%
  filter(year == 2021, parameter %in% metals, !is.na(value), limit_symbol == "<") %>%
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
addWorksheet(wb, "2021 only <dl")
writeData(wb, "2021 only <dl", d1)

# only groundwater samples
d1 <- data %>%
  filter(year == 2021, parameter %in% metals, !is.na(value), sampletype == "groundwater") %>%
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
addWorksheet(wb, "2021 gw")
writeData(wb, "2021 gw", d1)

# only groundwater all years (1977, 1992, 2021) 
d1 <- data %>%
  filter(parameter %in% metals, !is.na(value), sampletype == "groundwater") %>%
  # some metals in 1992 are in mg/L, change to ug/L
  mutate(value = ifelse(parameter %in% c("Al", "Cd", "Co", "Cr", "Cu", "Mn", "Ni", "Pb", "Ti", "V", "Zn") & year == 1992, 
                        value * 1000, value)) %>%
  mutate(detection_limit = ifelse(parameter %in% c("Al", "Cd", "Co", "Cr", "Cu", "Mn", "Ni", "Pb", "Ti", "V", "Zn") & year == 1992, 
                                  detection_limit * 1000, detection_limit)) %>%
  mutate(units = ifelse(parameter %in% c("Al", "Cd", "Co", "Cr", "Cu", "Mn", "Ni", "Pb", "Ti", "V", "Zn") & year == 1992, 
                        "ug/l", units)) %>%
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
            dl = paste(sort(unique(detection_limit)), collapse = ", "),
            method = paste(unique(method), collapse = ", ")) 
addWorksheet(wb, "all years gw")
writeData(wb, "all years gw", d1)

# save excel workbook
freezePane(wb, sheet = 1, firstRow = T)
freezePane(wb, sheet = 2, firstRow = T)
freezePane(wb, sheet = 3, firstRow = T)
freezePane(wb, sheet = 4, firstRow = T)
freezePane(wb, sheet = 5, firstRow = T)
saveWorkbook(wb, file = paste0(output, "metals_statistics.xlsx"), overwrite = T)

###############################################################################
# Editing data
###############################################################################

# metals excluding Ag and Cd as they are all <dl
metals <- c("Al", "As", "B", "Ba", "Be", "Ca", "Co", "Cr", "Cu", "Fe", "K", "Li", 
            "Mg", "Mn", "Mo", "Na", "Ni", "Pb", "Sb", "Se", "Si", "Ti", "V", "Zn")

# select only 2021 and adjust units to ug/L for boxplots
d <- data %>%
  filter(#year == 2021, 
         !is.na(value),
         parameter %in% metals) %>%
  mutate(value = ifelse(units == "mg/l", value * 1000, value))

# add geology and land use
d <- d %>%
  left_join(., metadata %>% select(samplecode, geology, geology_abr, 
                                   landuse_zonal_map, Land.use.based.on.own.observations))

###############################################################################
# Data analysis
###############################################################################

#### Boxplots ####
# boxplot for all samples
sample_size <- d %>%
  group_by(parameter) %>% 
  dplyr::summarise(num = n())
dat <- d %>%
  left_join(sample_size) %>%
  mutate(myaxis = factor(paste0("n=", num)))
ggplot(dat %>% mutate(myaxis = paste0(parameter, "\n", num)), 
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
ggsave(paste0(output, "figures/boxplot_all_metals_ugl.png"))




#### difference between years ####

dat <- data %>%
  filter(parameter == "pH",
         sampletype == "groundwater")

# experiment wide kruskal significance test
kt <- kruskal.test(value ~ year, data = dat)

if(kt$p.value < 0.05) {
pt <- pairwise.wilcox.test(dat$value, dat$year,
                           p.adjust.method = "BH") # method = Benjamini-Hochberg
}

n_count <- dat %>%
  count(year)
dat <- dat %>%
  left_join(., n_count) %>%
  mutate(myaxis = paste0(year, "\n", "N=", n))
ggplot(dat, aes(x = myaxis, y = value, fill = as.factor(year))) +
  geom_boxplot(alpha = 0.7, show.legend = F) +
  #geom_violin(alpha = 0.2, show.legend = F) +
  stat_summary(fun = median, show.legend = F, geom = "crossbar") +
  #geom_jitter(show.legend = F, width = 0.25, shape = 21, color = "black") +
  labs(title =  "pH trend from 1977 till 2021 in groundwater, Curacao",
       x = NULL,
       y = "pH") +
  theme_classic() +
  theme(plot.title = element_text(size = 18)) +
  #theme(axis.text.x = element_markdown())
  geom_line(data = tibble(x = c(2, 3), y = c(8.5, 8.5)),
            aes(x=x, y=y),
            inherit.aes = F) +
  geom_line(data = tibble(x = c(1, 2.5), y = c(9.8, 9.8)),
            aes(x=x, y=y),
            inherit.aes = F) +
  geom_text(data = tibble(x = c(2.5), y = 8.6),
            aes(x = x, y = y), label = "*", size = 6,
            inherit.aes = F) +
  geom_text(data = tibble(x = c(1.75), y = 9.9),
            aes(x = x, y = y), label = "*", size = 6,
            inherit.aes = F)

ggplot(dat, aes(x = as.factor(year), y = value, fill = as.factor(year))) +
  geom_point(shape = 21, size = 2, stroke = 0.5)  +
  #coord_fixed() +
  labs(title = "pH trend from 1977 till 2021",
       x = "",
       y = "pH") +
  theme_classic() +
  theme(legend.position = "none")


ggplot(dat, aes(x=as.factor(year), y=value)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "", y = "pH")

