#
# Input: data of ICP-OES analysis
#
# Output: Cleaned lab results from ICP-OES merged together in long and wide format
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: XX-XX-XXXX
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, readxl)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/Lab/ICP/" 

ICP <- read_excel(paste0(input, "Third_fieldwork/240325_ICPOES_Mike_run1.xlsx"),
                  sheet = "processed", skip = 1, n_max = 69)

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################


# values with #Name indicates that the detector is overloaded
# values with x indicates that the value is above the calibration line
# values with b indicates that the value is below the detection limit ??

d <- ICP %>%
  # convert all elements to characters so they can be placed in one column
  mutate_at(c(2:28), as.character) %>%
  #place parameters in long format
  pivot_longer(., cols = c(Ag:Zn),
               values_to = "value",
               names_to = "parameter") %>%
  # add limit symbol based on string, detection limit values and units
  mutate(limit_symbol = ifelse(str_detect(value, "<"), "<", 
                               ifelse(str_detect(value, ">"), ">", "")),
         # range = 1 - 100 ug/l, dl = 0.1?
         detection_limit = case_when(
           #parameter %in% c("Ca", "S", "Si") ~ 500,
           parameter %in% c("Ba", "Na") ~ 1, # K, Li and Sr were first also here
           parameter %in% c("Ca", "Mg", "K") ~ 0.5,
           TRUE ~ 0.025),
         units = "mg/l") %>%
  # remove - sign from values before converting to numeric
  mutate(value = gsub("^-", "", value)) %>%
  # change values to numeric 
  mutate(value = parse_number(value)) %>%
  # change limit symbol for values <= 1 ug/l
  mutate(limit_symbol = ifelse(is.na(value), "", 
                               ifelse(value <= detection_limit, "<", limit_symbol)),
         sd = NA,
         method = "ICP-OES",
         notes = "") %>%
  # change units and values for trace elements to ug/L
  mutate(value = ifelse(!parameter %in% c("Ca", "Fe", "K", "Mg", "Na", "P", "S", "Si", "Sr"), value * 1000, value),
         detection_limit = ifelse(!parameter %in% c("Ca", "Fe", "K", "Mg", "Na", "P", "S", "Si", "Sr"), 
                                  detection_limit * 1000, detection_limit),
         units = ifelse(!parameter %in% c("Ca", "Fe", "K", "Mg", "Na", "P", "S", "Si", "Sr"), "ug/l", units)) %>%
  # change values < dl to dl
  mutate(value = ifelse(value <= detection_limit, detection_limit, value)) %>%
  # select only relevant columns
  select(samplecode, parameter, value, sd, limit_symbol, detection_limit, units, method, notes)


# check if no double values per parameter
check <- d %>%
  group_by(samplecode, parameter) %>%
  summarise(n_values = length(value)) %>%
  filter(n_values > 1)

if(nrow(check) > 0) {
  stop(paste("some elements still have more than 1 value per sample:",
             sort(unique(check$parameter))))
}

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

# # boxplot per parameter to see from which parameters to change units.
# calc_boxplot_stat <- function(x) {
#   coef <- 1.5
#   n <- sum(!is.na(x))
#   # calculate quantiles
#   stats <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
#   names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
#   iqr <- diff(stats[c(2, 4)])
#   # set whiskers
#   outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
#   if (any(outliers)) {
#     stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
#   }
#   return(stats)
# }
# 
# ggplot(d %>% mutate(parameter = paste0(parameter, " [", units, "]")), 
#        aes(x = parameter, y = value, groep = parameter)) +
#   #geom_boxplot(outlier.shape = NA) +
#   stat_summary(fun.data = calc_boxplot_stat, geom = "boxplot") +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "") +
#   theme_bw() +
#   facet_wrap(facets = "parameter", scales = "free")
# # boxplots with only values >dl
# ggplot(d %>% mutate(parameter = paste0(parameter, " [", units, "]")) %>%
#          filter(limit_symbol != "<"), 
#        aes(x = parameter, y = value, groep = parameter)) +
#   #geom_boxplot(outlier.shape = NA) +
#   stat_summary(fun.data = calc_boxplot_stat, geom = "boxplot") +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "") +
#   theme_bw() +
#   facet_wrap(facets = "parameter", scales = "free")
# 
# ggplot(d %>% filter(units != "mg/l"),
#          #mutate(parameter = paste0(parameter, " [", units, "]")) %>% 
#        aes(x = parameter, y = value, groep = parameter)) +
#   #geom_boxplot(outlier.shape = NA) +
#   stat_boxplot(geom = 'errorbar', width = 0.4) +
#   stat_summary(fun.data = calc_boxplot_stat, geom = "boxplot", fill = "lightgrey", width = 0.6) +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "concentration [ug/l]") +
#   #coord_cartesian(ylim = c(0, 300)) +
#   theme_bw() 
#   #facet_wrap(facets = "parameter", scales = "free")


ggplot(d, aes(x = parameter, y = value)) +
  geom_boxplot() +
  theme_bw() + 
  facet_wrap(facets = "units", scales = "free")

# # how many samples >dl and <cl per parameter
check_limits <- d %>%
  group_by(parameter, units) %>%
  summarise(samples = n(),
            "< cal. line [%]" = round(length(value[limit_symbol == "<"]) / n() * 100, digits = 1),
            "> cal. line [%]" = round(length(value[limit_symbol == ">"]) / n() * 100, digits = 1),
            "value ok [%]" = round(length(value[limit_symbol == ""]) / n() * 100, digits = 1),
            min = min(value, na.rm = T),
            med = median(value, na.rm = T),
            avg = mean(value, na.rm = T),
            max = max(value, na.rm = T)) %>%
  view()

# check statistics
d %>%
  group_by(parameter, units) %>%
  summarise(n = n(),
            min = min(value, na.rm = T),
            med = median(value, na.rm = T),
            avg = mean(value, na.rm = T),
            max = max(value, na.rm = T))

# 
# d %>% 
#   group_by(parameter) %>%
#   summarise(n.dl = n_distinct(value[limit_symbol == "<"])) %>%
#   view()

# make wide format ICP
d_ICP_wide <- d %>%
  # adjust digits of values
  mutate(value = round(value, digits = 3)) %>%
  # select main cations
  #filter(parameter %in% c("Na", "Ca", "Mg", "B", "K", "Fe", "Si"))
  # adjust values < and > dl
  mutate(parameter = paste(parameter, units),
         value = paste(limit_symbol, value)) %>%
  select(samplecode, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value) 

###############################################################################
# save data
###############################################################################

openxlsx::write.xlsx(d, paste0(output, "Clean_data/Third_fieldwork/ICPOES_Oct-Jan_2024.xlsx"))
openxlsx::write.xlsx(d_ICP_wide, paste0(output, "Clean_data/Third_fieldwork/ICPOES_Oct-Jan_2024_wide.xlsx"))


