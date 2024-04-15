#
# Input: DOC analysis from UvA
#         
# Output: Cleaned DOC file
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 01-11-2022
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, cowplot, ggpubr)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/" 

# alkalinity data file
DOC1 <- read.xlsx(paste0(input, "Lab/DOC_UvA/2022/2022_DOC_total.xlsx"),
                  sheet = "Run1")
DOC2 <- read.xlsx(paste0(input, "Lab/DOC_UvA/2022/2022_DOC_total.xlsx"),
                  sheet = "Run2")
DOC3 <- read.xlsx(paste0(input, "Lab/DOC_UvA/2023/Mike Wit SEALINK samples 2022.xlsx"),
                  sheet = "Total_DOC")
TDN3 <- read.xlsx(paste0(input, "Lab/DOC_UvA/2023/Mike Wit SEALINK samples 2022.xlsx"),
                  sheet = "Total_TDN")

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################

## For 2021 DOC measurements
# Select columns and combine datasets
d1 <- rbind(DOC1 %>% rename(Sample.ID = Sample.Name,
                           Conc. = `DOC.[mg/L]`),
           DOC2 %>% select(-c(Result, X6))) %>%
  rename(samplecode = Sample.ID)

# Calculate average value and SD
d1 <- d1 %>% 
  group_by(samplecode) %>%
  mutate(value = mean(Conc.),
         sd = sd(Conc.)) %>%
  ungroup() %>%
  # only unique rows
  filter(!is.na(AVG)) %>%
  # add sampletype
  mutate(sampletype = substr(samplecode, start = 1, stop = 2)) %>%
  ## put into format of other data so that it can be added to the final dataset
  # add columns with parameter name, units and notes
  mutate(parameter = "DOC",
         units = "mg/l",
         notes = "") %>%
  # rename columns
  rename(value = value) %>%
  # add limit symbol, detection limit column and method
  mutate(limit_symbol = "",
         detection_limit = NA,
         method = "DOC UvA") %>%
  # select only relevant columns 
  select(samplecode, parameter, value, sd, limit_symbol, detection_limit, units, method, notes, sampletype) 

## For 2022 DOC + TDN measurements
d2 <- DOC3 %>%
  rename(samplecode = Sample_ID) %>%
  mutate(value = `DOC_umol/L` / 1000 * 12.011, # convert umol/L to mg/L --> /1000 * 12.011
         sd = SD / 1000 * 12.011, # convert umol/L to mg/L --> /1000 * 12.011
         sampletype = substr(samplecode, start = 1, stop = 2)) %>%
  ## put into format of other data so that it can be added to the final dataset
  # add columns with parameter name, units and notes
  mutate(parameter = "DOC",
         units = "mg/l") %>%
  # add limit symbol, detection limit column and method
  mutate(limit_symbol = "",
         detection_limit = NA,
         method = "DOC UvA") %>%
  # select only relevant columns 
  select(samplecode, parameter, value, sd, limit_symbol, detection_limit, units, method, notes, sampletype) 

n2 <- TDN3 %>%
  rename(samplecode = Sample_ID) %>%
  mutate(value = `TDN_umol/L` / 1000 * 14.0067, # convert umol/L to mg/L --> / 1000 * 14.0067
         sd = SD / 1000 * 14.0067, # convert umol/L to mg/L --> / 1000 * 14.0067
         sampletype = substr(samplecode, start = 1, stop = 2)) %>%
  ## put into format of other data so that it can be added to the final dataset
  # add columns with parameter name, units and notes
  mutate(parameter = "TDN",
         units = "mg/l") %>%
  # add limit symbol, detection limit column and method
  mutate(limit_symbol = "",
         detection_limit = NA,
         method = "DOC UvA") %>%
  # select only relevant columns 
  select(samplecode, parameter, value, sd, limit_symbol, detection_limit, units, method, notes, sampletype) 

## Combine all
d <- rbind(d1, d2, n2)

## Some plots to check DOC concentrations
# plot gw
ggplot(d %>% filter(sampletype == "GW"), 
       aes(x = samplecode, y = value)) +
  geom_errorbar(aes(ymin = value-sd, ymax = value+sd),
                width = 0.8, colour = "black") +
  geom_point() +
  scale_y_continuous("[mg/L]") +
  theme_bw() +
  coord_flip() +
  facet_wrap(facets = "parameter", scales = "free_x")
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# plot all
ggplot(d %>% filter(parameter == "DOC"), aes(x = reorder(samplecode, -value), y = value, colour = sampletype)) +
  geom_errorbar(aes(ymin = value-sd, ymax = value+sd),
                width = 0.4, colour = "black") +
  geom_point() +
  scale_y_continuous("DOC [mg/L]", limits = c(0, 75)) +
  scale_x_discrete("") +
  scale_color_manual(values = c("grey", "skyblue", "blue1",
                                "forestgreen", "orangered", "turquoise4", "plum", "purple")) +
  theme_bw() +
  theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = c(0.9, 0.7)) +
  ggtitle("ordered DOC samples 2021-2022")
  #facet_wrap(facets = 'sampletype', scales = "free")
ggsave(paste0(output, "Output/Figures/Hydrochemistry/Nutrients/DOC_ordered_2021_2022.png"),
       width = 8, height = 8)

# vertical plot
ggplot(d %>% filter(parameter == "DOC"), aes(x = reorder(samplecode, value), y = value, colour = sampletype)) +
  geom_errorbar(aes(ymin = value-sd, ymax = value+sd),
                width = 0.4, colour = "black") +
  geom_point() +
  scale_y_continuous("DOC [mg/L]", limits = c(0, 75)) +
  scale_x_discrete("") +
  scale_color_manual(values = c("grey", "skyblue", "blue1",
                                      "forestgreen", "orangered", "turquoise4", "plum", "purple")) +
                                        theme_bw() +
  coord_flip() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = c(0.9, 0.5)) +
  ggtitle("ordered DOC samples 2021-2022")

# plot per group
ggplot(d %>% filter(parameter == "DOC"), aes(x = samplecode, y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin = value-sd, ymax = value+sd),
                width = 0.2) +
  scale_y_continuous("DOC [mg/L]") +
  theme_bw() +
  facet_wrap(facets = 'sampletype', scales = "free")

# plot boxplots per group
ggplot(d %>% filter(parameter == "DOC"), aes(x = sampletype, y = value, fill = sampletype)) +
  geom_boxplot() +
  scale_y_continuous("DOC [mg/L]") +
  theme_bw()

# in grey with in zoom plot
sample_size <- d %>%
  filter(parameter == "DOC") %>%
  group_by(sampletype) %>% summarise(num = n())

p1 <- d %>%
  filter(parameter == "DOC") %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(sampletype, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y = value)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot(width = 0.6, fill = "grey") +
  scale_x_discrete(name = "") +
  scale_y_continuous("DOC [mg/L]") +
  theme_bw() 

p2 <- d %>%
  filter(parameter == "DOC") %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(sampletype, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y = value)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot(width = 0.6, fill = "grey") +
  scale_x_discrete(name = "") +
  scale_y_continuous("DOC [mg/L]") +
  coord_cartesian(ylim = c(0, 65)) +
  theme_bw()

ggarrange(p1, p2,
          labels = "AUTO", 
          common.legend = T)
ggsave(paste0(output, "Output/Figures/Hydrochemistry/Nutrients/DOC_boxplots_per_watertype_2021-2022.png"),
       width = 8, height = 4)

# in colour
p1 <- d %>%
  filter(parameter == "DOC") %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(sampletype, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y = value, fill = sampletype)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  #scale_fill_viridis(discrete = TRUE) +
  #geom_boxplot(width = 0.6, fill = "grey") +
  geom_boxplot(width = 0.6) +
  scale_x_discrete(name = "") +
  scale_y_continuous("DOC [mg/L]") +
  scale_fill_discrete(labels = c("Groundwater", "Rainwater", "Seawater", 
                                 "Spring", "Surface runoff", "Surfacewater",
                                 "Tapwater", "Wastewater")) +
  theme_bw() +
  theme(legend.title = element_text(colour = "black", size = 10, face = "bold")) +
  theme(legend.background = element_rect(fill = "white",
                                         linewidth = 0.5,
                                         linetype = "solid",
                                         colour = "black"))

p2 <- d %>%
  filter(parameter == "DOC") %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(sampletype, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y = value, fill = sampletype)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  #scale_fill_viridis(discrete = TRUE) +
  geom_boxplot(width = 0.6) +
  scale_x_discrete(name = "") +
  scale_y_continuous("DOC [mg/L]") +
  scale_fill_discrete(labels = c("Groundwater", "Rainwater", "Seawater", 
                                 "Spring", "Surface runoff", "Surfacewater",
                                 "Tapwater", "Wastewater")) +
  coord_cartesian(ylim = c(0, 65)) +
  theme_bw() + 
  theme(legend.title = element_text(colour = "black", size = 10, face = "bold")) +
  theme(legend.background = element_rect(fill = "white",
                                         linewidth = 0.5,
                                         linetype = "solid",
                                         colour = "black"))

ggarrange(p1, p2,
          labels = "AUTO", 
          common.legend = T)
ggsave(paste0(output, "Output/Figures/Hydrochemistry/Nutrients/DOC_boxplots_per_watertype_2021-2022_coloured.png"),
       width = 8, height = 8)


# # plot legend as separate plot
# grobs <- ggplotGrob(p2$grobs)
# legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
# 
# plot_grid(p1, p2, labels = "AUTO")

# table
d %>%
  filter(parameter == "DOC") %>%
  mutate(conc = round(value, 2)) %>%
  group_by(sampletype) %>%
  summarise(n = n(),
            min = min(conc),
            median = median(conc, na.rm = T),
            value = mean(conc),
            max = max(conc, na.rm = T)) %>%
  view()

# Check if every sample has only 1 value
check <- d %>%
  filter(parameter == "DOC") %>%
  group_by(samplecode) %>%
  summarise(measurements = n_distinct(value)) %>%
  filter(measurements > 1)
if(nrow(check) > 0) {
  stop("More than 1 value for DOC in a sample")
}

# 

###############################################################################
# save data
###############################################################################

write.xlsx(d %>% select(-sampletype), paste0(output, "Clean_data/DOC_clean.xlsx"))


