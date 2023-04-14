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
DOC1 <- read.xlsx(paste0(input, "lab/DOC_UvA/2022_DOC_total.xlsx"),
                  sheet = "Run1")
DOC2 <- read.xlsx(paste0(input, "lab/DOC_UvA/2022_DOC_total.xlsx"),
                  sheet = "Run2")

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################

# Select columns and combine datasets
d <- rbind(DOC1 %>% rename(Sample.ID = Sample.Name,
                           Conc. = `DOC.[mg/L]`),
           DOC2 %>% select(-c(Result, X6))) %>%
  rename(samplecode = Sample.ID)

# Calculate average value and SD
d <- d %>% 
  group_by(samplecode) %>%
  mutate(avg = mean(Conc.),
         sd = sd(Conc.)) %>%
  ungroup() %>%
  # only unique rows
  filter(!is.na(AVG)) %>%
  # add sampletype
  mutate(sampletype = substr(samplecode, start = 1, stop = 2)) %>%
  # remove redundant columns of individial concentrations and duplicate columns
  select(-c(Conc., AVG, SD))

# plot gw
ggplot(d %>% filter(sampletype == "GW"), 
       aes(x = samplecode, y = avg)) +
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd),
                width = 0.8, colour = "black") +
  geom_point() +
  scale_y_continuous("DOC [mg/L]") +
  theme_bw() +
  coord_flip()
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# plot all
ggplot(d, aes(x = reorder(samplecode, -avg), y = avg, colour = sampletype)) +
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd),
                width = 0.4, colour = "black") +
  geom_point() +
  scale_y_continuous("DOC [mg/L]", limits = c(0, 75)) +
  scale_x_discrete("") +
  scale_color_manual(values = c("grey", "skyblue", "blue1",
                                "forestgreen", "orangered", "turquoise4", "plum", "purple")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = c(0.9, 0.7))
  #facet_wrap(facets = 'sampletype', scales = "free")
ggsave(paste0(output, "Output/Figures/Hydrochemistry/Nutrients/DOC_ordered_2021.png"),
       width = 8, height = 8)

# vertical plot
ggplot(d, aes(x = reorder(samplecode, avg), y = avg, colour = sampletype)) +
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd),
                width = 0.4, colour = "black") +
  geom_point() +
  scale_y_continuous("DOC [mg/L]", limits = c(0, 75)) +
  scale_x_discrete("") +
  scale_color_manual(values = c("grey", "skyblue", "blue1",
                                      "forestgreen", "orangered", "turquoise4", "plum", "purple")) +
                                        theme_bw() +
  coord_flip() +
  theme(legend.position = c(0.9, 0.5))

# plot per group
ggplot(d, aes(x = samplecode, y = avg)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd),
                width = 0.2) +
  scale_y_continuous("DOC [mg/L]") +
  theme_bw() +
  facet_wrap(facets = 'sampletype', scales = "free")

# plot boxplots per group
ggplot(d, aes(x = sampletype, y = avg, fill = sampletype)) +
  geom_boxplot() +
  scale_y_continuous("DOC [mg/L]") +
  theme_bw()

# in grey with in zoom plot
sample_size <- d %>%
  group_by(sampletype) %>% summarise(num = n())

p1 <- d %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(sampletype, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y = avg)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot(width = 0.6, fill = "grey") +
  scale_x_discrete(name = "") +
  scale_y_continuous("DOC [mg/L]") +
  theme_bw() 

p2 <- d %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(sampletype, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y = avg)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot(width = 0.6, fill = "grey") +
  scale_x_discrete(name = "") +
  scale_y_continuous("DOC [mg/L]") +
  coord_cartesian(ylim = c(0, 65)) +
  theme_bw()

ggarrange(p1, p2,
          labels = "AUTO", 
          common.legend = T)
ggsave(paste0(output, "Output/Figures/Hydrochemistry/Nutrients/DOC_boxplots_per_watertype_2021.png"),
       width = 8, height = 4)

# in colour
p1 <- d %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(sampletype, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y = avg, fill = sampletype)) +
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
  left_join(sample_size) %>%
  mutate(myaxis = paste0(sampletype, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y = avg, fill = sampletype)) +
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
ggsave(paste0(output, "Output/Figures/Hydrochemistry/Nutrients/DOC_boxplots_per_watertype_2021_coloured.png"),
       width = 8, height = 8)


# # plot legend as separate plot
# grobs <- ggplotGrob(p2$grobs)
# legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
# 
# plot_grid(p1, p2, labels = "AUTO")

# table
d %>% mutate(conc = round(avg, 2)) %>%
  group_by(sampletype) %>%
  summarise(n = n(),
            min = min(conc),
            median = median(conc, na.rm = T),
            avg = mean(conc),
            max = max(conc, na.rm = T)) %>%
  view()

# put into format of other data so that it can be added to the final dataset
dat <- d %>%
  # add columns with parameter name, units and notes
  mutate(parameter = "DOC",
         units = "mg/l",
         notes = "") %>%
  # rename columns
  rename(value = avg) %>%
  # add limit symbol, detection limit column and method
  mutate(limit_symbol = "",
         detection_limit = NA,
         method = "DOC UvA") %>%
  # select only relevant columns 
  select(samplecode, parameter, value, sd, limit_symbol, detection_limit, units, method, notes) 

# Check if every sample has only 1 value
check <- dat %>%
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

write.xlsx(dat, paste0(output, "Clean_data/doc_clean.xlsx"))


