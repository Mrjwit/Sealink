#
# Input: hydrochemical dataset of Curacao with metadata file
#         
# Output: Boxplots, statistics and multivariate analysis of metals
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
pacman::p_load(tidyverse, openxlsx, scales, ggtext, ggcorrplot,
               corrplot, FactoMineR, factoextra, cluster)

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

# This part creates overviews of descriptive statistics for various selections
# for metal concentrations and writes these to an Excel file with different sheets
# uncomment the different function for creating, writing and saving the workbooks

# all metals 
metals <- c("Ag", "Al", "As", "B", "Ba", "Be", "Ca", "Cd", "Co", "Cr", "Cu", "Fe", "K", 
            "Li", "Mg", "Mn", "Mo", "Na", "Ni", "Pb", "Sb", "Se", "Si", "Ti", "V", "Zn")

# create empty workbook
#wb <- createWorkbook()

# Total overview all metals in all sample types and including values < dl
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
#addWorksheet(wb, "2021 incl <dl")
#writeData(wb, "2021 incl <dl", d1)

# only values > dl in all sample types
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
#addWorksheet(wb, "2021 excl <dl")
#writeData(wb, "2021 excl <dl", d1)

# only values < dl in all sample types
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
#addWorksheet(wb, "2021 only <dl")
#writeData(wb, "2021 only <dl", d1)

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
#addWorksheet(wb, "2021 gw")
#writeData(wb, "2021 gw", d1)

# only groundwater but for all years (1977, 1992, 2021) 
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
#addWorksheet(wb, "all years gw")
#writeData(wb, "all years gw", d1)

# # save excel workbook
# freezePane(wb, sheet = 1, firstRow = T)
# freezePane(wb, sheet = 2, firstRow = T)
# freezePane(wb, sheet = 3, firstRow = T)
# freezePane(wb, sheet = 4, firstRow = T)
# freezePane(wb, sheet = 5, firstRow = T)
# saveWorkbook(wb, file = paste0(output, "metals_statistics.xlsx"), overwrite = T)

###############################################################################
# Editing data
###############################################################################

# metals excluding Ag and Cd as they are all <dl
metals <- c("Al", "As", "B", "Ba", "Be", "Ca", "Co", "Cr", "Cu", "Fe", "K", "Li", 
            "Mg", "Mn", "Mo", "Na", "Ni", "Pb", "Sb", "Se", "Si", "Ti", "V", "Zn")

set <- c("Na", "Ca", "K", "Fe", "Mg", "Si", "NH4", "Cl", "HCO3", "SO4", "NO3", "PO4", "pH")

# select only 2021 and adjust units to ug/L for boxplots
d <- data %>%
  filter(year == 2021, 
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
## Fig.1 - boxplot metals for all sample types ##
# create dataset with nr of samples per parameter
sample_size <- d %>%
  group_by(parameter) %>% 
  dplyr::summarise(num = n())
# merge nr of samples together with concentrations
dat <- d %>%
  left_join(sample_size) %>%
  mutate(myaxis = factor(paste0("n=", num)))
# Plot boxplot
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
  annotation_logticks(sides = "lr") +
  labs(title = "All metals in all sample types, 2021")
#ggsave(paste0(output, "figures/metals/boxplot_all_metals_ugl.png"), dpi = 1000)

## Fig.1B - boxplot metals for all sample types only values >dl ##
# create dataset with nr of samples per parameter
sample_size <- d %>%
  filter(limit_symbol != "<") %>%
  group_by(parameter) %>% 
  dplyr::summarise(num = n())
# merge nr of samples together with concentrations
dat <- d %>%
  filter(limit_symbol != "<") %>%
  left_join(sample_size) %>%
  mutate(myaxis = factor(paste0("n=", num)))
# Plot boxplot
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
  annotation_logticks(sides = "lr") +
  labs(title = "All metals in all sample types, 2021")
#ggsave(paste0(output, "figures/metals/boxplot_all_metals_ugl_>dl.png"), dpi = 1000)

## Fig.2 - boxplot only minor/trace metals for all sample types ##
sample_size <- d %>%
  filter(!parameter %in% c("Ca", "K", "Mg", "Na", "Si")) %>%
  group_by(parameter) %>% 
  dplyr::summarise(num = n())
dat <- d %>%
  filter(!parameter %in% c("Ca", "K", "Mg", "Na", "Si")) %>%
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
  annotation_logticks(sides = "lr") +
  labs(title = "Minor/trace metals in all sample types, 2021")
#ggsave(paste0(output, "figures/metals/boxplot_trace_metals_ugl.png"), dpi = 1000)

## Fig.3 - boxplot all metals differentiating between sample types ##
sample_size <- d %>%
  filter(sampletype %in% c("groundwater", "surfacewater", "wastewater")) %>%
  group_by(parameter) %>% 
  dplyr::summarise(num = n())
dat <- d %>%
  filter(sampletype %in% c("groundwater", "surfacewater", "wastewater")) %>%
  left_join(sample_size) %>%
  mutate(myaxis = factor(paste0("n=", num)))
# Plot boxplot
ggplot(dat %>% mutate(myaxis = paste0(parameter, "\n", num)), 
       aes(x = myaxis, y = value, fill = sampletype)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot(width = 0.6) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "[ug/L]", trans = 'log10',
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)),
                     sec.axis = sec_axis(~.,
                                         labels = trans_format("log10", math_format(10^.x)))) +
  theme_bw() + 
  theme(legend.position = c(0.87, 0.87)) +
  annotation_logticks(sides = "lr") +
  labs(title = "All metals per sampletype, 2021")
#ggsave(paste0(output, "figures/metals/boxplot_metals_ugl_different_sampletypes.png"), dpi = 1000)

## Fig.4 - boxplot metals for all sample types ##
sample_size <- d %>%
  filter(sampletype == "groundwater") %>%
  group_by(parameter) %>% 
  dplyr::summarise(num = n())
dat <- d %>%
  filter(sampletype == "groundwater") %>%
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
  annotation_logticks(sides = "lr") +
  labs(title = "All metals in groundwater, 2021")
#ggsave(paste0(output, "figures/metals/boxplot_all_metals_gw_ugl.png"), dpi = 1000)

## Fig.4B - boxplot minor/trace metals in groundwater##
sample_size <- d %>%
  filter(sampletype == "groundwater",
         !parameter %in% c("Ca", "K", "Mg", "Na", "Si")) %>%
  group_by(parameter) %>% 
  dplyr::summarise(num = n())
dat <- d %>%
  filter(sampletype == "groundwater",
         !parameter %in% c("Ca", "K", "Mg", "Na", "Si")) %>%
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
  annotation_logticks(sides = "lr") +
  labs(title = "Minor/trace metals in groundwater, 2021")
#ggsave(paste0(output, "figures/metals/boxplot_trace_metals_gw_ugl.png"), dpi = 1000)

#### Correlation diagram ####
# leave Ag, Be, Cd, Sb
metals_cor <- c("Al", "As", "B", "Ba", 
                "Ca", "Co", "Cr", "Cu", "Fe",
                "K", "Li", "Mg", "Mn", "Mo", "Na", 
                "Ni", "P", "Pb", "S", "Se",
                "Si", "Ti", "V", "Zn")
# alternative selection also without Na, Ca, Mg
# metals_cor <- c("Al", "As", "B", "Ba",
#                 "Co", "Cr", "Cu", "Fe",
#                 "K", "Li", "Mn", "Mo",
#                 "Ni", "P", "Pb", "S", "Se",
#                 "Si", "Ti", "V", "Zn")

# check distributions -> most fit a lognormal distribution. This has implications for the calculated means, sd and correlations!
# alternatively, only log-transform parameters that follow lognormal distribution and leave other as is? 
d %>%
  filter(sampletype == "groundwater") %>%
  mutate(logvalue = log(value)) %>%
  ggplot(., aes(x = logvalue)) +
  geom_density() +
  facet_wrap(facets = "parameter", scales = "free") +
  theme_bw()

# only for groundwater samples
dat <- d %>%
  filter(sampletype == "groundwater") %>%
  filter(parameter %in% metals_cor) %>%
  mutate(logvalue = log(value)) %>%
  # select only samplecode and concentrations and convert to wide format
  select(samplecode, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value) %>%
  # select(samplecode, parameter, logvalue) %>%
  # pivot_wider(names_from = parameter,
  #             values_from = logvalue) %>%
  # set samplecodes as row names/index
  column_to_rownames(., var = "samplecode") 

# log transformed concentrations
dat_log <- d %>%
  #filter(sampletype == "groundwater") %>%
  filter(parameter %in% c("Al", "As", "B", "Ba", "Be",
                          "Ca", "Co", "Cr", "Cu", "Fe",
                          "K", "Li", "Mg", "Mn", "Mo", "Na", 
                          "Ni", "P", "Pb", "S", "Sb", "Se",
                          "Si", "Ti", "V", "Zn")) %>%
  mutate(logvalue = log(value)) %>%
  # select only samplecode and concentrations and convert to wide format
  select(samplecode, parameter, logvalue) %>%
  pivot_wider(names_from = parameter,
              values_from = logvalue) %>%
  # remove outliers? 
  #filter(!samplecode %in% c("GW021", "GW030")) %>%
  # set samplecodes as row names/index
  column_to_rownames(., var = "samplecode") %>%
  drop_na()

# correlation chart to see distribution, bivariate scatter plots and correlation values
#chart.Correlation(dat, histogram = T, pch=19)

# Correlation diagram
p_mat <- cor_pmat(dat, method = "spearman") # computes correlation matrix with p-values
corr <- cor(dat, use = "complete.obs", # computes correlation matrix   
            method = "spearman")  # use Spearman or Kendall for non-normal distributions
corrplot(corr, method = "circle", type = "lower", order = "hclust", 
         p.mat = p_mat, sig.level = 0.05, insig = "blank",
         tl.col = "black", tl.srt = 45)

#### HCA ####
# because observed values of each variable of samples have different orders of magnitude
# and different units, data transformation is necessary to obtain dimensionless data to
# avoid inefficient classification and improve classification accuracy. This is often done
# using Z-scores to standardize the data, leading to a mean of 0 and sd of 1.

# scale all columns to unit variance
dat_scale <- as.data.frame(scale(dat_log))
# build distance matrix / dissimilarity values
dist_mat <- dist(dat_scale, method = "euclidean")

## build cluster object
# clustering methods: 
# 1. single linkage and complete linkage are significantly affected by outliers! Not recommended
# 2. Median linkage and centroid linkage ... not recommended?
# 3. Average linkage (between-group and within-group) not easily affected by outliers. Performed
# well in clustering and recommended with large number of samples, complex variables
# 4. Ward's minimum-variance could capture and enlarge subtle differences between clusters.
# Especially effective with fewer objects and variables.

hclust_var <- hclust(dist_mat, method = "ward.D")
# standard plot dendrogram
plot(hclust_var, cex = 0.7, hang = -1,
     main = "HCA Ward's method", sub = NA, xlab = NA)
# match colours with later plots of clusters
# default ggplot colours:
hex_codes <- hue_pal()(4) # show_col(hex_codes) shows colours + codes
#abline(h = 16, col = "red", lty = "dashed")
rect.hclust(hclust_var, k = 4, border = c("#00BFC4", "#7CAE00", "#F8766D", "#C77CFF"))

# cut dendrogram into clusters
cut_var <- cutree(hclust_var, k = 4)
table(cut_var)

# visualise clusters in scatter plot (similar to PCA?)
p <- fviz_cluster(list(data = dat_log, cluster = cut_var),
                  main = "Ward's method clusters of main constituents")
p + theme_bw()

## Describing the clusters
# contributing parameters per group 
# add cluster column to log-transformed and standardized dataset
res <- dat_scale %>%
  mutate(cluster = cut_var)
# add parameter mean to each cluster
clus_mean <- res %>%
  pivot_longer(cols = names(res)[1]:names(res)[ncol(res)-1],
               names_to = "parameter",
               values_to = "value") %>%
  group_by(cluster, parameter) %>%
  dplyr::summarise(mean = mean(value))

# visualise relative contribution parameter to each cluster
ggplot(clus_mean, aes(x = parameter, y = mean, colour = as.character(cluster), 
                      group = as.character(cluster))) +
  geom_line() +
  xlab("") +
  ylab("mean of standardized concentrations") +
  labs(colour = "cluster") +
  theme_bw()

#### PCA ####
res.pca <- PCA(dat_log, graph = T, ncp = 8)
summary(res.pca)
# plot of eigenvalues
ggplot(data.frame(res.pca$eig), aes(x = 1:nrow(res.pca$eig), y = res.pca$eig[,1])) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 1) +
  theme_bw() +
  labs(x = "Components",
       y = "Eigenvalues")

#plot(res.pca, choix = "ind", habillage=5)
# get overview of which parameters relate significantly to which dimension
dimdesc(res.pca, axes = 1:6)

# draw elipses around 13th variable which is categorical
#plotellipses(res.pca, 13)



## using other PCA package

# extract and visualize eigenvalues/variances
get_eig(res.pca) # 5 dimensions with eigenvalue > 1, explaining 76% of variance
fviz_screeplot(res.pca, addlabels = T)

# Extract and visualize results for variables
var <- get_pca_var(res.pca)
var
# correlations between variables -> not all significant? 
show(var$cor)
# contribution of variables
show(var$contrib)
# default graph of variables
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE ) # Avoid text overlapping

#### Variable contributions to the principal axes
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC3
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)
# Contributions of variables to PC4
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10)
# Contributions of variables to PC5
fviz_contrib(res.pca, choice = "var", axes = 5, top = 10)


