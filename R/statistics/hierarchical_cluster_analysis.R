#
# Input: hydrochemical dataset of Curacao with metadata file
#         
# Output: Cluster analysis and plots
# 
# Dependencies: none
#
# source material: https://uc-r.github.io/hc_clustering 
#
# Author: Mike Wit
# Date: 15-07-2022
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, scales, cluster, factoextra, 
               dendextend, ape)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/" 

# load cleaned data
data <- openxlsx::read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx"))
metadata <- openxlsx::read.xlsx(paste0(input, "metadata_2021.xlsx"))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/Output/Statistics/HCA" 

###############################################################################
# Editing data
###############################################################################

## Select relevant parameters to include in cluster analysis
# Basic parameters
set <- c("pH", "EC_uS", "Na", "Ca", "K", "Fe", "Mg", "Si", "NH4", 
         "Cl", "HCO3", "SO4", "NO3", "PO4")

# metals excluding Ag and Cd as they are all <dl
metals <- c("Al", "As", "B", "Ba", "Be", "Ca", "Co", "Cr", "Cu", "Fe", "K", "Li", 
            "Mg", "Mn", "Mo", "Na", "Ni", "Pb", "Sb", "Se", "Si", "Ti", "V", "Zn")

# parameters to avoid
avoid <- c("NO2_field", "NO3_field", "Eh", "DO_sat", 
           "HCO3_field", "HCO3_lab", "Rn", "Ag", "Cd",
           "P", "S")

# select only 2021 and select relevant parameters from above
d <- data %>%
  filter(year == 2021, 
         !is.na(value),
         parameter %in% set) 

# add geology and land use
d <- d %>%
  left_join(., metadata %>% select(samplecode, geology, geology_abr, 
                                   landuse_zonal_map, Land.use.based.on.own.observations))

# place values in wide format for distance matrix
dat <- d %>%
  # only for groundwater
  #filter(sampletype == "groundwater") %>%
  # filter(!parameter %in% avoid,
  #        method != "IA") %>%
  # mutate(value = ifelse(parameter %in% c("E.coli", "DO"), value + 1, value)) %>%
  # log transform values due to non normal distribution ?
  # pH and isotopes should not be log transformed
  mutate(sel = ifelse(parameter == "pH" | method == "IA", 1, 0)) %>%
  #filter(sel == 0) %>%
  mutate(logvalue = ifelse(sel == 1, value, log(value))) %>%
  # select only samplecode and concentrations and convert to wide format
  # select(samplecode, parameter, value) %>%
  # pivot_wider(names_from = parameter,
  #             values_from = value) %>%
  select(samplecode, parameter, logvalue) %>%
  pivot_wider(names_from = parameter,
              values_from = logvalue) %>%
  # set samplecodes as row names/index
  column_to_rownames(., var = "samplecode") %>%
  # remove NA's
  drop_na()

###############################################################################
# Hierarchical Cluster Analysis
###############################################################################

### HCA
# because observed values of each variable of samples have different orders of magnitude
# and different units, data transformation is necessary to obtain dimensionless data to
# avoid inefficient classification and improve classification accuracy. This is often done
# using Z-scores to standardize the data, leading to a mean of 0 and sd of 1.

# scale all columns to unit variance
dat_scale <- as.data.frame(scale(dat))

# build distance matrix / dissimilarity values
dist_mat <- dist(dat_scale, method = "euclidean")

## Determine optimal nr of clusters
# Elbow method 
# location of breakpoint (knikpunt) is generally considered as an indicator of appropriate nr of clusters
fviz_nbclust(dat, FUN = hcut, method = "wss") # within-cluster sum of square

# Average Silhouette method
# measures the quality of clustering, i.e. how well each sample lies within its cluster
fviz_nbclust(dat, FUN = hcut, method = "silhouette")

# Gap Statisic method
# compares the total intracluster variation for different values of k with their expected values under
# null reference distribution of the data (i.e. distribution with no obvious clustering)
gap_stat <- clusGap(dat, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

## build cluster object
# clustering methods: 
# 1. single linkage and complete linkage are significantly affected by outliers! Not recommended
# 2. Median linkage and centroid linkage ... not recommended?
# 3. Average linkage (between-group and within-group) not easily affected by outliers. Performed
# well in clustering and recommended with large number of samples, complex variables
# 4. Ward's minimum-variance could capture and enlarge subtle differences between clusters.
# Especially effective with fewer objects and variables. -> most suited for hydrochemical data

hclust_avg <- hclust(dist_mat, method = "average")
hclust_var <- hclust(dist_mat, method = "ward.D")
# standard plot dendrogram
plot(hclust_var, cex = 0.7, hang = -1,
     main = "HCA Ward's method", sub = NA, xlab = NA)
# clusters for main constituents 
# colours
# colour blind friendly:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# default ggplot colours:
hex_codes <- hue_pal()(5) # show_col(hex_codes) shows colours + codes
abline(h = 16, col = "red", lty = "dashed")
rect.hclust(hclust_var, k = 5, border = c("#00BF7D", "#00B0F6", "#A3A500", "#E76BF3", "#F8766D"))
# abline(h = 12, col = "green", lty = "dashed")
# rect.hclust(hclust_var, k = 6, border = 2:6)

# # Other plot possibilities 
# plot(as.phylo(hclust_var), cex = 0.6, label.offset = 0.5)
# # fan plot
# colors <- c("red", "blue", "green", "orange", "black")
# plot(as.phylo(hclust_var), cex = 0.6, label.offset = 0.5,
#      type = "fan", tip.color = colors[cut_var])

# for metals

## Checking clusters
# cut dendrogram into clusters
cut_var <- cutree(hclust_var, k = 5)
table(cut_var)

# visualise clusters in scatter plot (similar to PCA?)
p <- fviz_cluster(list(data = dat, cluster = cut_var),
                  main = "Ward's method clusters of main constituents")
p + theme_bw()

## comparing 2 clustering methods
dend1 <- as.dendrogram(hclust_var)
dend2 <- as.dendrogram(hclust_avg)

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = F, # turn off dashed lines
           common_subtrees_color_lines = F, # turn off line colors
           common_subtrees_color_branches = T, # color common branches
           main = paste("entanglement =", round(entanglement(dend_list), 2)) # paste entanglement measure between 1-0
           )

## Describing the clusters
# contributing parameters per group 

# add cluster column to log-transformed and standardized dataset
res <- dat_scale %>%
  mutate(cluster = cut_var)

clus_mean <- dat_scale %>%
  aggregate(by = list(cut_var), 
            FUN = mean) 
clus_mean

clus_mean <- res %>%
  #rename(EC = EC_uS) %>%
  # pivot_longer(cols = Ca:SO4,
  #              names_to = "parameter",
  #              values_to = "value") %>%
  pivot_longer(cols = names(res)[1]:names(res)[ncol(res)-1],
               names_to = "parameter",
               values_to = "value") %>%
  group_by(cluster, parameter) %>%
  summarise(mean = mean(value))

ggplot(clus_mean, aes(x = parameter, y = mean, colour = as.character(cluster), 
                      group = as.character(cluster))) +
  geom_line() +
  xlab("") +
  ylab("mean of standardized concentrations") +
  labs(colour = "cluster") +
  theme_bw()

###############################################################################
# Save results
###############################################################################

















