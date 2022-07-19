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

# select only 2021 and select relevant parameters from above
d <- data %>%
  filter(year == 2021, 
         !is.na(value),
         parameter %in% metals) 

# add geology and land use
d <- d %>%
  left_join(., metadata %>% select(samplecode, geology, geology_abr, 
                                   landuse_zonal_map, Land.use.based.on.own.observations))

# place values in wide format for distance matrix
dat <- d %>%
  # only for groundwater
  #filter(sampletype == "groundwater") %>%
  # log transform values due to non normal distribution ?
  mutate(logvalue = ifelse(parameter != "pH", log(value), value)) %>%
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
plot(hclust_var, cex = 0.7, hang = -1)
# clusters for main constituents 
abline(h = 16, col = "red", lty = "dashed")
rect.hclust(hclust_var, k = 4, border = 2:6)
# abline(h = 12, col = "green", lty = "dashed")
# rect.hclust(hclust_var, k = 6, border = 2:6)

# 
plot(as.phylo(hclust_var), cex = 0.6, label.offset = 0.5)
# fan plot
colors <- c("red", "blue", "green", "orange", "black")
plot(as.phylo(hclust_var), cex = 0.6, label.offset = 0.5,
     type = "fan", tip.color = colors[cut_var])

# for metals

## Checking clusters
# cut dendrogram into clusters
cut_var <- cutree(hclust_var, k = 5)
table(cut_var)

# visualise clusters in scatter plot (similar to PCA?)
p <- fviz_cluster(list(data = dat, cluster = cut_var))
p + theme_bw()

# add cluster column to dataset
dat %>%
  mutate(cluster = cut_var) %>%
  head()

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

###############################################################################
# Save results
###############################################################################
















