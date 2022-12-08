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
# Basic parameters, remove EC? 
set <- c("pH", "DO", "Na", "Ca", "Mg", "K", "Si", "Fe", "Mn", "NH4", 
         "Cl", "HCO3", "SO4", "NO3", "PO4", "Br")

# metals excluding Ag and Cd as they are all <dl
metals <- c("Al", "As", "B", "Ba", "Be", "Ca", "Co", "Cr", "Cu", "Fe", "K", "Li", 
            "Mg", "Mn", "Mo", "Na", "Ni", "Pb", "Sb", "Se", "Si", "Ti", "V", "Zn")

# parameters to avoid
avoid <- c("NO2_field", "NO3_field", "Eh", "DO_sat", 
           "HCO3_field", "HCO3_lab", "Rn", "Ag", "Cd",
           "P", "S")

# add DO for GW021 and RW002 and Mn for SEA001
d <- data %>%
  # use average DO+pH concentration of RW001 and RW003 for RW002
  mutate(value = ifelse(samplecode == "RW002" & parameter == "DO", 7.517, value)) %>%
  mutate(value = ifelse(samplecode == "RW002" & parameter == "pH", 6.3, value)) %>%
  # use Mn concentration from SEA002 for SEA001
  mutate(value = ifelse(samplecode == "SEA001" & parameter == "Mn", 1.82, value)) %>%
  # use average DO+pH concentration from KG geology
  mutate(value = ifelse(samplecode == "GW021" & parameter == "DO", 2.31, value)) %>%
  mutate(value = ifelse(samplecode == "GW021" & parameter == "pH", 7.10, value))

# select only 2021 and select relevant parameters from above
d <- d %>%
  filter(year == 2021, 
         !is.na(value),
         parameter %in% set) %>%
  select(samplecode, parameter, value)

# average values for locations with 2 samples on the same location
d_set <- d %>%
  filter(samplecode %in% c("GW014A", "GW014B", "GW020A", "GW020B", "GW035A", "GW035B", 
                           "GW040A", "GW040B", "GW053A", "GW053B", "GW055A", "GW055B", 
                           "GW060A", "GW060B")) %>%
  mutate(code = str_sub(samplecode, -1)) %>%
  mutate(samplecode = str_sub(samplecode, start = 1, end = 5)) %>%
  select(samplecode, code, parameter, value) %>%
  pivot_wider(names_from = code,
              names_glue = "{.value}_{code}",
              values_from = c(value)) %>%
  mutate(value = (value_A + value_B) / 2) %>%
  select(samplecode, parameter, value)

# combine averaged concentrations with dataset
d <- d %>%
  filter(!samplecode %in% c("GW014A", "GW014B", "GW020A", "GW020B", "GW035A", "GW035B", 
                            "GW040A", "GW040B", "GW053A", "GW053B", "GW055A", "GW055B", 
                            "GW060A", "GW060B")) %>%
  rbind(., d_set)

# add geology and land use
d <- d %>%
  left_join(., metadata %>% select(samplecode, geology, geology_abr, 
                                   landuse_zonal_map, Land.use.based.on.own.observations))

# place values in wide format for distance matrix
dat_log <- d %>%
  # only for groundwater
  #filter(sampletype == "groundwater") %>%
  # filter(!parameter %in% avoid,
  #        method != "IA") %>%
  # mutate(value = ifelse(parameter %in% c("E.coli", "DO"), value + 1, value)) %>%
  # log transform values due to non normal distribution ?
  # pH and isotopes should not be log transformed
  mutate(sel = ifelse(parameter %in% c("pH", "DO", "Si", "NO3"), 1, 0)) %>%
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
  column_to_rownames(., var = "samplecode") 
  # remove NA's
  #drop_na()

dat <- d %>%
  # only for groundwater
  #filter(sampletype == "groundwater") %>%
  # filter(!parameter %in% avoid,
  #        method != "IA") %>%
  # mutate(value = ifelse(parameter %in% c("E.coli", "DO"), value + 1, value)) %>%
  # select only samplecode and concentrations and convert to wide format
  select(samplecode, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value) %>%
  # set samplecodes as row names/index
  column_to_rownames(., var = "samplecode") 
# remove NA's
#drop_na()



###############################################################################
# Hierarchical Cluster Analysis
###############################################################################

### HCA
# because observed values of each variable of samples have different orders of magnitude
# and different units, data transformation is necessary to obtain dimensionless data to
# avoid inefficient classification and improve classification accuracy. This is often done
# using Z-scores to standardize the data, leading to a mean of 0 and sd of 1.

# scale all columns to unit variance
dat_scale <- as.data.frame(scale(dat_log))

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

# plot with dendextend
dend <- as.dendrogram(hclust_var)
dend %>%
  set("labels_col", value = c("skyblue", "orange", "grey", "#7CAE00"), k=4) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey", "#7CAE00"), k = 4) %>%
  plot(horiz=TRUE)
abline(v = 18, lty = 2)

# Used!! 
png(paste0(output, "/HCA_allsamples_4clusters.png"),
    width = 960, height = 480)
dend %>%
  # 4 clusters
  # set("labels_col", value = c("skyblue", "orange", "grey", "#7CAE00", "#F8766D"), k =4) %>%
  # set("branches_k_color", value = c("skyblue", "orange", "grey", "#7CAE00", "#F8766D"), k = 4) %>%
  # 7 subclusters
  set("labels_col", value = c("skyblue", "orange", "darkorange3", "grey", "azure4", "#7CAE00", "forestgreen", "#F8766D"), k = 7) %>%
  set("branches_k_color", value = c("skyblue", "orange", "darkorange3",  "grey", "azure4", "#7CAE00", "forestgreen", "#F8766D"), k = 7) %>%
  plot()
abline(h = 25, lty = 2)
abline(h = 13, lty = 2)
dev.off()

# clusters for main constituents 
# colours
# colour blind friendly:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# default ggplot colours:
hex_codes <- hue_pal()(4) # show_col(hex_codes) shows colours + codes
abline(h = 16, col = "red", lty = "dashed")
rect.hclust(hclust_var, k = 4, border = c("#00BF7D", "#00B0F6", "#A3A500", "#E76BF3", "#F8766D"))
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
cut_var <- cutree(hclust_var, k = 4) 
cut_subvar <- cutree(hclust_var, k = 7)
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

# alternatively, add clusters to dataset of original concentrations and make 
# table with means per cluster per parameter
clus_mean <- dat %>%
  aggregate(by = list(cut_var), 
            FUN = mean) 

d <- dat %>%
  cbind(samplecode = rownames(.)) %>%
  left_join(data %>% filter(year == 2021, parameter == "EC_uS") %>% select(samplecode, value)) %>%
  rename(EC = value) %>%
  select(samplecode, everything())

res <- d %>%
  mutate(x = cut_var) %>%
  mutate(xsub = cut_subvar) %>%
  # adjust cluster numbers to match dendrogram order
  mutate(cluster = case_when(
    x == 1 ~ 2,
    x == 2 ~ 4,
    x == 3 ~ 3,
    x == 4 ~ 1,
    TRUE ~ 99)) %>%
  # adjust subcluster numbers to match dendrogram order
  mutate(subcluster = case_when(
    xsub == 1 ~ "2A",
    xsub == 2 ~ "4B",
    xsub == 3 ~ "3B",
    xsub == 4 ~ "4A",
    xsub == 5 ~ "2B",
    xsub == 6 ~ "1",
    xsub == 7 ~ "3A",
    TRUE ~ "99")) %>%
  select(-c(x, xsub)) %>%
  # put into long format to perform analysis
  pivot_longer(cols = Br:EC,
               names_to = "parameter",
               values_to = "value") %>%
  # add EC values for missing samples
  mutate(value = case_when(
    samplecode == "GW014" & parameter == "EC" ~ 2386,
    samplecode == "GW020" & parameter == "EC" ~ 5639,
    samplecode == "GW035" & parameter == "EC" ~ 4565,
    samplecode == "GW040" & parameter == "EC" ~ 1682,
    samplecode == "GW053" & parameter == "EC" ~ 5710,
    samplecode == "GW055" & parameter == "EC" ~ 2630,
    samplecode == "GW060" & parameter == "EC" ~ 1291,
    samplecode == "RW002" & parameter == "EC" ~ 92, 
    TRUE ~ value))

clus_mean <- res %>%
  group_by(cluster, parameter) %>%
  summarise(n=n(),
            mean = mean(value)) %>%
  pivot_wider(names_from = parameter,
              values_from = mean)
write.xlsx(clus_mean, paste0(output, "/cluster_mean.xlsx"))

subcluster_mean <- res %>%
  group_by(subcluster, parameter) %>%
  summarise(n=n(),
            mean = mean(value)) %>%
  pivot_wider(names_from = parameter,
              values_from = mean)
write.xlsx(subcluster_mean, paste0(output, "/subcluster_mean.xlsx"))
  
ggplot(res, aes(x = parameter, y = mean, fill = as.character(cluster))) + 
  geom_col(position = "dodge") 
  facet_wrap(facets = "cluster", scales = "free")

# export dataset with coordinates and clusters to visualize on GIS map
d <- res %>%
  pivot_wider(names_from = parameter) %>%
  left_join(metadata %>% 
              select(samplecode, xcoord, ycoord) %>%
              mutate(samplecode = ifelse(samplecode %in% c("SEA001", "SEA002", "SW003A", "SW003B"), 
                                         samplecode, str_sub(samplecode, start = 1, end = 5))) %>%
              distinct()) 

write.csv(d, paste0(output, "/clusters_hydrochemical2021.csv"))

###############################################################################
# Save results
###############################################################################

















