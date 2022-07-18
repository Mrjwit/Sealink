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
pacman::p_load(tidyverse, openxlsx, scales, ggtext, ggcorrplot,
               corrplot, FactoMineR)

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
  annotation_logticks(sides = "lr") +
  labs(title = "All metals in all sample types, 2021")
ggsave(paste0(output, "figures/metals/boxplot_all_metals_ugl.png"), dpi = 1000)

# metals differentiating between sample types


# only trace metals in all samples
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
  labs(title = "Trace metals in all sample types, 2021")
ggsave(paste0(output, "figures/metals/boxplot_trace_metals_ugl.png"), dpi = 1000)

# all metals in groundwater
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
ggsave(paste0(output, "figures/metals/boxplot_all_metals_gw_ugl.png"), dpi = 1000)

# only trace/heavy metals in groundwater
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
  labs(title = "Trace metals in groundwater, 2021")
ggsave(paste0(output, "figures/metals/boxplot_trace_metals_gw_ugl.png"), dpi = 1000)

#### Correlation diagram ####
# leave Ag, Be, Cd, Sb
metals_cor <- c("Al", "As", "B", "Ba", 
                "Ca", "Co", "Cr", "Cu", "Fe",
                "K", "Li", "Mg", "Mn", "Mo", "Na", 
                "Ni", "P", "Pb", "S", "Se",
                "Si", "Ti", "V", "Zn")
# alternative selection without Na, Ca, Mg
metals_cor <- c("Al", "As", "B", "Ba", 
                "Co", "Cr", "Cu", "Fe",
                "K", "Li", "Mn", "Mo",  
                "Ni", "P", "Pb", "S", "Se",
                "Si", "Ti", "V", "Zn")

# check distributions -> most fit a lognormal distribution. This has implications for the calculated means and sd and correlations!
d %>%
  filter(sampletype == "groundwater") %>%
  mutate(logvalue = log(value)) %>%
  ggplot(., aes(x = logvalue)) +
  geom_density() +
  facet_wrap(facets = "parameter", scales = "free") +
  theme_bw()

# only for groundwater
dat <- d %>%
  filter(sampletype == "groundwater") %>%
  filter(parameter %in% metals_cor) %>%
  mutate(logvalue = log(value)) %>%
  # select only samplecode and concentrations and convert to wide format
  select(samplecode, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value) %>%
  #select(samplecode, parameter, logvalue) %>%
  # pivot_wider(names_from = parameter,
  #             values_from = logvalue) %>%
  # remove outliers? 
  #filter(!samplecode %in% c("GW021", "GW030")) %>%
  select(-samplecode)

# Correlation diagram
p_mat <- cor_pmat(dat) # computes correlation matrix with p-values
corr <- cor(dat, use = "complete.obs", # computes correlation matrix   
            method = "spearman")  # use Spearman or Kendall for non normal distributions
corrplot(corr, method = "circle", type = "lower", order = "hclust", 
         p.mat = p_mat, sig.level = 0.05, insig = "blank",
         tl.col = "black", tl.srt = 45)

# correlation chart to see distribution, bivariate scatter plots and correlation values
chart.Correlation(dat, histogram = T, pch=19)

# PCA
res.pca <- PCA(dat, graph = T)
summary(res.pca)
# plot of eigenvalues
ggplot(data.frame(res.pca$eig), aes(x = 1:nrow(res.pca$eig), y = res.pca$eig[,1])) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 1) +
  theme_bw() +
  labs(x = "Components",
       y = "Eigenvalues")

plot(res.pca, choix = "ind", habillage=2)
# which parameters relate significantly to which dimension
dimdesc(res.pca, axes = 1:2)

# draw elipses around 13th variable which is categorical
plotellipses(res.pca, 13)



# using other package

# extract and visualize eigenvalues/variances
get_eig(res.pca) # 5 dimensions with eigenvalue > 1, explaining 76% of variance
fviz_screeplot(res.pca, addlabels = T)

# Extract and visualize results for variables
var <- get_pca_var(res.pca)
var
# correlations between variables
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



#### PCA metals ####



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

data %>%
  filter(year == 2021, sampletype != "air", method == "ICP-MS") %>%
  group_by(parameter, method, units) %>%
  summarise(detetion_limit = detection_limit) %>%
  distinct() %>%
  view()

