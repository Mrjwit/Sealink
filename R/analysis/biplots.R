#
# Input: all cleaned datafiles
#         
# Output: Biplots of hydrochemical samples
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 05-09-2023
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, cowplot, scales)
# pacman::p_load(tidyverse, openxlsx, ggmap, devtools,scales,
#                sf, cowplot, scales, corrplot, Hmisc, ggpubr,
#                ggbiplot, ggcorrplot, psych, FactoMineR, polynom)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/final_merged/" 

# load cleaned data
data <- openxlsx::read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx"))
metadata <- openxlsx::read.xlsx(paste0(input, "metadata_2021_2022_2023.xlsx"))
meta_2020 <- openxlsx::read.xlsx(paste0(input, "metadata_2020.xlsx"))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# Editing data
###############################################################################

# create plot theme
theme_mw <- function(){
  font <- "Georgia"
  theme_bw() %+replace% # use theme_bw as base and replace elements 
    
    theme(
      #text elements
      plot.title = element_text(
        family = font,
        size = 14,
        face = 'bold',
        hjust = 0,
        vjust = 2),
      plot.subtitle = element_text(
        family = font,
        size = 12,
        hjust = 0,
        vjust = 2),
      axis.title = element_text(
        family = font,
        size = 11),
      axis.text = element_text(
        family = font,
        size = 10),
      legend.title = element_text(
        family = font,
        size = 12),
      legend.text = element_text(
        family = font,
        size = 11)
    )
}

# use dataset of concentrations converted to meq/l
an <- c("Cl", "HCO3", "NO3", "PO4", "SO4")
cat <- c("Na", "Ca", "Mg", "Fe", "K", "NH4")

d_meql <- data %>%
  filter(year > 2020, 
         parameter %in% c(an, cat, "EC"),
         samplecode != "SP001B",
         sampletype %in% c("groundwater", "seawater")) %>%
  # remove PO4 with IC measurements
  mutate(remove = ifelse(method == "IC" & parameter == "PO4", 1, 0)) %>%
  filter(remove == 0,
         !units %in% c("mg N/L", "mg P/L")) %>%                    
  # change mg/l to meq/l for anions
  mutate(meql = case_when(
    parameter == "Cl" ~ value / 35.453,
    parameter == "HCO3" ~ value / 61.0168,
    parameter == "NO3" ~ value / 62.0049, # parameter == "NO3" & limit_symbol != "<" ~ value / 62.0049, # dl NO3 = 0.03 mg/l = 0.0004838327 meq/l
    parameter == "NO2" ~ value / 46.0055,
    parameter == "PO4" ~ value / 94.9714 * 3,
    parameter == "SO4" ~ value / 96.06 * 2,
    parameter == "Br" ~ value / 79.904,
    parameter == "F" ~ value / 18.998403,
    parameter == "EC" ~ value,
    TRUE ~ NA_real_ )) %>%
  mutate(meql = case_when(
    parameter == "Na" ~ value / 22.989769,
    parameter == "Ca" ~ value / 40.078 * 2,
    parameter == "Mg" ~ value / 24.305 * 2,
    parameter == "Fe" ~ value / 55.845 * 2,
    parameter == "Mn" ~ value / 54.938044 * 2 / 1000,
    parameter == "K" ~ value / 39.0983,
    parameter == "NH4" ~ value / 18.04, # parameter == "NH4" & limit_symbol != "<" ~ value / 18.04, # dl NH4 = 0.053 mg/l (NH41) or 0.258 m/gl (NH410) = 0.002937916 meq/l / 0.01430155 meq/l
    TRUE ~ meql )) %>%
  dplyr::select(samplecode, year, parameter, meql, sampletype) %>%
  pivot_wider(names_from = parameter,
              values_from = meql) %>%
  filter(!is.na(Cl))

# add metadata
d_meql <- d_meql %>%
  left_join(., metadata %>% dplyr::select(samplecode, `Well.depth.below.surface.(m)`, 
                                 `Groundwater.level.below.surface.(m)`,
                                 sample_depth,
                                 geology, geology_abr, landuse_zonal_map, dist.coast, 
                                 geozone, geoz_pop, geoz_dens, geoz_house, geoz_avg_house, 
                                 neigh, neigh_dens)) %>% # elevation left out because not in dataset?
  #mutate(geology = ifelse(geology == "Knip Group - intrusive", "Knip Group", geology)) %>%
  mutate(geology = ifelse(geology == "Other" & sampletype == "groundwater", "Knip Group - intrusive", geology))

# seawater avg concentrations
sw <- d_meql %>%
  filter(sampletype == "seawater") %>%
  dplyr::select(Ca:SO4) %>%
  colMeans(.)

#### Biplots ####
## Plots in meq/l 

# Plots against Cl
# Na vs Cl
y1 <- sw[10]
y0 <- 0
x1 <- sw[2]
x0 <- 0
# for linear
m <- (y1 - y0) / (x1 - x0)
b <- y1 - m*x1
# for linear line in log-log plot so that it doesnt plot through (1,1)
dat <- data.frame(x = c(1E-20, x1),
                  y = c(1E-20*m, y1))

# Halite dissolution ? --> Na depletion/enrichment --> cation exchange --> freshening/salinization?
p1 <- ggplot(d_meql %>% filter(sampletype == "groundwater"), aes(x = Cl, y = Na)) +
  geom_point(aes(fill = geology), size = 2, pch = 21, alpha = 0.7) +
  # add avg seawater Curacao
  geom_point(aes(x = sw[2], y = sw[10]), shape = 23, fill = "red", size = 4) +
  # add seawater ratio mixing line
  geom_line(data = dat, aes(x = x, y = y), linetype = "dashed", colour = "black") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  coord_cartesian(xlim = c(0.5, 1000),
                  ylim = c(0.5, 1000)) +
  scale_fill_manual(name = "Geology",
                      values = c("darkolivegreen3", "darkgreen",
                                 "lightskyblue3", "purple4", "firebrick",
                                 "gold"),
                      labels = c("Diabase E", "Diabase W",
                                 "Sed. rocks (MCF)", "Complex sed. rocks (KG)", "Intrusion (KG)",
                                 "Limestones")) +
  labs(x = expression('Cl'^'-'~'(meq/L)'),
       y = expression('Na'^'+'~'(meq/L)'),
       title = "Na vs Cl") +
  theme_mw() +
  theme(#panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position = "none") +
  annotation_logticks(sides = "bl")

# K vs Cl
y1 <- sw[6]
y0 <- 0
x1 <- sw[2]
x0 <- 0
# for linear
m <- (y1 - y0) / (x1 - x0)
b <- y1 - m*x1
# for linear line in log-log plot so that it doesnt plot through (1,1)
dat <- data.frame(x = c(1E-20, x1),
                  y = c(1E-20*m, y1))
# K is largely removed --> fixation of K by illite clays (weathering product of diabase)
p2 <- ggplot(d_meql %>% filter(sampletype == "groundwater"), aes(x = Cl, y = K)) +
  geom_point(aes(fill = geology), size = 2, pch = 21, alpha = 0.7) +
  # add avg seawater Curacao
  geom_point(aes(x = sw[2], y = sw[6]), shape = 23, fill = "red", size = 4) +
  # add seawater ratio mixing line
  geom_line(data = dat, aes(x = x, y = y), linetype = "dashed", colour = "black") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10"
                     #breaks = trans_breaks("log10", function(x) 10^x),
                     #labels = trans_format("log10", math_format(10^.x))
  ) +
  coord_cartesian(xlim = c(0.5, 1000),
                  ylim = c(0.001, round(max(d_meql$K),0))) +
  scale_fill_manual(name = "Geology",
                      values = c("darkolivegreen3", "darkgreen",
                                 "lightskyblue3", "purple4", "firebrick",
                                 "gold"),
                      labels = c("Diabase E", "Diabase W",
                                 "Sed. rocks (MCF)", "Complex sed. rocks (KG)", "Intrusion (KG)",
                                 "Limestones")) +
  labs(x = expression('Cl'^'-'~'(meq/L)'),
       y = expression('K'^'+'~'(meq/L)'),
       title = "K vs Cl") +
  theme_mw() +
  theme(#panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position = "none") +
  annotation_logticks(sides = "bl")

# Ca vs Cl
y1 <- sw[1]
y0 <- 0
x1 <- sw[2]
x0 <- 0
# for linear
m <- (y1 - y0) / (x1 - x0)
b <- y1 - m*x1
# for linear line in log-log plot so that it doesnt plot through (1,1)
dat <- data.frame(x = c(1E-20, x1),
                  y = c(1E-20*m, y1))
# Ca enrichment due to calcite dissolution and cation exchange
p3 <- ggplot(d_meql %>% filter(sampletype == "groundwater"), aes(x = Cl, y = Ca)) + 
  geom_point(aes(fill = geology), size = 2, pch = 21, alpha = 0.7) +
  # add avg seawater Curacao
  geom_point(aes(x = sw[2], y = sw[1]), shape = 23, fill = "red", size = 4) +
  # add seawater ratio mixing line
  geom_line(data = dat, aes(x = x, y = y), linetype = "dashed", colour = "black") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  coord_cartesian(xlim = c(0.5, 1000),
                  ylim = c(0.5, round(max(d_meql$Ca),0))) +
  scale_fill_manual(name = "Geology",
                      values = c("darkolivegreen3", "darkgreen",
                                 "lightskyblue3", "purple4", "firebrick",
                                 "gold"),
                      labels = c("Diabase E", "Diabase W",
                                 "Sed. rocks (MCF)", "Complex sed. rocks (KG)", "Intrusion (KG)",
                                 "Limestones")) +
  labs(x = expression('Cl'^'-'~'(meq/L)'),
       y = expression('Ca'^'2+'~'(meq/L)'),
       title = "Ca vs Cl") +
  theme_mw() +
  theme(#panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position = "none") +
  annotation_logticks(sides = "bl")

# Mg vs Cl
y1 <- sw[7]
y0 <- 0
x1 <- sw[2]
x0 <- 0
# for linear
m <- (y1 - y0) / (x1 - x0)
b <- y1 - m*x1
# for linear line in log-log plot so that it doesnt plot through (1,1)
dat <- data.frame(x = c(1E-20, x1),
                  y = c(1E-20*m, y1))
# Mg enrichment --> dolomite dissolution, dissolution of silicates: olivines, pyroxenes and hornblende? Cation-exchange? but less than Ca enrichment
p4 <- ggplot(d_meql %>% filter(sampletype == "groundwater"), aes(x = Cl, y = Mg)) + 
  geom_point(aes(fill = geology), size = 2, pch = 21, alpha = 0.7) +
  # add avg seawater Curacao
  geom_point(aes(x = sw[2], y = sw[7]), shape = 23, fill = "red", size = 4) +
  # add seawater ratio mixing line
  geom_line(data = dat, aes(x = x, y = y), linetype = "dashed", colour = "black") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  coord_cartesian(xlim = c(0.5, 1000),
                  ylim = c(0.5, round(max(d_meql$Mg),0))) +
  scale_fill_manual(name = "Geology",
                      values = c("darkolivegreen3", "darkgreen",
                                 "lightskyblue3", "purple4", "firebrick",
                                 "gold"),
                      labels = c("Diabase E", "Diabase W",
                                 "Sed. rocks (MCF)", "Complex sed. rocks (KG)", "Intrusion (KG)",
                                 "Limestones")) +
  labs(x = expression('Cl'^'-'~'(meq/L)'),
       y = expression('Mg'^'2+'~'(meq/L)'),
       title = "Mg vs Cl") +
  theme_mw() +
  theme(#panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position = "none") +
  annotation_logticks(sides = "bl")

# HCO3 vs Cl
y1 <- sw[5]
y0 <- 0
x1 <- sw[2]
x0 <- 0
# for linear
m <- (y1 - y0) / (x1 - x0)
b <- y1 - m*x1
# for linear line in log-log plot so that it doesnt plot through (1,1)
dat <- data.frame(x = c(1E-20, x1),
                  y = c(1E-20*m, y1))
# Elevated HCO3 due to dissolution
p5 <- ggplot(d_meql %>% filter(sampletype == "groundwater"), aes(x = Cl, y = HCO3)) + 
  geom_point(aes(fill = geology), size = 2, pch = 21, alpha = 0.7) +
  # add avg seawater Curacao
  geom_point(aes(x = sw[2], y = sw[5]), shape = 23, fill = "red", size = 4) +
  # add seawater ratio mixing line
  geom_line(data = dat, aes(x = x, y = y), linetype = "dashed", colour = "black") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  coord_cartesian(xlim = c(0.5, 1000),
                  ylim = c(0.5, round(max(d_meql$HCO3),0))) +
  scale_fill_manual(name = "Geology",
                      values = c("darkolivegreen3", "darkgreen",
                                 "lightskyblue3", "purple4", "firebrick",
                                 "gold"),
                      labels = c("Diabase E", "Diabase W",
                                 "Sed. rocks (MCF)", "Complex sed. rocks (KG)", "Intrusion (KG)",
                                 "Limestones")) +
  labs(x = expression('Cl'^'-'~'(meq/L)'),
       y = expression(HCO[3]^'-'~'(meq/L)'),
       title = "HCO3 vs Cl") +
  theme_mw() +
  theme(#panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position = "none") +
  annotation_logticks(sides = "bl")

# SO4 vs Cl
y1 <- sw[12]
y0 <- 0
x1 <- sw[2]
x0 <- 0
# for linear
m <- (y1 - y0) / (x1 - x0)
b <- y1 - m*x1
# for linear line in log-log plot so that it doesnt plot through (1,1)
dat <- data.frame(x = c(1E-20, x1),
                  y = c(1E-20*m, y1))
# Elevated SO4 ..
p6 <- ggplot(d_meql %>% filter(sampletype == "groundwater"), aes(x = Cl, y = SO4)) + 
  geom_point(aes(fill = geology, shape = sampletype), size = 2, pch = 21, alpha = 0.7) +
  # add avg seawater Curacao
  geom_point(aes(x = sw[2], y = sw[12]), shape = 23, fill = "red", size = 4) +
  # add seawater ratio mixing line
  geom_line(data = dat, aes(x = x, y = y), linetype = "dashed", colour = "black") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  coord_cartesian(xlim = c(0.5, 1000),
                  ylim = c(0.005, 100)) +
  scale_fill_manual(name = "Geology",
                      values = c("darkolivegreen3", "darkgreen",
                                 "lightskyblue3", "purple4", "firebrick",
                                 "gold"),
                      labels = c("Diabase E", "Diabase W",
                                 "Sed. rocks (MCF)", "Complex sed. rocks (KG)", "Intrusion (KG)",
                                 "Limestones")) +
  labs(x = expression('Cl'^'-'~'(meq/L)'),
       y = expression(SO[4]^'2-'~'(meq/L)'),
       title = "SO4 vs Cl") +
  theme_mw() +
  theme(#panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position = "none") +
  annotation_logticks(sides = "bl")

p7 <- ggplot(d_meql %>% filter(sampletype == "groundwater"), aes(x = Cl, y = SO4)) + 
  geom_point(aes(fill = geology), size = 2, pch = 21, alpha = 0.7) +
  # add avg seawater Curacao
  geom_point(aes(x = x1, y = y1), shape = 23, fill = "red", size = 4) +
  # add seawater ratio mixing line
  geom_line(data = dat, aes(x = x, y = y), linetype = "dashed", colour = "black") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  coord_cartesian(xlim = c(0.5, 1000),
                  ylim = c(0.001, 100)) +
  scale_fill_manual(name = "Geology",
                      values = c("darkolivegreen3", "darkgreen",
                                 "lightskyblue3", "purple4", "firebrick",
                                 "gold"),
                      labels = c("Diabase East", "Diabase West",
                                 "Turbidite sequence (MCF)", "Complex sedimentary rocks (KG)", "Intrusion (KG)",
                                 "Limestones")) +
  # scale_shape_discrete(name = "",
  #                      labels = c("groundwater", "seawater")) +
  theme_mw() +
  theme(#panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    # legend.direction = "horizontal",
    legend.box = "vertical"
    #legend.text=element_text(size=2.5)
  ) +
  guides(shape = "none") +
  annotation_logticks(sides = "bl")

grobs <- ggplotGrob(p7)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

# Combine Cl plots
p_graph <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6,  
                              ncol = 2, labels = "AUTO")
cowplot::plot_grid(p_graph, legend, nrow = 2, rel_heights = c(1, 0.1))
ggsave(paste0(output, "Output/Figures/Hydrochemistry/Biplots/Biplots_Cl.png"),
       width = 8, height = 8)


# Alkalis Na+K vs Ca+Mg
# Enrichment of Ca+Mg with respect to Na+K --> cation exchange
p1 <- ggplot(d_meql %>% filter(sampletype == "groundwater"), aes(x = Na+K, y = Ca+Mg)) + 
  geom_point(aes(fill = geology), size = 2, pch = 21, alpha = 0.8) +
  geom_abline(linetype = "dashed", colour = "steelblue") +
  geom_abline(slope = 2, linetype = "dashed", colour = "grey50") +  
  geom_abline(slope = 0.5, linetype = "dashed", colour = "grey50") +
  geom_abline(slope = (sw[1]+sw[7])/(sw[10]+sw[6]),linetype = "dashed", colour = "red") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  scale_fill_manual(name = "Geology",
                    values = c("darkolivegreen3", "darkgreen",
                               "lightskyblue3", "purple4", "firebrick",
                               "gold"),
                    labels = c("Diabase East", "Diabase West",
                               "Turbidite sequence (MCF)", "Complex sedimentary rocks (KG)", "Intrusion (KG)",
                               "Limestones")) +
  labs(x = expression('Na'^'+'~+~'K'^'+'~'(meq/L)'),
       y = expression('Ca'^'2+'~+~'Mg'^'2+'~'(meq/L)'),
       title = "(Na+K) vs (Ca+Mg)") +
  annotate(geom = "text", x = 180, y = 220, label = "1:1 line", angle = 45, colour = "steelblue") + 
  annotate(geom = "text", x = 230, y = 5.5, label = "seawater mixing line" , angle = 14, colour = "red") + 
  theme_mw() +
  theme(legend.position = "none") +
  annotation_logticks(sides = "bl")

# Ca+Mg vs HCO3+SO4
# influence of carbonate mineral dissolution (calcite/dolomite)
p2 <- ggplot(d_meql %>% filter(sampletype == "groundwater"), aes(x = Ca+Mg, y = HCO3+SO4)) + 
  geom_point(aes(fill = geology), size = 2, pch = 21, alpha = 0.8) +
  geom_abline(linetype = "dashed", colour = "steelblue") +
  geom_abline(slope = (sw[5]+sw[12])/(sw[1]+sw[7]),linetype = "dashed", colour = "red") +
  annotate(geom = "text", x = 25, y = 35, label = "carbonate mineral dissolution", angle = 50, colour = "steelblue") + 
  annotate(geom = "text", x = 140, y = 22, label = paste("extra source of Ca+Mg", "from reverse ion exchange", sep = "\n") , angle = 0, colour = "grey50") + 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  scale_fill_manual(name = "Geology",
                    values = c("darkolivegreen3", "darkgreen",
                               "lightskyblue3", "purple4", "firebrick",
                               "gold"),
                    labels = c("Diabase East", "Diabase West",
                               "Turbidite sequence (MCF)", "Complex sedimentary rocks (KG)", "Intrusion (KG)",
                               "Limestones")) +
  labs(x = expression(Ca^'2+'~+~Mg^'2+'~(meq/L)),
       y = expression(HCO[3]^'-'~+~SO[4]^'2-'~(meq/L)),
       title = "(Ca+Mg) vs (HCO3+SO4)") +
  theme_mw() +
  theme(legend.position = "none") +
  annotation_logticks(sides = "bl")

# Ca vs HCO3
# Role of carbonate/silicate minerals weathering
p3 <- ggplot(d_meql %>% filter(sampletype == "groundwater"), aes(x = Ca, y = HCO3)) + 
  geom_point(aes(fill = geology), size = 2, pch = 21, alpha = 0.8) +
  geom_abline(slope = 2, linetype = "dashed", colour = "steelblue") +
  geom_abline(slope = (sw[5])/(sw[1]),linetype = "dashed", colour = "red") +
  annotate(geom = "text", x = 4, y = 25, label = "2:1 line", angle = 55, colour = "steelblue") + 
  annotate(geom = "text", x = 6, y = 3, label = paste("extra source of Ca", "cation exchange", sep = "\n"), angle = 0, colour = "grey50") + 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  scale_fill_manual(name = "Geology",
                    values = c("darkolivegreen3", "darkgreen",
                               "lightskyblue3", "purple4", "firebrick",
                               "gold"),
                    labels = c("Diabase East", "Diabase West",
                               "Turbidite sequence (MCF)", "Complex sedimentary rocks (KG)", "Intrusion (KG)",
                               "Limestones")) +
  labs(x = expression(Ca^'2+'~(meq/L)),
       y = expression(HCO[3]^'-'~(meq/L)),
       title = "Calcite dissolution line") +
  theme_mw() +
  theme(legend.position = "none") +
  annotation_logticks(sides = "bl")

# Ca+Mg vs HCO3
# Role of dolomite dissolution
p4 <- ggplot(d_meql %>% filter(sampletype == "groundwater"), aes(x = Ca+Mg, y = HCO3)) + 
  geom_point(aes(fill = geology), size = 2, pch = 21, alpha = 0.8) +
  geom_abline(slope = 2, linetype = "dashed", colour = "steelblue") +
  geom_abline(slope = (sw[5])/(sw[1]+sw[7]),linetype = "dashed", colour = "red") +
  annotate(geom = "text", x = 4, y = 22, label = "2:1 line", angle = 65, colour = "steelblue") + 
  annotate(geom = "text", x = 150, y = 12, label = paste("extra source of Ca+Mg", "reverse ion exchange", sep = "\n") , angle = 0, colour = "grey50") + 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  scale_fill_manual(name = "Geology",
                    values = c("darkolivegreen3", "darkgreen",
                               "lightskyblue3", "purple4", "firebrick",
                               "gold"),
                    labels = c("Diabase East", "Diabase West",
                               "Turbidite sequence (MCF)", "Complex sedimentary rocks (KG)", "Intrusion (KG)",
                               "Limestones")) +
  labs(x = expression(Ca^'2+'~+~Mg^'2+'~(meg/L)),
       y = expression(HCO[3]^'-'~(meq/L)),
       title = "Dolomite dissolution line") + 
  theme_mw() +
  theme(legend.position = "none") +
  annotation_logticks(sides = "bl")


# Ca vs SO4
# influence of gypsum dissolution
p5 <- ggplot(d_meql %>% filter(sampletype == "groundwater"), aes(x = Ca, y = SO4)) + 
  geom_point(aes(fill = geology), size = 2, pch = 21, alpha = 0.8) +
  geom_abline(linetype = "dashed", colour = "steelblue") +
  geom_abline(slope = (sw[12])/(sw[1]),linetype = "dashed", colour = "red") +
  annotate(geom = "text", x = 2, y = 3, label = "1:1 line", angle = 26, colour = "steelblue") + 
  annotate(geom = "text", x = 2, y = 15, label = "seawater mixing line", angle = 40, colour = "red") + 
  annotate(geom = "text", x = 12, y = 0.5, label = "Extra source of Ca", angle = 0, colour = "grey50") + 
  #annotate(geom = "text", x = 45, y = 50, label = "1:1 line gypsum dissolution", angle = 59, colour = "steelblue") + 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  scale_fill_manual(name = "Geology",
                    values = c("darkolivegreen3", "darkgreen",
                               "lightskyblue3", "purple4", "firebrick",
                               "gold"),
                    labels = c("Diabase East", "Diabase West",
                               "Turbidite sequence (MCF)", "Complex sedimentary rocks (KG)", "Intrusion (KG)",
                               "Limestones")) +
  labs(x = expression(Ca^'2+'~(meq/L)),
       y = expression(SO[4]^'2-'~(meq/L)),
       title = "Gypsum dissolution line") +
  theme_mw() +
  theme(legend.position = "none") +
  annotation_logticks(sides = "bl")

ggplot(d_meql %>% filter(sampletype == "groundwater"), aes(x = (HCO3 / (HCO3 + SO4)), y = (Ca / (Ca + Mg)))) + 
  geom_point(aes(fill = factor(geology)), colour = "black", shape = 21, size = 2, alpha = 0.8) +
  #geom_point(aes(colour = geology)) +
  # add avg seawater Curacao
  geom_point(aes(x = 0.04, y = 0.16), shape = 23, fill = "red", size = 4) +
  geom_abline(linetype = "dashed", colour = "darkgrey", slope = -1, intercept = 1) +
  #geom_abline(slope = (sw[12])/(sw[1]),linetype = "dashed", colour = "red") +
  annotate(geom = "text", x = 0.1, y = 1, label = "Anhydrite / gypsum", colour = "black") + 
  annotate(geom = "text", x = 0.95, y = 1, label = "Calcite", colour = "black") + 
  annotate(geom = "text", x = 0.95, y = 0, label = "Dolomite", colour = "black") + 
  scale_fill_manual(name = "Geology",
                      values = c("darkolivegreen3", "darkgreen",
                                 "lightskyblue3", "purple4", "firebrick",
                                 "gold"),
                      labels = c("Diabase East", "Diabase West",
                                 "Turbidite sequence (MCF)", "Complex sedimentary rocks (KG)", "Intrusion (KG)",
                                 "Limestones")) +
  coord_cartesian(xlim = c(0,1),
                  ylim = c(0,1)) + 
  labs(x = expression(HCO[3]~'/'~(HCO[3] + SO[4])),
       y = expression(Ca~'/'~(Ca + Mg)),
       title = "Carbonate minerals") +
  theme_mw() +
  theme(legend.position = "bottom")
ggsave(paste0(output, "Output/Figures/Hydrochemistry/Biplots/Carbonate_minerals_groundwater.png"),
       width = 6.6, height = 6.6)

# for legend
grobs <- ggplotGrob(p1)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

# Add plots together using cowplot
cowplot::plot_grid(p1, p2, p3, p4, p5, ncol = 2)
# Create grid
# ggpubr::ggarrange(p1, p2, p3, p4, p5, # list of plots
#                   labels = "AUTO", # labels
#                   common.legend = T, # COMMON LEGEND
#                   legend = "bottom", # legend position
#                   align = "hv", # Align them both, horizontal and vertical
#                   ncol = 2)  # number of rows
ggpubr::ggarrange(p1, p2, p3, p4, p5,
                  labels = "AUTO",
                  common.legend = T,
                  legend = "bottom",
                  ncol = 2, 
                  nrow = 3)
ggsave(paste0(output, "Output/Figures/Hydrochemistry/Biplots/Biplots_minerals.png"),
       width = 8, height = 8)


##########################################################################
# Ionic ratios in groundwater
##########################################################################

## wide format with ratios
SO4sea <- data %>% filter(sampletype == "seawater", parameter == "SO4") %>% pull(value) %>% mean()
Clsea <- data %>% filter(sampletype == "seawater", parameter == "Cl") %>% pull(value) %>% mean()

# # add metadata
# d_meql <- d_meql %>%
#   left_join(., d_meta %>% dplyr::select(samplecode, `Well.depth.below.surface.(m)`, 
#                                  `Groundwater.level.below.surface.(m)`,
#                                  sample_depth,
#                                  geology, geology_abr, landuse_zonal_map, dist.coast, 
#                                  geozone, geoz_pop, geoz_dens, geoz_house, geoz_avg_house, 
#                                  neigh, neigh_dens, elevation)) %>%
#   #mutate(geology = ifelse(geology == "Knip Group - intrusive", "Knip Group", geology)) %>%
#   mutate(geology = ifelse(geology == "Other" & sampletype == "groundwater", "Knip Group - intrusive", geology))

d_wide <- data %>%
  filter(year > 2020) %>%
  # remove isotopes until accurate measurements are included
  filter(method != "IA") %>%
  mutate(parameter = paste(parameter, units)) %>%
  # add metadata
  left_join(., metadata %>% dplyr::select(samplecode, geology)) %>% 
  mutate(geology = ifelse(geology == "Other" & sampletype == "groundwater", "Knip Group - intrusive", geology)) %>%
  dplyr::select(putcode, samplecode, year, parameter, value, watercode, sampletype, subtype, geology) %>%
  distinct() %>%
  # put in wide format
  pivot_wider(names_from = parameter,
              values_from = value) %>%
  # calculate ratios
  mutate(`B/Cl` = (`B ug/l`/1000) / `Cl mg/l`,
         `Cl/B` = `Cl mg/l` / (`B ug/l`/1000),
         `Br/Cl` = (`Br mg/l` / `Cl mg/l`),
         `Ca/Cl` = `Ca mg/l` / `Cl mg/l`,
         `SO4/Cl` = `SO4 mg/l` / `Cl mg/l`,
         `SO4 cons/prod` = `SO4 mg/l` - (`Cl mg/l` * SO4sea/Clsea)) # SO4 consumption/production normalized to SO4:Cl of seawater

# data %>%
#   dplyr::group_by(putcode, samplecode, watercode, sampletype, subtype, parameter) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)

data %>%
  filter(year > 2021) %>%
  dplyr::select(samplecode, sampletype, parameter, value) %>%
  filter(parameter %in% c("B", "Cl", "EC")) %>%
  #filter(sampletype != "groundwater") %>%
  pivot_wider(names_from = parameter,
              values_from = value)

d %>%
  dplyr::select(samplecode, sampletype, subtype, `EC uS/cm`, `B ug/l`, `Cl mg/l`, `B/Cl`) %>%
  #filter(sampletype != "groundwater") %>%
  view()

summ <- d_wide %>% 
  group_by(sampletype) %>%
  summarise(n = n(),
            avg_B = mean(`B ug/l`, na.rm = T),
            med_B = median(`B ug/l`, na.rm = T),
            avg_Cl = mean(`Cl mg/l`, na.rm = T),
            med_Cl = median(`Cl mg/l`, na.rm = T),
            `avg_B/Cl` = mean(`B/Cl`, na.rm = T),
            `med_B/Cl` = median(`B/Cl`, na.rm = T),
            `avg_Cl/B` = mean(`Cl/B`, na.rm = T),
            `med_Cl/B` = median(`Cl/B`, na.rm = T))

# B/Cl ratio
p1 <- ggplot(data = d_wide, aes(x = `B/Cl`, y = fct_rev(sampletype))) +
  geom_boxplot(fill = "lightgrey") +
  labs(y = "") +
  theme_mw() 

p2 <- ggplot(data = d_wide, aes(x = `B/Cl`, y = subtype)) +
  geom_boxplot(fill = "lightgrey") +
  labs(y = "") +
  theme_mw() 

m <- ((summ[6,8]-summ[1,8])/(summ[6,6]-summ[1,6]))
b <- summ[6,8] - summ[6,6] * sl

p3 <- ggplot(data = d_wide %>% filter(!sampletype %in% c("rainwater", "seawater")), 
             aes(x = `Cl mg/l`, y = `B/Cl`)) +
  geom_point(aes(color = sampletype), alpha = 0.7) +
  geom_smooth(formula = y ~ x) +
  geom_abline(intercept = b$`med_B/Cl`, slope = m$`med_B/Cl`, linetype = "dashed") +
  coord_cartesian(xlim = c(0, 1000)) +
  theme_mw() +
  theme(legend.position = "bottom")
  #theme(legend.position = c(0.9, 0.65))

cowplot::plot_grid(p1, p2, p3, ncol = 1, labels = "AUTO")
ggsave(paste0(output, "Output/Figures/Hydrochemistry/Biplots/B-Cl_sampletypes.png"),
       width = 8, height = 8)

# Ca/Cl 
p1 <- ggplot(data = d, aes(x = `Ca/Cl`, y = fct_rev(sampletype))) +
  geom_boxplot(fill = "lightgrey") +
  labs(y = "") +
  theme_bw() 

p3 <- ggplot(data = d, 
             aes(x = `Cl mg/l`, y = `Ca/Cl`)) +
  geom_point(aes(color = sampletype)) +
  geom_smooth(formula = y ~ x) +
  #geom_abline(intercept = intc$`med_B/Cl`, slope = sl$`med_B/Cl`, linetype = "dashed") +
  coord_cartesian(xlim = c(0, 1000)) +
  theme_bw() 
#theme(legend.position = c(0.9, 0.65))

cowplot::plot_grid(p1, p3, ncol = 1, labels = "AUTO")

##### SO4/Cl  #####
# different sampletypes
p1 <- ggplot(data = d_wide, aes(x = `SO4/Cl`, y = fct_rev(sampletype))) +
  geom_boxplot(fill = "lightgrey") +
  labs(y = "") +
  theme_bw() 

p2 <- ggplot(data = d_wide, 
             aes(x = `Cl mg/l`, y = `SO4/Cl`)) +
  geom_point(aes(color = sampletype)) +
  #geom_smooth(formula = y ~ x) +
  #geom_abline(intercept = intc$`med_B/Cl`, slope = sl$`med_B/Cl`, linetype = "dashed") +
  scale_x_continuous(trans = "log10") +
  #coord_cartesian(xlim = c(0, 1000)) +
  theme_bw() 
#theme(legend.position = c(0.9, 0.65))

cowplot::plot_grid(p1, p2, ncol = 1, labels = "AUTO")

# boxplots groundwater geology
p1 <- ggplot(data = d_wide %>% filter(sampletype == "groundwater", !is.na(geology)), 
             aes(x = `SO4 cons/prod`, y = fct_rev(geology), fill = geology)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(name = "Geology",
                    values = c("darkolivegreen3", "darkgreen",
                               "lightskyblue3", "purple4", "firebrick",
                               "gold"),
                    labels = c("Diabase East", "Diabase West",
                               "Turbidite sequence (MCF)", "Complex sedimentary rocks (KG)", "Intrusion (KG)",
                               "Limestones")) +
  labs(y = "") +
  theme_bw() +
  theme(legend.position = "none")
        #axis.text.y=element_blank()) 

p1 <- ggplot(data = d_wide %>% filter(sampletype == "groundwater", !is.na(geology)), 
             aes(y = `SO4 cons/prod`, x = geology, fill = geology)) +
  geom_boxplot(alpha = 0.8) +
  coord_cartesian(ylim = c(-250, 750)) +
  scale_fill_manual(name = "Geology",
                    values = c("darkolivegreen3", "darkgreen",
                               "lightskyblue3", "purple4", "firebrick",
                               "gold"),
                    labels = c("Diabase East", "Diabase West",
                               "Turbidite sequence (MCF)", "Complex sedimentary rocks (KG)", "Intrusion (KG)",
                               "Limestones")) +
  labs(x = "",
       y = "SO4 consumed(-) / produced(+) mg/L") +
  theme_mw() +
  theme(legend.position = "none",
        axis.text.x = element_blank()) 

p2 <- ggplot(data = d_wide %>% filter(sampletype == "groundwater", !is.na(geology)), 
             aes(x = `Cl mg/l`, y = `SO4 cons/prod`, fill = geology)) +
  geom_point(colour = "black",
             pch = 21, 
             size = 2,
             alpha = 0.8) +
  #geom_abline(intercept = intc$`med_B/Cl`, slope = sl$`med_B/Cl`, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous("Cl mg/L", trans = "log10") +
  coord_cartesian(xlim = c(25, 5000),
                  ylim = c(-250, 500)) +
  scale_fill_manual(name = "Geology",
                    values = c("darkolivegreen3", "darkgreen",
                               "lightskyblue3", "purple4", "firebrick",
                               "gold"),
                    labels = c("Diabase East", "Diabase West",
                               "Turbidite sequence (MCF)", "Complex sedimentary rocks (KG)", "Intrusion (KG)",
                               "Limestones")) +
  theme_mw() +
  theme(legend.position = "bottom",
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        axis.title.y = element_blank()
        ) 

plt <- ggpubr::ggarrange(p1, p2, labels = "AUTO", legend = "bottom", common.legend = T, align = "v")
# ggpubr::annotate_figure(plt, top = ggpubr::text_grob("Groundwater sulphate consumption/production",
#                         face = "bold", size = 14))
# ggsave(paste0(output, "Output/Figures/Hydrochemistry/Biplots/SO4/SO4prod-consump_geol_combined.png"),
#        width = 6.6, height = 4.1)
ggpubr::annotate_figure(plt, top = ggpubr::text_grob("Groundwater sulphate consumption/production inzoom",
                                                     face = "bold", size = 14))

ggsave(paste0(output, "Output/Figures/Hydrochemistry/Biplots/SO4/SO4prod-consump_geol_combined_inzoom.png"),
       width = 6.6, height = 4.1)

ggplot(data = d_wide %>% filter(sampletype == "groundwater", year > 2000, !is.na(geology)), 
       aes(x = `Cl mg/l`, y = `SO4 cons/prod`, fill = geology)) +
  geom_point(colour = "black",
             pch = 21, 
             size = 2,
             alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(trans = "log10") +
  scale_fill_manual(name = "Geology",
                      values = c("darkolivegreen3", "darkgreen",
                                 "lightskyblue3", "purple4", "firebrick",
                                 "gold"),
                      labels = c("Diabase East", "Diabase West",
                                 "Turbidite sequence (MCF)", "Complex sedimentary rocks (KG)", "Intrusion (KG)",
                                 "Limestones")) +
  labs(y = "SO4 consumed(-)/produced(+) mg/L",
       title = "Groundwater sulphate consumption/production") +
  theme_mw() +
  # theme(#panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(), 
  #   legend.position = "none") +
  theme(legend.position = "bottom") +
  annotation_logticks(sides = "b")
ggsave(paste0(output, "Output/Figures/Hydrochemistry/Biplots/SO4/SO4prod-consump.png"),
       width = 6.6, height = 6.6)

## + inzoom within -500 and 500 mg/L SO4
ggplot(data = d_wide %>% filter(sampletype == "groundwater", year > 2000, 
                                !is.na(geology), abs(`SO4 cons/prod`) < 500), 
       aes(x = `Cl mg/l`, y = `SO4 cons/prod`, fill = geology)) +
  geom_point(colour = "black",
             pch = 21, 
             size = 2,
             alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(trans = "log10") +
  scale_fill_manual(name = "Geology",
                    values = c("darkolivegreen3", "darkgreen",
                               "lightskyblue3", "purple4", "firebrick",
                               "gold"),
                    labels = c("Diabase East", "Diabase West",
                               "Turbidite sequence (MCF)", "Complex sedimentary rocks (KG)", "Intrusion (KG)",
                               "Limestones")) +
  labs(y = "SO4 consumed(-)/produced(+) mg/L",
       title = "Groundwater sulphate consumption/production") +
  theme_mw() +
  # theme(#panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(), 
  #   legend.position = "none") +
  theme(legend.position = "bottom") +
  annotation_logticks(sides = "b")
ggsave(paste0(output, "Output/Figures/Hydrochemistry/Biplots/SO4/SO4prod-consump_inzoom.png"),
       width = 6.6, height = 6.6)

# SO4 with O2
ggplot(data = d_wide %>% filter(sampletype == "groundwater", year > 2000, !is.na(`DO mg/l`)), 
       aes(x = `Cl mg/l`, y = `SO4 cons/prod`, 
           fill = cut(`DO mg/l`, breaks = c(0, 1, 2, 5, 9)))) +
  geom_point(colour = "black",
             pch = 21,
             size = 2,
             alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(trans = "log10") +
  scale_fill_manual(name = "DO [mg/l]",
                    labels = c("0 - 1",
                               "1 - 2",
                               "2 - 5",
                               "5 - 9"),
                    values = c("red",
                               "orange",
                               "darkolivegreen3",
                               "cornflowerblue")) +
  labs(y = "SO4 consumed(-)/produced(+) mg/L",
       title = "Groundwater sulphate consumption/production") +
  theme_mw() +
  # theme(#panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(), 
  #   legend.position = "none") +
  theme(legend.position = c(0.9, 0.8)) +
  annotation_logticks(sides = "b")

ggplot(data = d_wide %>% filter(sampletype == "groundwater", year > 2000, !is.na(`DO mg/l`)), 
       aes(x = `Cl mg/l`, y = `SO4 cons/prod`, 
           fill = `DO mg/l`)) +
  geom_point(colour = "black",
             pch = 21,
             size = 2,
             alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(trans = "log10") +
  scale_fill_viridis_c() +
  labs(y = "SO4 consumed(-)/produced(+) mg/L",
       title = "Groundwater sulphate consumption/production") +
  theme_mw() +
  # theme(#panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(), 
  #   legend.position = "none") +
  theme(legend.position = c(0.9, 0.8)) +
  annotation_logticks(sides = "b")

ggsave(paste0(output, "Output/Figures/Hydrochemistry/Biplots/SO4/SO4prod-consump_with_DO_viridis.png"),
       width = 6.6, height = 6.6)

# SO4 with NO3
ggplot(data = d_wide %>% filter(sampletype == "groundwater", year > 2000, !is.na(`NO3 mg/l`), 
                                geology %in% c("Curacao Lava Formation East", "Curacao Lava Formation West")), 
       aes(x = `Cl mg/l`, y = `SO4 cons/prod`, 
           fill = cut(`NO3 mg/l`, breaks = c(0, 5, 10, 25, 50, 100, 1000)))) +
  geom_point(colour = "black",
             pch = 21,
             size = 2,
             alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(trans = "log10") +
  scale_fill_manual(name = "NO3 [mg/l]",
                    labels = c("0 - 5",
                               "5 - 10",
                               "10 - 25",
                               "25 - 50",
                               "50 - 100",
                               ">100"),
                    values = c("cornflowerblue",
                               "lightgreen",
                               "darkolivegreen4",
                               "yellow",
                               "orange",
                               "red")) +
  labs(y = "SO4 consumed(-)/produced(+) mg/L",
       title = "Groundwater sulphate consumption/production") +
  theme_mw() +
  # theme(#panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(), 
  #   legend.position = "none") +
  theme(legend.position = c(0.9, 0.8)) +
  annotation_logticks(sides = "b")
ggsave(paste0(output, "Output/Figures/Hydrochemistry/Biplots/SO4/SO4prod-consump_with_NO3.png"),
       width = 6.6, height = 6.6)

ggplot(data = d_wide %>% filter(sampletype == "groundwater", year > 2000, !is.na(`NO3 mg/l`), 
                                geology %in% c("Curacao Lava Formation East", "Curacao Lava Formation West")), 
       aes(x = `NO3 mg/l`, y = `SO4 cons/prod`, 
           fill = geology)) +
  geom_point(colour = "black",
             pch = 21, 
             size = 2,
             alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = lm)+
  #scale_x_continuous(trans = "log10") +
  scale_colour_manual(name = "Geology",
                      values = c("darkolivegreen3", "darkgreen",
                                 "lightskyblue3", "purple4", "firebrick",
                                 "gold"),
                      labels = c("Diabase East", "Diabase West",
                                 "Turbidite sequence (MCF)", "Complex sedimentary rocks (KG)", "Intrusion (KG)",
                                 "Limestones")) +
  labs(y = "SO4 consumed(-)/produced(+) mg/L",
       title = "Groundwater sulphate consumption/production") +
  theme_mw() +
  # theme(#panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(), 
  #   legend.position = "none") +
  theme(legend.position = c(0.9, 0.8)) 
  #annotation_logticks(sides = "b")
# ggsave(paste0(output, "Output/Figures/Hydrochemistry/Biplots/SO4/SO4prod-consump_with_NO3.png"),
#        width = 6.6, height = 6.6)

## with DOC? 
ggplot(data = d_wide %>% filter(sampletype == "groundwater", year > 2000, !is.na(`DOC mg/l`)), 
       aes(x = `Cl mg/l`, y = `SO4 cons/prod`, 
           fill = cut(`DOC mg/l`, breaks = c(0, 2.5, 5, 10, 20, 40)))) +
  geom_point(colour = "black",
             pch = 21,
             size = 2,
             alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(trans = "log10") +
  scale_fill_manual(name = "DOC [mg/l]",
                    labels = c("0 - 2.5",
                               "2.5 - 5",
                               "5 - 10",
                               "10 - 20",
                               "20 - 40"),
                    values = c("cornflowerblue",
                               "darkolivegreen4",
                               "yellow",
                               "orange",
                               "red")) +
  labs(y = "SO4 consumed(-)/produced(+) mg/L",
       title = "Groundwater sulphate consumption/production") +
  theme_mw() +
  # theme(#panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(), 
  #   legend.position = "none") +
  theme(legend.position = c(0.9, 0.8)) +
  annotation_logticks(sides = "b")
ggsave(paste0(output, "Output/Figures/Hydrochemistry/Biplots/SO4/SO4prod-consump_with_DOC.png"),
       width = 6.6, height = 6.6)


##  extremes >500 mg/L SO4
ggplot(data = d_wide %>% filter(sampletype == "groundwater", year > 2000, 
                                !is.na(geology), abs(`SO4 cons/prod`) > 500), 
       aes(x = `Cl mg/l`, y = `SO4 cons/prod`)) +
  geom_point(aes(color = geology), alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(trans = "log10") +
  scale_colour_manual(name = "Geology",
                      values = c("darkolivegreen3", "darkgreen",
                                 "lightskyblue3", "purple4", "firebrick",
                                 "gold"),
                      labels = c("Diabase East", "Diabase West",
                                 "Turbidite sequence (MCF)", "Complex sedimentary rocks (KG)", "Intrusion (KG)",
                                 "Limestones")) +
  labs(y = "SO4 consumed(-)/produced(+) mg/L",
       title = "Groundwater sulphate consumption/production") +
  theme_mw() +
  # theme(#panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(), 
  #   legend.position = "none") +
  theme(legend.position = "bottom") +
  annotation_logticks(sides = "b")
ggsave(paste0(output, "Output/Figures/Hydrochemistry/Biplots/SO4prod-consump_extremes.png"),
       width = 7, height = 7)


ggplot(data = d_wide %>% filter(sampletype == "groundwater", year > 2000),
       aes(x = `Cl mg/l`, y = `SO4 cons/prod`)) +
  geom_boxplot(aes(fill = geology))

ggplot(data = d_wide %>% filter(sampletype == "groundwater", year > 2000),
       aes(x = `SO4 cons/prod`, y = fct_rev(geology))) +
  geom_boxplot(aes(fill = geology))

ggplot(data = d_wide %>% filter(sampletype == "groundwater", year > 2000, geology %in% c("Curacao Lava Formation East", "Curacao Lava Formation West")),
       aes(x = fct_rev(geology), y = `SO4 cons/prod`)) +
  geom_boxplot(aes(fill = geology)) +
  labs(x = "") +
  theme_mw()

d_wide %>% filter(year > 2000) %>% dplyr::select(samplecode, year, geology, `SO4 mg/l`, `Cl mg/l`, `SO4 cons/prod`) %>% view()









