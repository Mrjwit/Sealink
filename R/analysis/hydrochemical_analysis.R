#
# Input: all cleaned datafiles
#         
# Output: Hydrochemical analysis
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 03-04-2022
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, ggmap, 
               sf, leaflet, data.table, cowplot, data.table,
               scales, corrplot, Hmisc, ggpubr,
               ggbiplot)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/" 

# load cleaned data
data <- openxlsx::read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx"))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# Data analysis
###############################################################################

#### Piper diagram ####

# Convert concentrations to meq/l and place in wide format
an <- c("Cl", "HCO3", "NO3", "PO4", "SO4")
cat <- c("Na", "Ca", "Mg", "Fe", "K", "NH4")

d <- data %>%
  filter(year == 2021, 
         parameter %in% c(an, cat)) %>%
  filter(!str_detect(method, "PHOS"),
         units != "mg N/L") %>%                    
  # change mg/l to meq/l for anions
  mutate(meql = case_when(
    parameter == "Cl" ~ value / 35.453,
    parameter == "HCO3" ~ value / 61.0168,
    parameter == "NO3" & limit_symbol != "<" ~ value / 62.0049,
    #parameter == "NO2" ~ value / 46.0055,
    parameter == "PO4" ~ value / 94.9714 * 3,
    parameter == "SO4" ~ value / 96.06 * 2,
    #parameter == "Br" ~ value / 79.904,
    #parameter == "F" ~ value / 18.998403,
    #parameter == "EC_uS" ~ value,
    TRUE ~ NA_real_ )) %>%
  mutate(meql = case_when(
    parameter == "Na" ~ value / 22.989769,
    parameter == "Ca" ~ value / 40.078 * 2,
    parameter == "Mg" ~ value / 24.305 * 2,
    parameter == "Fe" ~ value / 55.845 * 2,
    #parameter == "Mn" ~ value / 54.938044 * 2 / 1000,
    parameter == "K" ~ value / 39.0983,
    parameter == "NH4" & limit_symbol != "<" ~ value / 18.04,
    TRUE ~ meql )) %>%
  select(samplecode, parameter, meql, sampletype) %>%
  pivot_wider(names_from = parameter,
              values_from = meql) %>%
  filter(!is.na(Cl))

# functions for piper diagram https://rstudio-pubs-static.s3.amazonaws.com/542159_ae160ba405044883a58ba3a53e4f7e6d.html
# takes as input a dataframe with cation and anion concentrations in meql and converts them to percentages
toPercent <- function (d) {
  totalCations <- d$Ca + d$Mg + d$Na + d$K
  d$Ca <- 100 * (d$Ca/totalCations)
  d$Mg <- 100 * (d$Mg/totalCations)
  d$Na <- 100 * (d$Na/totalCations)
  d$K <- 100 * (d$K/totalCations)
  totalAnions <- d$Cl + d$SO4 + d$HCO3
  d$Cl <- 100 * (d$Cl/totalAnions)
  d$SO4 <- 100 * (d$SO4/totalAnions)
  #d$CO3 <- 100 * (d$CO3/totalAnions)
  d$HCO3 <- 100 * (d$HCO3/totalAnions)
  return(d)
}

dat <- toPercent(d)

# takes percentage data and converts them to xy coordinates for plotting
transform_piper_data <- function(Mg, Ca, Cl, SO4, name=samplecode, watertype=watertype){
  if(is.null(name)){
    name = rep(1:length(Mg),3)
  } else {
    name = rep(name,3)
  }
  y1 <- Mg * 0.86603
  x1 <- 100*(1-(Ca/100) - (Mg/200))
  y2 <- SO4 * 0.86603
  x2 <-120+(100*Cl/100 + 0.5 * 100*SO4/100)
  new_point <- function(x1, x2, y1, y2, grad=1.73206){
    b1 <- y1-(grad*x1)
    b2 <- y2-(-grad*x2)
    M <- matrix(c(grad, -grad, -1,-1), ncol=2)
    intercepts <- as.matrix(c(b1,b2))
    t_mat <- -solve(M) %*% intercepts
    data.frame(x=t_mat[1,1], y=t_mat[2,1])
  }
  np_list <- lapply(1:length(x1), function(i) new_point(x1[i], x2[i], y1[i], y2[i]))
  npoints <- do.call("rbind",np_list)
  data.frame(observation=name,x=c(x1, x2, npoints$x), y=c(y=y1, y2, npoints$y), watertype = watertype)
}

piper.data <- transform_piper_data(dat$Mg, dat$Ca, dat$Cl, dat$SO4, dat$samplecode, dat$watertype)
piper.data <- piper.data %>%
  mutate(date = 2021)

ggplot_piper <- function(piper.data,output = c("ggplot","plotly")) {
  grid1p1 <<- data.frame(x1 = c(20,40,60,80), x2= c(10,20,30,40),y1 = c(0,0,0,0), y2 = c(17.3206,34.6412,51.9618, 69.2824))
  grid1p2 <<- data.frame(x1 = c(20,40,60,80), x2= c(60,70,80,90),y1 = c(0,0,0,0), y2 = c(69.2824, 51.9618,34.6412,17.3206))
  grid1p3 <<- data.frame(x1 = c(10,20,30,40), x2= c(90,80,70,60),y1 = c(17.3206,34.6412,51.9618, 69.2824), y2 = c(17.3206,34.6412,51.9618, 69.2824))
  grid2p1 <<- grid1p1
  grid2p1$x1 <- grid2p1$x1+120
  grid2p1$x2 <- grid2p1$x2+120
  grid2p2 <<- grid1p2
  grid2p2$x1 <- grid2p2$x1+120
  grid2p2$x2 <- grid2p2$x2+120
  grid2p3 <<- grid1p3
  grid2p3$x1 <- grid2p3$x1+120
  grid2p3$x2 <- grid2p3$x2+120
  grid3p1 <<- data.frame(x1=c(100,90, 80, 70),y1=c(34.6412, 51.9618, 69.2824, 86.603), x2=c(150, 140, 130, 120), y2=c(121.2442,138.5648,155.8854,173.2060))
  grid3p2 <<- data.frame(x1=c(70, 80, 90, 100),y1=c(121.2442,138.5648,155.8854,173.2060), x2=c(120, 130, 140, 150), y2=c(34.6412, 51.9618, 69.2824, 86.603))
  grid3p3 <<- data.frame(x1=c(85, 85, 85, 85),y1=c(60.6221, 147.2251, 147.2251, 60.6221), x2=c(135,135, 135, 135), y2=c(147.2251, 60.6221, 147.2251, 60.6221))

  p <- ggplot2::ggplot() +
    ## left hand ternary plot
    ggplot2::geom_segment(ggplot2::aes(x=0,y=0, xend=100, yend=0), size = 0.75) +
    ggplot2::geom_segment(ggplot2::aes(x=0,y=0, xend=50, yend=86.603), size = 0.75) +
    ggplot2::geom_segment(ggplot2::aes(x=50,y=86.603, xend=100, yend=0), size = 0.75) +
    ## right hand ternary plot
    ggplot2::geom_segment(ggplot2::aes(x=120,y=0, xend=220, yend=0), size = 0.75) +
    ggplot2::geom_segment(ggplot2::aes(x=120,y=0, xend=170, yend=86.603), size = 0.75) +
    ggplot2::geom_segment(ggplot2::aes(x=170,y=86.603, xend=220, yend=0), size = 0.75) +
    ## Upper diamond
    ggplot2::geom_segment(ggplot2::aes(x=110,y=190.5266, xend=60, yend=103.9236), size = 0.75) +
    ggplot2::geom_segment(ggplot2::aes(x=110,y=190.5266, xend=160, yend=103.9236), size = 0.75) +
    ggplot2::geom_segment(ggplot2::aes(x=110,y=17.3206, xend=160, yend=103.9236), size = 0.75) +
    ggplot2::geom_segment(ggplot2::aes(x=110,y=17.3206, xend=60, yend=103.9236), size = 0.75) +
    ## Add grid lines to the plots
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p3, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p3, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p1, linetype = "dashed", size = 0.25, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p2, linetype = "dashed", size = 0.25, colour = "grey50") +
    # Add bold grid lines for water types
    ggplot2::geom_segment(ggplot2::aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p3, size = 0.25, colour = "grey20") +

    ### Labels and grid values
    ggplot2::geom_text(ggplot2::aes(c(20,40,60,80),c(-5,-5,-5,-5), label=c(80, 60, 40, 20)), size=3) +
    ggplot2::geom_text(ggplot2::aes(c(35,25,15,5),grid1p2$y2, label=c(80, 60, 40, 20)), size=3) +
    ggplot2::coord_equal(ratio=1) +  
    ggplot2::geom_text(ggplot2::aes(c(215,205,195,185),grid2p3$y2, label=c(20, 40, 60, 80)), size=3) +
    ggplot2::geom_text(ggplot2::aes(c(140,160,180,200),c(-5,-5,-5,-5), label=c(20, 40, 60, 80)), size=3) +
    ggplot2::geom_text(ggplot2::aes(grid3p1$x1-5,grid3p1$y1, label=c(80, 60, 40, 20)), size=3) +
    ggplot2::geom_text(ggplot2::aes(grid3p1$x2+5,grid3p1$y2, label=c(20, 40, 60, 80)), size=3) +
    ggplot2::geom_text(ggplot2::aes(grid3p2$x1-5,grid3p2$y1, label=c(20, 40, 60, 80)), size=3) +
    ggplot2::geom_text(ggplot2::aes(grid3p2$x2+5,grid3p2$y2, label=c(80, 60, 40, 20)), size=3) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(),
                   legend.title = element_blank()) +
    ggplot2::geom_point(data=piper.data, 
                        ggplot2::aes(x,y, colour=factor(watertype),
                                     shape = watertype, 
                                     text = paste(observation,
                                                  '</br></br>Date: ',
                                                  date))) + 
    # Adjust manual scale so the shape and colour are together in legend
    # however, it is now no longer flexible and only works for 6 classes
    scale_colour_manual(name = "water type",
                        values = c("#F8766D", "#00BFC4", "#00BA38",
                                   "#B79F00", "#619CFF", "#C77CFF")) +
    scale_shape_manual(name = "water type",
                       values = c(19, 17, 15, 3, 7, 8)) +
    ggplot2::annotate(
      geom = "text",
      label = c("1", "2", "3", "4", "5", "5"),
      x = c(70, 110, 150, 110, 110, 105),
      y = c(103.92, 175, 103.92, 32, 75, 138.5648)
    ) +
    ggplot2::geom_text(ggplot2::aes(
      #geom = "text",
      x = 20,
      y = c(150, 140, 130, 120, 110),
      #label = "Alkalinity~as~HCO[3]^'-'"
      #label = "1 Ca-HCO[3]\n 2 Ca-Cl\n 3 Na-Cl\n 4 Na-HCO[3]\n 5 Mixed type"
      label = c("1~Ca-`HCO`[3]", "2~Ca-`Cl`", "3~Na-`Cl`", "4~Na-`HCO`[3]", "5~Mixed~type")
    ), parse=TRUE)
  
  
  if (output == "ggplot"){
    p <- p + 
      ggplot2::geom_text(ggplot2::aes(17,50, label="Mg^'2+'"), angle=60, size=4, parse=TRUE) +  
      ggplot2::geom_text(ggplot2::aes(77.5,50, label="Na^'+'~+~K^'+'"), angle=-60, size=4,parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(50,-10, label="Ca^'2+'"), size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(170,-10, label="Cl^'-'"), size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(205,50, label="SO[4]^'-'"), angle=-60, size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(142,50, label="Alkalinity~as~HCO[3]^'-'"), angle=60, size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(72.5,150, label="SO[4]^'-'~+~Cl^'-'"), angle=60, size=4, parse=TRUE) +
      ggplot2::geom_text(ggplot2::aes(147.5,150, label="Ca^'2+'~+~Mg^'2+'"), angle=-60, size=4, parse=TRUE)
  }
  
  if (output == "plotly"){
    #this fixes an issue that plotly can't render geom_text() with the  angle option set properly
    p <- plotly::ggplotly(p,
                          tooltip = c("text")
    )
    p <- p  %>% plotly::layout(
      annotations=list(text=c("Mg<sup>2+</sup>",
                              "Na<sup>+</sup> + K<sup>+</sup>",
                              "Ca<sup>2+</sup>",
                              "Cl<sup>-</sup>",
                              "SO<sub>4</sub><sup>-</sup>",
                              "Alkalinity as HCO<sub>3</sub><sup>-</sup>",
                              "SO<sub>4</sub><sup>-2</sup> + Cl<sup>-</sup>",
                              "Ca<sup>2+</sup> + Mg<sup>2+</sup>"),
                       x = c(17,77.5,50,170,205,142.5,72.5,147.5),
                       y = c(50,50,-10,-10,50,50,150,150),
                       textangle = c(-60,60,0,0,60,-60,-60,60),
                       "showarrow"=F, font=list(size = 12, color = "black")
      ))
    
  }
  
  return(p)
}

# Make piper diagram
ggplot_piper(piper.data, output = "ggplot")

# different versions with geology/EC/etc?

#### Stiff diagram ####
an <- c("Cl", "HCO3", "NO3", "PO4", "SO4")
cat <- c("Na", "Ca", "Mg", "Fe", "K", "NH4")

d <- data %>%
  filter(year == 2021, 
         parameter %in% c("Cl", "SO4", "HCO3", 
                          "Na", "Ca", "K", "Mg")) %>%
  filter(!str_detect(method, "PHOS"),
         units != "mg N/L") %>%                    
  # change mg/l to meq/l for anions
  mutate(meql = case_when(
    parameter == "Cl" ~ value / 35.453,
    parameter == "HCO3" ~ value / 61.0168,
    #parameter == "NO3" & limit_symbol != "<" ~ value / 62.0049,
    #parameter == "NO2" ~ value / 46.0055,
    #parameter == "PO4" ~ value / 94.9714 * 3,
    parameter == "SO4" ~ value / 96.06 * 2,
    #parameter == "Br" ~ value / 79.904,
    #parameter == "F" ~ value / 18.998403,
    #parameter == "EC_uS" ~ value,
    TRUE ~ NA_real_ )) %>%
  mutate(meql = case_when(
    parameter == "Na" ~ value / 22.989769,
    parameter == "Ca" ~ value / 40.078 * 2,
    parameter == "Mg" ~ value / 24.305 * 2,
    #parameter == "Fe" ~ value / 55.845 * 2,
    #parameter == "Mn" ~ value / 54.938044 * 2 / 1000,
    parameter == "K" ~ value / 39.0983,
    #parameter == "NH4" & limit_symbol != "<" ~ value / 18.04,
    TRUE ~ meql )) %>%
  select(samplecode, parameter, meql, sampletype) %>%
  pivot_wider(names_from = parameter,
              values_from = meql) %>%
  filter(!is.na(Cl))
d <- toPercent(d)

# naje stiff diagrams
with(d, stiffPlot)



#### Watertypes ####
# convert concentrations to meq/l
# Convert concentrations to meq/l and place in wide format
an <- c("Cl", "HCO3", "NO3", "PO4", "SO4")
cat <- c("Na", "Ca", "Mg", "Fe", "K", "NH4")

d <- data %>%
  filter(year == 2021, 
         parameter %in% c("Cl", "SO4", "HCO3", 
                          "Na", "Ca", "K", "Mg")) %>%
  filter(!str_detect(method, "PHOS"),
         units != "mg N/L") %>%                    
  # change mg/l to meq/l for anions
  mutate(meql = case_when(
    parameter == "Cl" ~ value / 35.453,
    parameter == "HCO3" ~ value / 61.0168,
    #parameter == "NO3" & limit_symbol != "<" ~ value / 62.0049,
    #parameter == "NO2" ~ value / 46.0055,
    #parameter == "PO4" ~ value / 94.9714 * 3,
    parameter == "SO4" ~ value / 96.06 * 2,
    #parameter == "Br" ~ value / 79.904,
    #parameter == "F" ~ value / 18.998403,
    #parameter == "EC_uS" ~ value,
    TRUE ~ NA_real_ )) %>%
  mutate(meql = case_when(
    parameter == "Na" ~ value / 22.989769,
    parameter == "Ca" ~ value / 40.078 * 2,
    parameter == "Mg" ~ value / 24.305 * 2,
    #parameter == "Fe" ~ value / 55.845 * 2,
    #parameter == "Mn" ~ value / 54.938044 * 2 / 1000,
    parameter == "K" ~ value / 39.0983,
    #parameter == "NH4" & limit_symbol != "<" ~ value / 18.04,
    TRUE ~ meql )) %>%
  select(samplecode, parameter, meql, sampletype) %>%
  pivot_wider(names_from = parameter,
              values_from = meql) %>%
  filter(!is.na(Cl)) %>%
  select(samplecode, sampletype, Cl, SO4, HCO3, Na, K, Ca, Mg)
d <- toPercent(d)

d <- d %>%
  mutate(watertype = case_when(
    Ca + Mg > 50 & HCO3 > 50 ~ "Ca-HCO3",   # carbonate dissolution
    Ca + Mg > 50 & Cl + SO4 > 50 ~ "Ca-Cl",       # salinizing groundwater with ion exchange of Na for Ca
    Na + K > 50 & Cl + SO4 > 50 ~ "Na-Cl",        # seawater origin
    Na + K > 50 & HCO3 > 50 ~ "Na-HCO3",    # freshening groundwater with ion exchange of Ca for Na
  TRUE ~ "Mixed type" ))                    # mixing water

  
  
d <- d %>%
  rowwise() %>%
  mutate(an = names(.[3:5])[which.max(c_across(cols = Cl:HCO3))],
         cat = names(.[6:9])[which.max(c_across(cols = Na:Mg))]) %>%
  mutate(type = paste(cat, an, sep = "-"))

d %>% group_by(watertype) %>%
  summarise(n()) %>%
  view()


#### Correlation matrix ####
## metals ##

# all samples
d <- data %>%
  filter(year == 2021,
         !is.na(value),
         method == "ICP-MS") %>%
  # remove Ag and Cd since all values are <dl
  filter(!parameter %in% c("Ag", "Cd")) %>%
  select(samplecode, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value)

corr <- cor(d[,-1], use = "complete.obs", method = "pearson")
# data are not normally distributed, so use either Kendall or Spearman!
# test for normal distribution
# 1. use Shaprio-Wilk normality test, if p >= 0.05 then it follows normal distribution (null hypothesis)
shapiro.test(d$Mg)
# 2. use Q-Q plot (quantile-quantile plot) to draw correlation between sample and theoretical normal distribution
ggqqplot(d$Mg, ylab = "Mg")
# test correlation of 2 parameters
res <- cor.test(d$Mg, d$Ca, method = "kendall")
# get p-value res$p.value
# get correlation coefficient res$estimate
# so use Kendall or Spearman
#corr <- rcorr(d[,-1], use = "complete.obs", method = "kendall") # better, but package Hmisc needed
corr <- cor(d[,-1], use = "complete.obs", method = "kendall")
p_mat <- cor_pmat(d[,-1])
#ggcorrplot(corr, method = "circle", type = "lower", p.mat = p_mat)
png(paste0(output, "Output/Statistics/figures/correlation_matrix_metals_terrestrial.png"), 
    width = 10, height = 10, res = 300, units = "cm")
corrplot(corr, method = "circle", type = "lower", order = "hclust", 
         p.mat = p_mat, sig.level = 0.05, insig = "blank")
dev.off()

corrplot(corr, tl.col = "brown", tl.srt = 30, bg = "white",
         type = "full")

# only groundwater
d <- data %>%
  filter(year == 2021,
         !is.na(value),
         method == "ICP-MS",
         sampletype == "groundwater") %>%
  # remove Ag and Cd since all values are <dl
  filter(!parameter %in% c("Ag", "Cd")) %>%
  select(samplecode, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value)

corr <- cor(d[,-1], use = "complete.obs", method = "kendall")  
corrplot(corr, method = "circle", type = "lower", #order = "hclust", 
         p.mat = p_mat, sig.level = 0.05, insig = "blank")

#### PCA ####
d <- data %>%
  filter(year == 2021,
         !is.na(value),
         method == "ICP-MS") %>%
  # remove Ag and Cd since all values are <dl
  filter(!parameter %in% c("Ag", "Cd")) %>%
  select(samplecode, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value)

g <- ggbiplot(d,
              obs.scale = 1,
              var.scale = 1,
              groups = training$Species,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)
 

#### Plots ####

## boxplots
d <- data %>%
  filter(year == 2021,
         parameter %in% c("EC", "pH", "HCO3", "Cl", "SO4", "Na", "Ca", "Mg", "K", "Fe", "NH4", "NO3", "PO4")) 

# per sampletype  
ggplot(d, aes(x = watercode, y = value)) +
  geom_boxplot() +
  facet_wrap(facets = "parameter", scales = "free")

# in groundwater
ggplot(d %>% filter(watercode == "GW"), aes(x = geology, y = value)) +
  geom_boxplot() +
  facet_wrap(facets = "parameter", scales = "free")
  
  
  
  
  
  
  
  
  





