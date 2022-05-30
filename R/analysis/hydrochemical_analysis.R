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
pacman::p_load(tidyverse, openxlsx, ggmap, devtools,
               sf, cowplot, scales, corrplot, Hmisc, ggpubr,
               ggbiplot, ggcorrplot, psych, FactoMineR, polynom)

install_github("vqv/ggbiplot")
library(plyr)
library(dplyr)
library(ggbiplot)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Clean_data/" 

# load cleaned data
data <- openxlsx::read.xlsx(paste0(input, "hydrochemistry_curacao.xlsx"))
metadata <- openxlsx::read.xlsx(paste0(input, "metadata_2021.xlsx"))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# Editing data
###############################################################################

# add geology and land use
data <- data %>%
  left_join(., metadata %>% select(samplecode, geology, geology_abr))

# metadata %>% 
#   group_by(Geology) %>%
#   dplyr::summarise(n())
# 
# metadata %>%
#   group_by(Land.use.based.on.own.observations, `Other.-.Land.use.based.on.own.observations`) %>%
#   dplyr::summarise(n()) 


###############################################################################
# Data analysis
###############################################################################

#### Piper diagram ####

# Convert concentrations to meq/l and place in wide format
an <- c("Cl", "HCO3", "NO3", "PO4", "SO4")
cat <- c("Na", "Ca", "Mg", "Fe", "K", "NH4")

d <- data %>%
  filter(year == 2021, 
         parameter %in% c(an, cat, "EC_uS")) %>%
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
    parameter == "EC_uS" ~ value,
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
  select(samplecode, geology, parameter, meql, sampletype) %>%
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

dat <- toPercent(d) %>%
  select(samplecode, sampletype, geology, EC_uS, Cl, SO4, HCO3, Na, K, Ca, Mg)

# takes percentage data and converts them to xy coordinates for plotting
transform_piper_data <- function(Mg, Ca, Cl, SO4, name=samplecode, 
                                 sampletype=sampletype, geology=geology, EC_uS=EC_uS){
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
  data.frame(observation=name,x=c(x1, x2, npoints$x), y=c(y=y1, y2, npoints$y), 
             sampletype = sampletype, geology = geology, EC = EC_uS)
}

piper.data <- transform_piper_data(dat$Mg, dat$Ca, dat$Cl, dat$SO4, dat$samplecode, dat$sampletype, dat$geology, dat$EC_uS)
piper.data <- piper.data %>%
  mutate(date = 2021)

ggplot_piper <- function(piper.data, output = c("ggplot","plotly"), scale = sampletype) {
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
                   axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())
    
    ### Plot data
    if(scale == "sampletype"){
      p <- p + ggplot2::geom_point(data=piper.data, 
                                   ggplot2::aes(x,y, colour=factor(sampletype),
                                                shape = sampletype, 
                                                text = paste(observation,
                                                             '</br></br>Date: ',
                                                             date))) + 
        scale_colour_manual(name = "sample type",
                            values = c("#F8766D", "#00BFC4", "#00BA38",
                                       "#B79F00", "#619CFF", "#C77CFF")) +
        scale_shape_manual(name = "sample type",
                           values = c(19, 17, 15, 3, 7, 8))
    }  
  
    if(scale == "geology"){
      p <- p + ggplot2::geom_point(data=piper.data, 
                                   ggplot2::aes(x,y, colour=factor(geology),
                                                text = paste(observation,
                                                             '</br></br>Date: ',
                                                             date))) + 
        scale_colour_manual(name = "geology",
                            values = c("yellowgreen", "seagreen4", "lightskyblue2",
                                       "purple4", "coral1", "yellow2", "grey50"))
    }  
    
    if(scale == "EC") {
      p <- p + ggplot2::geom_point(data=piper.data, 
                                   #ggplot2::aes(x,y, size = EC, colour = cut(EC, breaks = c(0, 1000, 5000, 10000, 20000, 50000)),
                                   ggplot2::aes(x,y, size = EC, colour = EC,
                                                text = paste(observation,
                                                             '</br></br>Date: ',
                                                             date)),
                                                alpha = 0.7) +
        # scale_colour_continuous(breaks = c(1000, 5000, 10000, 20000, 50000),
        #                         type = "viridis") +
        scale_colour_distiller(name = "EC (uS/cm)",
                               breaks = c(1000, 5000, 10000, 20000, 50000),
                               palette = "Spectral",
                               direction = -1) +
        scale_size_continuous(name = "EC (uS/cm)",
                              breaks = c(1000, 5000, 10000, 20000, 50000)) +
        guides(color = guide_legend(), size = guide_legend())
        # scale_size(name = "EC (uS/cm)",
        #            breaks = c(1000, 5000, 10000, 20000, 50000))
    }
  
    p <- p + ggplot2::annotate(
    geom = "text",
    label = c("1", "2", "3", "4", "5", "6"),
      x = c(70, 110, 150, 110, 110, 105),
      y = c(103.92, 175, 103.92, 32, 75, 138.5648)
    ) +
    ggplot2::geom_text(ggplot2::aes(
      #geom = "text",
      x = -10,
      y = c(170, 160, 150, 140, 130, 120, 110),
      #label = "Alkalinity~as~HCO[3]^'-'"
      #label = "1 Ca-HCO[3]\n 2 Ca-Cl\n 3 Na-Cl\n 4 Na-HCO[3]\n 5 Mixed type"
      label = c("Water~type", "1~Ca-`HCO`[3]", "2~Ca-`Cl`", "3~Na-`Cl`", 
                "4~Na-`HCO`[3]", "5~Mixed~Na-Ca-`HCO`[3]", "6~Mixed~Ca-Mg-`Cl`")
    ), parse=TRUE,
    hjust = 0) + 
      theme(legend.position = c(0.9, 0.72))
  
  
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

# Make piper diagram with different versions using geology, EC and watertype
ggplot_piper(piper.data, output = "ggplot", scale = "sampletype")
ggsave(paste0(output, "Output/Figures/Hydrochemistry/piperdiagram_2021_allsamples_per_sampletype.png"))
ggplot_piper(piper.data, output = "ggplot", scale = "geology")
ggsave(paste0(output, "Output/Figures/Hydrochemistry/piperdiagram_2021_allsamples_per_geology.png"))
ggplot_piper(piper.data, output = "ggplot", scale = "EC")
ggsave(paste0(output, "Output/Figures/Hydrochemistry/piperdiagram_2021_allsamples_per_EC.png"))

# just groundwater
ggplot_piper(piper.data %>% filter(sampletype == "groundwater"), output = "ggplot", scale = "geology")
ggsave(paste0(output, "Output/Figures/Hydrochemistry/piperdiagram_2021_groundwater_per_geology.png"))
ggplot_piper(piper.data %>% filter(sampletype == "groundwater"), output = "ggplot", scale = "EC")
ggsave(paste0(output, "Output/Figures/Hydrochemistry/piperdiagram_2021_groundwater_per_EC.png"))

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


#### Hierarchical Cluster Analysis #### 

# Do it only for 2021
data %>%
  filter(year == 2021) %>%
  filter(parameter == "Fe", sampletype == "groundwater") %>%
  select(sampletype, parameter, value, units, limit_symbol) %>%
  view()
  summary()
  ggplot(aes(y = value)) %>%
  geom_boxplot()

data %>%
  filter(!parameter %in% c("clustermember", "c1", "c2", "c3", "c4", "Diepte", "RSC", "SAR")) %>%
  filter(!parameter %in% c("HCO3_field", "HCO3_lab", "NO3_field", "NO2_field", "DO_sat")) %>%
  group_by(year) %>%
  summarise(n.param = n_distinct(parameter),
            params = paste(sort(unique(parameter)), collapse = ", ")) %>%
  view()

d %>%
  filter(parameter == "EC_uS", sampletype == "groundwater") %>%
  ggplot(aes(y = value)) +
  geom_boxplot()


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
corrplot(corr, method = "circle", type = "lower", order = "hclust", 
         p.mat = p_mat, sig.level = 0.05, insig = "blank")

#### PCA ####
# example
data(wine)
wine.pca <- prcomp(wine, scale. = TRUE)
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         groups = wine.class, ellipse = TRUE) +
  scale_color_discrete(name = "") +
  theme(legend.direction = "horizontal", legend.position = "top")

## first use a subselection to test
d <- data %>%
  filter(year == 2021,
         #sampletype == "groundwater",
         method != "IA",
         !is.na(value),
         !units %in% c("mg N/L", "mg P/L"),
         samplecode != "SP001B") %>%
  # remove PO4 with IC measurements
  mutate(remove = ifelse(method == "IC" & parameter == "PO4", 1, 0)) %>%
  filter(remove == 0) %>%
  # remove Ag and Cd since all values are <dl and double parameters
  filter(parameter %in% c("pH", "HCO3", "Cl", "SO4", "NO3", "PO4", 
                          "Ca", "Na", "Mg", "K", "Fe", "Mn", "NH4", 
                          "B", "Si", "V", "Zn", "Ni")) %>%
  select(samplecode, sampletype, subtype, parameter, value) %>%
  # normalize/standardize concentrations using z-score, this can also be done automatically within prcomp using scale = T
  #group_by(parameter) %>%
  #mutate(zscore = (value - mean(value)) / sd(value)) %>%
  #mutate(scaled = value - mean(value)) %>%
  #mutate(logval = log(value + 1)) %>%
  #ungroup() %>%
  # either use concentrations or zcore. Remove the other
  #select(-scaled) %>%
  pivot_wider(names_from = parameter,
              values_from = value) %>%
  na.omit()

d_long <- d %>% pivot_longer(cols = B:Zn, names_to = "parameter", values_to = "conc")

# check standardized concentrations
ggplot(d_long, aes(x = parameter, y = conc)) +
  geom_boxplot() +
  theme_bw() 
  #facet_wrap(~parameter, scales = "free")


# first make correlation matrix
corr <- cor(d[,-c(1:3)], use = "complete.obs", method = "kendall")
p_mat <- cor_pmat(d[,-c(1:3)])
corrplot(corr, method = "number", order = "hclust", type = "lower",
         p.mat = p_mat, sig.level = 0.05, insig = "blank")

#d.pca <- prcomp(d %>% select(-samplecode, -sampletype, -subtype), scale. = TRUE)
d.pca <- princomp(d %>% select(-samplecode, -sampletype, -subtype), cor = T)
d.pca <- PCA(d %>% select(-samplecode, -sampletype, -subtype))

# get overview of sd and variance explained per principal component
summary(d.pca)

screeplot(d.pca)
# make own screeplot
varExp = (100*d.pca$sdev^2/sum(d.pca$sdev^2))
varDF = data.frame(Dimensions=1:length(varExp),
                   varExp = varExp,
                   varCum = cumsum(varExp))
ggplot(varDF, aes(x=Dimensions, y = varExp)) +
  geom_point() +
  geom_col(fill = "steelblue") +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = 1:nrow(varDF)) +
  ylab("% variance explained")


# get the loadings/coefficient per PC for all parameters
# or the contribution of the variables to the nth PC
round(unclass(d.pca$loadings[, 1:6]), digits = 2)
#round(unclass(d.pca$rotation[, 1:6]), digits = 2) 

ggbiplot(d.pca,
         obs.scale = 1, var.scale = 1
         #groups = d.class
) +
  scale_color_discrete(name = "") +
  theme_bw() +
  theme(legend.direction = "horizontal", legend.position = "bottom")


# for metals in groundwater only
d <- data %>%
  filter(year == 2021,
         sampletype == "groundwater",
         !is.na(value),
         method == "ICP-MS") %>%
  # remove Ag and Cd since all values are <dl
  filter(!parameter %in% c("Ag", "Cd")) %>%
  select(samplecode, sampletype, subtype, parameter, value) %>%
  pivot_wider(names_from = parameter,
              values_from = value) %>%
  na.omit() # this excludes some samples that have NA for certain elements

## standardize concentrations through log transformation and z-scores in order to 
## eliminate influence of different units between variables and acquire a normal/log-normal distribution

d.class <- dplyr::pull(d, subtype)
d.pca <- prcomp(d %>% select(-samplecode, -sampletype, -subtype), scale. = TRUE)
ggbiplot(d.pca,
         obs.scale = 1, var.scale = 1,
         groups = d.class) +
  scale_color_discrete(name = "") +
  theme_bw() +
  theme(legend.direction = "horizontal", legend.position = "bottom")

## for all parameters
d <- data %>%
  filter(year == 2021,
         #sampletype == "groundwater",
         method != "IA",
         !is.na(value),
         !units %in% c("mg N/L", "mg P/L"),
         samplecode != "SP001B") %>%
  # remove PO4 with IC measurements
  mutate(remove = ifelse(method == "IC" & parameter == "PO4", 1, 0)) %>%
  filter(remove == 0) %>%
  # remove Ag and Cd since all values are <dl and double parameters
  filter(!parameter %in% c("Ag", "Cd", "Temp", "HCO3_lab", "HCO3_field",
                           "NO3_field", "NO2_field", "DO_sat", "S", "P", "Rn")) %>%
  select(samplecode, sampletype, subtype, parameter, value) %>%
  # normalize/standardize concentrations using z-score, this can also be done automatically within prcomp using scale = T
  group_by(parameter) %>%
  mutate(zscore = (value - mean(value)) / sd(value)) %>%
  ungroup() %>%
  # either use concentrations or zcore. Remove the other
  select(-value) %>%
  pivot_wider(names_from = parameter,
              values_from = zscore) %>%
  na.omit()

d.class <- dplyr::pull(d, subtype)
d.pca <- prcomp(d %>% select(-samplecode, -sampletype, -subtype), scale. = TRUE)

screeplot(d.pca)

ggbiplot(d.pca,
         obs.scale = 1, var.scale = 1,
         groups = d.class) +
  scale_color_discrete(name = "") +
  scale_x_continuous(limits = c(-3, 10)) +
  scale_y_continuous(limits = c(-9, 5)) +
  theme_bw() +
  theme(legend.direction = "horizontal", legend.position = "bottom")

# step by step
# compute variance-covariance matrix (correlation matrix) and show eigenvalues

d.pca <- princomp(d %>% select(-samplecode, -sampletype, -subtype), cor = T)

summary(d.pca)

d.pca$loadings %>% view()
d.pca$scores %>% view()
screeplot(d.pca)
d.pca$sdev

corr <- cor(d[,-c(1:3)], use = "complete.obs", method = "kendall")
corrplot(corr, method = "number")

KMO(d[,-c(1:3)])


### explanations

## mean centering to focus shift from absolute concentrations
# to fluctuations in the concentrations



## scaling to remove effects of the variance (size of fluctuations)
# all variables become equally important. This is optional



## dimension reduction: remove correlated variables


#### Biplots ####
## Plots in meq/l 
# use dataset of concentrations converted to meq/l
an <- c("Cl", "HCO3", "NO3", "PO4", "SO4")
cat <- c("Na", "Ca", "Mg", "Fe", "K", "NH4")

d_meql <- data %>%
  filter(year == 2021, 
         parameter %in% c(an, cat),
         samplecode != "SP001B",
         sampletype == "groundwater") %>%
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
    parameter == "EC_uS" ~ value,
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
  select(samplecode, parameter, meql, sampletype) %>%
  pivot_wider(names_from = parameter,
              values_from = meql) %>%
  filter(!is.na(Cl))

# Alkalis Na+K vs Ca+Mg


# Ca+Mg vs HCO3+SO4
# influence of carbonate mineral dissolution (calcite/dolomite)
ggplot(d_meql, aes(x = Ca+Mg, y = HCO3+SO4)) + 
  geom_point() +
  geom_abline(linetype = "dashed", colour = "red") +
  scale_x_continuous(name = "(Ca+Mg) (meq/l)") +
  scale_y_continuous(name = "(HCO3+SO4) (meq/l)") +
  annotate(geom = "text", x = 25, y = 40, label = "carbonate mineral dissolution", angle = 74, colour = "steelblue") + 
  annotate(geom = "text", x = 100, y = 15, label = paste("extra source of Ca+Mg", "reverse ion exchange", sep = "\n") , angle = 0, colour = "red") + 
  ggtitle("(Ca+Mg) vs (HCO3+SO4)") +
  theme_bw()

# Ca vs HCO3
# Role of carbonate/silicate minerals weathering
ggplot(d_meql, aes(x = Ca, y = HCO3)) + 
  geom_point() +
  geom_abline(slope = 2, linetype = "dashed", colour = "red") +
  #geom_abline(linetype = "dashed", colour = "red") +
  scale_x_continuous(name = "Ca (meq/l)") +
  scale_y_continuous(name = "HCO3 (meq/l)") +
  annotate(geom = "text", x = 30, y = 38, label = "2:1 line", angle = 0, colour = "red") + 
  annotate(geom = "text", x = 55, y = 8, label = "extra source of Ca", angle = 0, colour = "red") + 
  ggtitle("Calcite dissolution") +
  theme_bw()

# Ca+Mg vs HCO3
# Role of dolomite dissolution


# Ca vs SO4
# influence of gypsum dissolution
ggplot(d_meql, aes(x = Ca, y = SO4)) + 
  geom_point() +
  geom_abline(linetype = "dashed", colour = "red") +
  scale_x_continuous(name = "Ca (meq/l)") +
  scale_y_continuous(name = "SO4 (meq/l)") +
  annotate(geom = "text", x = 45, y = 50, label = "1:1 line gypsum dissolution", angle = 59, colour = "steelblue") + 
  annotate(geom = "text", x = 45, y = 50, label = "1:1 line gypsum dissolution", angle = 59, colour = "steelblue") + 
  annotate(geom = "text", x = 45, y = 50, label = "1:1 line gypsum dissolution", angle = 59, colour = "steelblue") + 
  ggtitle("Ca vs SO4") +
  theme_bw()

# Na vs Cl
# Halite dissolution ? 
ggplot(d_meql, aes(x = Na, y = Cl)) + 
  geom_point() +
  geom_abline(linetype = "dashed", colour = "red") +
  # add seawater ratio mixing line
  scale_x_continuous(name = "Na (meq/l)") +
  scale_y_continuous(name = "Cl (meq/l)") +
  ggtitle("Na vs Cl") +
  theme_bw() 

# Add plots together using cowplot

## plots of seawater mixing line!
#


## Biplots in concentrations (mg/l)

# EC vs Cl
d <- data %>%
  filter(year == 2021, 
         parameter %in% c("EC_uS", "Cl"),
         samplecode != "SP001B") %>%
  select(samplecode, parameter, value) %>%
  pivot_wider(values_from = value,
              names_from = parameter)

my_formula <- y ~ poly(x, 2, raw = T)
df <- data.frame("x" = d$Cl, "y" = d$EC_uS)
m <- lm(formula = my_formula, df)
my_eq <- as.character(signif(as.polynomial(coef(m)), 2))
label.text <- paste(gsub("x", "~italic(x)", my_eq, fixed = TRUE),
                    paste("italic(R)^2",  
                          format(summary(m)$r.squared, digits = 2), 
                          sep = "~`=`~"),
                    sep = "~~~~")

# Plot of relationship between EC and Cl
ggplot(df, aes(x = x, y = y)) +
  geom_point(alpha = 0.2, shape = 21, fill = "blue", colour = "black", size = 2.5) +
  geom_smooth(method = "lm", formula = my_formula, linetype = "dashed") +
  # stat_cor(label.y = 50000, label.x = 3000, 
  #          aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) + 
  # stat_regline_equation(label.y = 47000, label.x = 3000) + # this is for linear model, not for exp! 
  geom_vline(xintercept = 150, colour = "lightblue") + # fresh
  geom_vline(xintercept = 1000, colour = "orange") + # brackish
  geom_vline(xintercept = 3000, colour = "red1") + # saline ~ 15% seawater
  geom_vline(xintercept = 21000, colour = "red4") + # hypersaline water
  #geom_vline(xintercept = 10000, colour = "red") + # saline
  scale_x_continuous(name = "Cl (mg/l)") +
  scale_y_continuous(name = "EC (uS/cm)") +
  annotate(geom = "text", x = 8000, y = 44000, label = label.text,
           family = "serif", hjust = 0, parse = T, size = 4) +
  annotate(geom = "text", x = -600, y = 56000, label = "fresh", angle = 90, colour = "lightblue") + 
  annotate(geom = "text", x = 500, y = 55000, label = "fresh-brackish", angle = 90, colour = "palegreen3") + 
  annotate(geom = "text", x = 2000, y = 56000, label = "brackish", angle = 90, colour = "orange") + 
  annotate(geom = "text", x = 12000, y = 56000, label = "saline", angle = 0, colour = "red1") + 
  annotate(geom = "text", x = 25000, y = 56000, label = "hypersaline", angle = 0, colour = "red4") + 
  ggtitle("EC vs Cl") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# Ca vs HCO3
d <- data %>%
  filter(year == 2021, 
         parameter %in% c("Ca", "HCO3"),
         samplecode != "SP001B") %>%
  select(samplecode, parameter, value) %>%
  pivot_wider(values_from = value,
              names_from = parameter)

ggplot(d, aes(x = Ca, y = HCO3)) + 
  geom_point() +
  geom_abline(linetype = "dashed", colour = "red") +
  scale_x_continuous(name = "Ca (mg/l)") +
  scale_y_continuous(name = "HCO3 (mg/l)") +
  ggtitle("Ca vs HCO3") +
  theme_bw()


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
  
  
  
  
#### Overzicht voor ICP-OES data  ####
# 
# 
# ICP_elem <- data %>% filter(year == 2021,
#                             method == "ICP-MS") %>%
#             distinct(parameter)
# 
# # per sample
# d <- data %>%
#   filter(year == 2021, 
#          parameter %in% c("EC_uS", "Cl", ICP_elem$parameter),
#          samplecode != "SP001B") %>%
#   select(samplecode, parameter, limit_symbol, value, detection_limit, units) %>%
#   mutate(parameter = paste0(parameter, " (", units, ")"), 
#          value = ifelse(limit_symbol == "<", paste(limit_symbol, value), value)) %>%
#   select(samplecode, parameter, value) %>%
#   pivot_wider(values_from = value,
#               names_from = parameter) %>%
#   select(samplecode, `EC_uS (uS/cm)`, `Cl (mg/l)`, everything())
# write.xlsx(d, paste0(output, "ICP-MS_overzicht_per_sample.xlsx"))
# 
# # per element
# d <- data %>%
#   filter(year == 2021, 
#          parameter %in% c("EC_uS", "Cl", ICP_elem$parameter),
#          samplecode != "SP001B") %>%
#   group_by(parameter, units) %>%
#   dplyr::summarise(n = length(value[!is.na(value)]),
#                    `% <RG` = round(length(value[limit_symbol == "<"]) / length(value[!is.na(value)]) * 100, digits = 0),
#                    min = min(value, na.rm = T),
#                    p10 = quantile(value, 0.1, na.rm = T),
#                    med = median(value, na.rm = T),
#                    avg = mean(value, na.rm = T),
#                    p90 = quantile(value, 0.9, na.rm = T),
#                    max = max(value, na.rm = T)) 
# write.xlsx(d, paste0(output, "ICP-MS_overzicht_per_element.xlsx"))
  





