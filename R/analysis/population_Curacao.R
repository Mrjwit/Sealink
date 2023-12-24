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

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/Population/" 

# load cleaned data
pop <- openxlsx::read.xlsx(paste0(input, "Population_Curacao.xlsx"))
tourist <- openxlsx::read.xlsx(paste0(input, "Tourism_Curacao.xlsx"))

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
        vjust = 3,
        margin = margin(0, 0 ,5, 0)),
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

# adjust tourist format
d_tour <- tourist %>%
  select(Year:est.cruise) %>%
  rename(`Cruise arrivals` = Cruise_arrivals,
         `Day trippers` = day_trippers,
         Stayovers = stayovers) %>%
  pivot_longer(., cols = `Cruise arrivals`:Stayovers,
               names_to = "tourist_group",
               values_to = "amount")

################################################
# Plots
################################################

# Population evolution Curacao
p1 <- ggplot(pop, aes(x = year, y = population)) +
  geom_point(shape = 21, fill = "steelblue", size = 2.5) +
  geom_line(linetype = "dashed") +
  labs(x = "", y = "",
       title = "Population Curacao") +
  theme_mw()

# Tourism evolution Curacao
p2 <- ggplot(d_tour, aes(x = Year, y = amount, fill = tourist_group)) +
  geom_bar(position = "stack", stat = "identity", colour = "black") +
  geom_line(aes(x = Year, y = est.stayovers), colour = "blue", linetype = "dashed") +
  geom_line(aes(x = Year, y = est.cruise+est.stayovers), colour = "red", linetype = "dashed") +
  #geom_smooth(aes(x = Year, y = total_visitors)) +
  labs(x = "", y = "",
       title = "Number of tourists Curacao") +
  theme_mw() + 
  theme(legend.position = c(0.3, 0.85))

cowplot::plot_grid(p1, p2, labels = "AUTO")

# combined population + tourist plot?
p1 <- ggplot() +
  geom_bar(data = d_tour %>% filter(Year != 2023), aes(x = Year, y = amount, fill = tourist_group),
           position = "stack", stat = "identity", colour = "black", alpha = 0.7) +
  geom_line(data = d_tour %>% filter(Year < 2023), 
            aes(x = Year, y = est.stayovers), colour = "blue", linetype = "dashed") +
  geom_line(data = d_tour %>% filter(Year < 2023), 
            aes(x = Year, y = est.cruise+est.stayovers), colour = "red", linetype = "dashed") +
  geom_point(data = pop, aes(x = year, y = population), size = 2.5) +
  geom_line(data = pop, aes(x = year, y = population)) +
  scale_fill_discrete(name = "tourist group") +
  labs(x = "", y = "", title = "Curacao population and tourist visits") +
  theme_mw() +
  theme(legend.position = c(0.25, 0.85),
        legend.background = element_blank())

p2 <- ggplot() +
  geom_bar(data = d_tour %>% filter(Year != 2023), aes(x = Year, y = amount, fill = tourist_group),
           position = "stack", stat = "identity", colour = "black", alpha = 0.7) +
  geom_point(data = pop, aes(x = year, y = population), size = 2.5) +
  geom_line(data = pop, aes(x = year, y = population)) +
  scale_fill_discrete(name = "tourist group") +
  labs(x = "", y = "", title = "only recent years") +
  coord_cartesian(xlim = c(2006, 2023)) +
  theme_mw() +
  theme(legend.position = c(0.25, 0.85),
        legend.background = element_blank())

cowplot::plot_grid(p1, p2, labels = "AUTO")


