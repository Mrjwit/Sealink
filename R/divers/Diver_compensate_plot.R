#
# Input: Raw csv file from diver pressure data
#         
# Output: Cleaned and compensated groundwater levels
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: XX-XX-XXXX
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, lubridate)

###############################################################################
# load data
###############################################################################

## Piscadera Catchment
# set data file location
input <- paste0("C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/Divers/",
                "Well data/Diver groundwater level timeseries/Piscadera_Catchment/") 
                
d1 <- "W7519_4N_54_46_2_Amarildo_14_11_to_24_12_2022_Uncompensated.csv"
d2 <- "Diver_T7951_Rooi_Piscadera_2_12_to_27_12_2022.CSV"

# merge 


## Klein Kwartier catchment


## Soto catchment


# data file
dat <- read.csv(paste0(input, filename), 
                skip = 49, sep = ";", dec = ",")

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################

# rename columns
# remove last row
d <- dat %>%
  head(-1) %>%
  # set date and time
  mutate(datetime = parse_date_time(Date.time, "%Y/%m/%d %H:%M:%S")) %>%
  mutate(date = as.POSIXct(datetime, format = "%m/%d/%Y")) %>%
  mutate(time = format(datetime, "%H:%M:%S"))

### Adjust for atmospheric pressure


### add manual groundwater measurements


### add rain data


## graphs
ggplot(d, aes(x = datetime, y = Pressure.cmH2O.)) +
  geom_point() +
  scale_x_datetime("", date_labels = "%d-%m-%Y") +
  theme_bw()

