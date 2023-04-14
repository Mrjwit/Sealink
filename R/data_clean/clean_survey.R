#
# Input: Survey data file for collection of field measurements in Curacao, 
# 2021-2022.
#         
# Output: Cleaned field measurements file for further hydrochemical analysis
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 12-01-2022
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
               sf, leaflet, data.table, cowplot, data.table)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/" 

# survey data file first fieldwork 2021-2022
survey <- read.xlsx(paste0(input, "Hydrochemie/Survey_data_Oct21_Jan22.xlsx"))
survey_old <- read.xlsx(paste0(input, "Hydrochemie/Survey_data.xlsx"))

# second fieldwork 2022-2023
survey2 <- read.xlsx(paste0(input, "Hydrochemie/Survey_data_second_fieldwork.xlsx"))[106:231,]

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################

# merge survey data fieldwork 1 (2021-2022) and 2 (2022-2023)
survey <- rbind(survey %>% 
                  select(-c(91, 96, 97, 110, 134, 139)) %>%
                  mutate("Well.code.(Titus)" = "",
                         "EC.profile" = "",
                         "Note.on.dipper.measurements" = ""), 
                survey2 %>%
                  select(-c(91, 96, 97, 110, 134, 139)))

# clean up the survey data
d1 <- survey %>%
  # remove completely empty columns
  select(where(~ !(all(is.na(.)) | all(. == "")))) %>%
  # select only records with samples
  filter(!is.na(`Sample.code.#1`)) %>%
  # when 2 samples are collected on 1 location, these are stored in the same record
  # rename columns from sample measurements in order to add second samples later on new lines
  rename(samplecode = `Sample.code.#1`,
         sample.depth.top.well.edge = `Sample.depth.top.well.edge.(m).#1`,
         DO = `DO.(mg/L).#1`,
         DO_sat = `DO.(saturation.%).#1`,
         EC_uS = `EC.(uS/cm).#1`,
         EC_mS = `EC.(mS/cm).#1`,
         pH = `pH.#1`,
         Temp = `Temperature.(°C).#1`,
         NO3_N = `NO3-N.(mg/L).#1`,
         NO2_N = `NO2-N.(mg/L).#1`,
         NO3 = `NO3.(mg/L).#1`,
         NO2 = `NO2.(mg/L).#1`,
         sample_method = `Sample.method.#1`,
         sample_depth = `Sample.depth.surface.(m)`,
         redox = `Redox.#1`,
         sample_notes = `Note.on.hydrochemistry.well.sample.1`)

# select second samples collected on same location
d2 <- d1 %>%
  filter(!is.na(`Sample.code.#2`)) %>%
  select(-c(samplecode:NO2, 
            sample_method, sample_depth,
            redox)) %>%
  # rename columns from sample measurements in order to merge with first samples later
  rename(samplecode = `Sample.code.#2`,
         sample.depth.top.well.edge = `Sample.depth.top.well.edge.(m).#2`,
         DO = `DO.(mg/L).#2`,
         DO_sat = `DO.(saturation.%).#2`,
         EC_uS = `EC.(uS/cm).#2`,
         EC_mS = `EC.(mS/cm).#2`,
         pH = `pH.#2`,
         Temp = `Temperature.(°C).#2`,
         NO3_N = `NO3-N.(mg/L).#2`,
         NO2_N = `NO2-N.(mg/L).#2`,
         NO3 = `NO3.(mg/L).#2`,
         NO2 = `NO2.(mg/L).#2`,
         sample_method = `Sample.method.#2`,
         sample_depth = `Sample.depth.surface.(m).#2`,
         redox = `Redox.(mV).#2`)

# merge two sample datasets and remove second samples columns
d <- d1 %>%
  select(-c(setdiff(names(d1), names(d2)))) %>%
  rbind(., d2)

# add date and time column
d <- d %>%
  mutate(datetime = as.POSIXct(Date.and.time * (60*60*24),
                               origin = "1899-12-30",
                               tz = "America/Curacao")) %>%
  mutate(date = strftime(datetime, format = "%d-%m-%Y"),
         time = strftime(datetime, format = "%H:%M:%S", tz = "America/Curacao"))

# check xy coordinates and add them from old survey file if necessary
survey_coord <- survey_old %>%
  select(sample, xcoord, ycoord) %>%
  filter(!is.na(sample)) %>%
  rename(samplecode = sample)

d <- d %>%
  left_join(., survey_coord) %>%
  mutate(xcoord = ifelse(is.na(xcoord), x, xcoord),
         ycoord = ifelse(is.na(ycoord), y, ycoord))

# check coordinates
# d %>% select(samplecode, x, y, xcoord, ycoord) %>% 
#   mutate(verschilx = ifelse(x == xcoord, 0, 1),
#          verschily = ifelse(y == ycoord, 0, 1)) %>%
#   mutate(xc = ifelse(is.na(xcoord), x, xcoord),
#          yc = ifelse(is.na(ycoord), y, ycoord)) %>%
#   view()
  
# reorder columns and select relevant columns for hydrochemistry
d_set <- d %>%
  select(samplecode, Well.ID, xcoord, ycoord, date, time, 
         `Well.depth.below.surface.(m)`, `Depth.of.well.owner.(m)`, `Groundwater.level.below.surface.(m)`,
         sample_method, sample_depth,
         EC_uS, EC_mS, pH, Temp, DO, DO_sat, redox, NO3_N, NO3, NO2_N, NO2, 
         sample_notes,
         `Geology.according.to.geological.map.(Beets)`, `Other.-.Geology.according.to.geological.map.(Beets)`,
         Geology.based.on.own.observations, Land.use.according.to.land.planning.map, 
         Land.use.based.on.own.observations, `Other.-.Land.use.based.on.own.observations`,
         Well.type, `Other.-.Well.type`, `Inner.well.diameter.(m)`, Casing.type.inner, Depth.casing.inner,
         `House./.location.waste.water.collection`, `Well.distance.from.cesspit.or.septic.tank.(m)`, `Well.distance.from.house.(m)`,
         `Note.on.sewage.(in.the.area)`, Note.on.the.well.type, Note.on.well.identification,
         Name.owner, Address, `Contact.mail/phone.number:`) %>%
  arrange(samplecode) %>%
  unique()

###############################################################################
# save data
###############################################################################

write.xlsx(d, paste0(output, "Clean_data/survey_complete.xlsx"))
write.xlsx(d_set, paste0(output, "Clean_data/survey_clean.xlsx"))


