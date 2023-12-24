#
# Input: Data from boat surveying:
# 1. GPS file
# 2. Radon data
# 3. Ponsel multimeter raw dataset
#         
# Output: Cleaned Pumping data graphs
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 19-12-2023
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, lubridate, ggpubr, data.table, sf,
               ggspatial, viridis)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/Radon/Bays/" 

#### 18-12-23 ####
# read data GPS
gps <- read.csv(paste0(input, "2023-12-18/20231218-162146 - Piscadera_18-12_23.txt")) %>%
  # set date and change timezone
  mutate(datetime2 = with_tz(parse_date_time(date.time, "%Y-%m-%d %H:%M:%S"), tz_out = "AST")) %>%
  #mutate(datetime2 = parse_date_time(date.time, "%Y-%m-%d %H:%M:%S"), tz = "AST") %>%
  mutate(date = as.POSIXct(date.time, format = "%Y-%m-%d")) %>%
  mutate(time = format(datetime2, "%H:%M:%S")) %>%
  select(datetime2, latitude, longitude)

# read ponsol multimeter data
pons <- read.csv(paste0(input, "2023-12-18/Boat_survey_radon_PiscaderaBay_18-12-2023.csv"),
                        skip = 3) %>%
  rename("record" = "X.",
         "datetime" = "Date",
         "conductivity (µS/cm)" = "Conductivité..µS.cm.",
         "salinity (ppt)" = "Salinité..ppt.",
         "O2 (%)" = "O2...Sat.",
         "O2 (mg/L)" = "O2..mg.l.",
         "temp O2 (C)" = "Température...C.",
         "temp EC (C)" = "Température...C..1",
         "pH" = "PH..pH.",
         "redox (mV)" = "Redox..mV.") %>%
  #"temp pH (C)" = "Température...C..1") %>%
  # set date
  mutate(datetime2 = force_tz(parse_date_time(datetime, "%m/%d/%Y %I:%M:%S %p"), tz_out = "AST")) %>%
  mutate(date = as.POSIXct(datetime, format = "%m/%d/%Y")) %>%
  mutate(time = format(datetime2, "%H:%M:%S")) 
  #filter(time > "16:21:45")

# radon continuous measurements
rad <- read.csv(paste0(input, "2023-12-18/RAD7 05738 Data 2023-12-18_PiscaderaBay_Titus.csv")) %>%
  mutate(datetime2 = force_tz(parse_date_time(Full.Date, "%m/%d/%y %H:%M"), tz_out = "AST")) %>%
  mutate(date = as.POSIXct(Full.Date, format = "%m/%d/%j")) %>%
  rename(radon_water = Radon.In.Water..Dpm.L.,
         radon_water_sd = Radon.In.Water.2.Sigma.Uncert...Dpm.L.)

# RBR Titus
rbr1 <- read.delim(paste0(input, "2023-12-18/Merge_RBR_CTD_submersed_Deeper_Garmin_SGD_detection_Piscaderabay_18_12_2023.txt"),
                  sep = ",") %>%
  mutate(date = "2023-12-18")

# add GPS data from nearest timestamp to ponsel and radon data
df <- left_join(gps, pons) %>%
  left_join(., rad %>% select(datetime2, radon_water, radon_water_sd, Total.Counts)) %>%
  mutate(date = "2023-12-18")

#### 19-12-23 ####
# read data GPS
gps <- read.csv(paste0(input, "2023-12-19/20231219-164625 - PiscaderaBay_19-12-2023.txt")) %>%
  # set date and change timezone
  mutate(datetime2 = with_tz(parse_date_time(date.time, "%Y-%m-%d %H:%M:%S"), tz_out = "AST")) %>%
  #mutate(datetime2 = parse_date_time(date.time, "%Y-%m-%d %H:%M:%S"), tz = "AST") %>%
  mutate(date = as.POSIXct(date.time, format = "%Y-%m-%d")) %>%
  mutate(time = format(datetime2, "%H:%M:%S")) %>%
  select(datetime2, latitude, longitude)

# read ponsol multimeter data
pons <- read.csv(paste0(input, "2023-12-19/Boat_survey_radon_PiscaderaBay_19-12-2023.csv"),
                        skip = 3) %>%
  rename("record" = "X.",
         "datetime" = "Date",
         "conductivity (µS/cm)" = "Conductivité..µS.cm.",
         "salinity (ppt)" = "Salinité..ppt.",
         "O2 (%)" = "O2...Sat.",
         "O2 (mg/L)" = "O2..mg.l.",
         "temp O2 (C)" = "Température...C.",
         "temp EC (C)" = "Température...C..1",
         "pH" = "PH..pH.",
         "redox (mV)" = "Redox..mV.") %>%
  #"temp pH (C)" = "Température...C..1") %>%
  # set date
  mutate(datetime2 = force_tz(parse_date_time(datetime, "%m/%d/%Y %I:%M:%S %p"), tz_out = "AST")) %>%
  mutate(date = as.POSIXct(datetime, format = "%m/%d/%Y")) %>%
  mutate(time = format(datetime2, "%H:%M:%S")) 
  #filter(time > "16:21:45")

# radon continuous measurements
rad <- read.csv(paste0(input, "2023-12-19/RAD7 05738 Data 2023-12-19_PiscaderaBay_Titus.csv")) %>%
  mutate(datetime2 = force_tz(parse_date_time(Full.Date, "%m/%d/%y %H:%M"), tz_out = "AST")) %>%
  mutate(date = as.POSIXct(Full.Date, format = "%m/%d/%j")) %>%
  rename(radon_water = Radon.In.Water..Dpm.L.,
         radon_water_sd = Radon.In.Water.2.Sigma.Uncert...Dpm.L.)

# RBR Titus
rbr2 <- read.delim(paste0(input, "2023-12-19/Data_merge_RBR_CTD_submersed_Deeper_Garmin_SGD_detection_Piscaderabay_19_12_2023.txt"),
                  sep = ",") %>%
  mutate(date = "2023-12-19")

# add GPS data from nearest timestamp to ponsel and radon data
df2 <- left_join(gps, pons) %>%
  left_join(., rad %>% select(datetime2, radon_water, radon_water_sd, Total.Counts)) %>%
  mutate(date = "2023-12-19")

#### 20-12-23 ####
# read data GPS
gps <- read.csv(paste0(input, "2023-12-20/20231220-164024 - PiscaderaBay-20-12-2023.txt")) %>%
  # set date and change timezone
  mutate(datetime2 = with_tz(parse_date_time(date.time, "%Y-%m-%d %H:%M:%S"), tz_out = "AST")) %>%
  #mutate(datetime2 = parse_date_time(date.time, "%Y-%m-%d %H:%M:%S"), tz = "AST") %>%
  mutate(date = as.POSIXct(date.time, format = "%Y-%m-%d")) %>%
  mutate(time = format(datetime2, "%H:%M:%S")) %>%
  select(datetime2, latitude, longitude)

# read ponsol multimeter data
pons <- read.csv(paste0(input, "2023-12-20/Boat_survey_radon_PiscaderaBay_20-12-2023.csv"),
                 skip = 3) %>%
  rename("record" = "X.",
         "datetime" = "Date",
         "conductivity (µS/cm)" = "Conductivité..µS.cm.",
         "salinity (ppt)" = "Salinité..ppt.",
         "O2 (%)" = "O2...Sat.",
         "O2 (mg/L)" = "O2..mg.l.",
         "temp O2 (C)" = "Température...C.",
         "temp EC (C)" = "Température...C..1",
         "pH" = "PH..pH.",
         "redox (mV)" = "Redox..mV.") %>%
  #"temp pH (C)" = "Température...C..1") %>%
  # set date
  mutate(datetime2 = force_tz(parse_date_time(datetime, "%m/%d/%Y %I:%M:%S %p"), tz_out = "AST")) %>%
  mutate(date = as.POSIXct(datetime, format = "%m/%d/%Y")) %>%
  mutate(time = format(datetime2, "%H:%M:%S")) 
#filter(time > "16:21:45")

# radon continuous measurements
rad <- read.csv(paste0(input, "2023-12-20/RAD7 05738 Data 2023-12-20_PiscaderaBay_Titus.csv")) %>%
  mutate(datetime2 = force_tz(parse_date_time(Full.Date, "%m/%d/%y %H:%M"), tz_out = "AST")) %>%
  mutate(date = as.POSIXct(Full.Date, format = "%m/%d/%j")) %>%
  rename(radon_water = Radon.In.Water..Dpm.L.,
         radon_water_sd = Radon.In.Water.2.Sigma.Uncert...Dpm.L.)

# RBR Titus
rbr3 <- read.delim(paste0(input, "2023-12-20/Data_merge_RBR_CTD_submersed_Deeper_Garmin_SGD_detection_Piscaderabay_20_12_2023.txt"),
                   sep = ",") %>%
  mutate(date = "2023-12-20") %>%
  select(-Index)

# add GPS data from nearest timestamp to ponsel and radon data
df3 <- left_join(gps, pons) %>% # gps, pons
  left_join(., rad %>% select(datetime2, radon_water, radon_water_sd, Total.Counts)) %>%
  mutate(date = "2023-12-20")

#### 22-12-23 ####
# read data GPS
gps <- read.csv(paste0(input, "2023-12-22/20231222-150520 - PiscaderaBay_22-12-2023.txt")) %>%
  # set date and change timezone
  mutate(datetime2 = with_tz(parse_date_time(date.time, "%Y-%m-%d %H:%M:%S"), tz_out = "AST")) %>%
  #mutate(datetime2 = parse_date_time(date.time, "%Y-%m-%d %H:%M:%S"), tz = "AST") %>%
  mutate(date = as.POSIXct(date.time, format = "%Y-%m-%d")) %>%
  mutate(time = format(datetime2, "%H:%M:%S")) %>%
  select(datetime2, latitude, longitude)

# read ponsol multimeter data
pons <- read.csv(paste0(input, "2023-12-22/Boat_survey_radon_PiscaderaBay_22-12-2023.csv"),
                 skip = 3) %>%
  rename("record" = "X.",
         "datetime" = "Date",
         "conductivity (µS/cm)" = "Conductivité..µS.cm.",
         "salinity (ppt)" = "Salinité..ppt.",
         "O2 (%)" = "O2...Sat.",
         "O2 (mg/L)" = "O2..mg.l.",
         "temp O2 (C)" = "Température...C.",
         "temp EC (C)" = "Température...C..1",
         "pH" = "PH..pH.",
         "redox (mV)" = "Redox..mV.") %>%
  #"temp pH (C)" = "Température...C..1") %>%
  # set date
  mutate(datetime2 = force_tz(parse_date_time(datetime, "%m/%d/%Y %I:%M:%S %p"), tz_out = "AST")) %>%
  mutate(date = as.POSIXct(datetime, format = "%m/%d/%Y")) %>%
  mutate(time = format(datetime2, "%H:%M:%S")) 
#filter(time > "16:21:45")

# radon continuous measurements
rad <- read.csv(paste0(input, "2023-12-22/RAD7 05738 Data 2023-12-22_PiscaderaBay_Titus.csv")) %>%
  mutate(datetime2 = force_tz(parse_date_time(Full.Date, "%m/%d/%y %H:%M"), tz_out = "AST")) %>%
  mutate(date = as.POSIXct(Full.Date, format = "%m/%d/%j")) %>%
  rename(radon_water = Radon.In.Water..Dpm.L.,
         radon_water_sd = Radon.In.Water.2.Sigma.Uncert...Dpm.L.)

# RBR Titus
rbr4 <- read.delim(paste0(input, "2023-12-22/Data_merge_RBR_CTD_submersed_Deeper_Garmin_SGD_detection_Piscaderabay_22_12_2023.txt"),
                   sep = ",") %>%
  mutate(date = "2023-12-22") %>%
  select(-Index)

# add GPS data from nearest timestamp to ponsel and radon data
df4 <- left_join(gps, pons) %>% # gps, pons
  left_join(., rad %>% select(datetime2, radon_water, radon_water_sd, Total.Counts)) %>%
  mutate(date = "2023-12-22")

df <- rbind(df, df2, df3, df4)

# convert to sf for spatial plotting
pts <- st_as_sf(df %>% filter(!is.na(longitude)), coords = c("longitude", "latitude"),
                crs = 4326, agr = "constant")
write.csv(df, paste0(input, "data_combined.csv"))
write.csv(df %>% filter(!is.na(`conductivity (µS/cm)`)), paste0(input, "data_EC_combined.csv"))

# importing GIS layers
input_GIS <- "C:/Users/mikewit/Documents/SEALINK/GIS/SEALINK/"

cur_coastline <- st_read(paste0(input_GIS,
                                "Layers/Topography/CUR_Coastline.shp")) %>%
  st_transform(crs = 4326)
cur_geology <- st_read(paste0(input_GIS,
                              "Layers/Geology/Geology Shapefile EATH.shp")) %>%
  st_transform(crs = 4326) 

catchments <- st_read(paste0(input_GIS,
                             "Layers/Catchments/Curacao_SAGA_catchments_Stahler_6.shp")) %>%
  st_transform(crs = 4326)


#### Plotting ####

# Radon timeseries
i <- "2023-12-22"
p1 <- ggplot(df %>% filter(date == "2023-12-22"), aes(x = datetime2, y = radon_water)) +
  geom_line() +
  geom_errorbar(aes(ymin = radon_water - radon_water_sd, ymax = radon_water + radon_water_sd),
                width = 200, colour = "darkgrey") +
  geom_point() +
  labs(x = "", y = "Rn [dpm/L]",
       title = "Radon survey Piscadera Bay 22-12-2023") +
  coord_cartesian(ylim = c(0, 2)) +
  theme_bw()

p2 <- ggplot(df %>% filter(date == "2023-12-22"), aes(x = datetime2, y = `Total.Counts`)) +
  geom_line() +
  geom_point() +
  labs(x = "", y = "Counts") +
  theme_bw()

p3 <- ggplot() +
  #geom_sf(data = cur_coastline, fill = "grey", alpha = 0.5) +
  geom_sf(data = pts %>% filter(date == "2023-12-22"), size = 0.05, colour = "black") +
  geom_sf(data = pts %>% filter(date == "2023-12-22", !is.na(radon_water)), 
          aes(fill = radon_water), 
          colour = "black",
          pch = 21,
          size = 3) +
  scale_fill_viridis("Rn [dpm/L]") +
  # coord_sf(xlim = c(-68.98, -68.96),
  #          ylim = c(12.12, 12.14)) +
  # ggtitle("Radon concentrations in groundwater on Curacao") + 
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true",
  #                        pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  # theme(panel.grid.major = element_line(color = gray(.5), 
  #                                       linetype = "dashed", 
  #                                       linewidth = 0.5), 
  #       panel.background = element_rect(fill = "aliceblue"))
  theme_mw()

p <- ggpubr::ggarrange(p1, p2, ncol = 1)
ggpubr::ggarrange(p, p3)

# EC + O2 + map
p1 <- ggplot(df %>% filter(date == "2023-12-22",
                           `conductivity (µS/cm)` > 50), aes(x = datetime2, y = `conductivity (µS/cm)`)) +
  geom_line() +
  geom_point() +
  labs(x = "", y = "EC [uS/cm]]",
       title = "EC + O2 survey Piscadera Bay 22-12-2023") +
  #coord_cartesian(ylim = c(0, 2)) +
  theme_bw()

p2 <- ggplot(df %>% filter(date == "2023-12-22",
                           `O2 (%)` < 5000 &  `O2 (%)` > 0), aes(x = datetime2, y = `O2 (%)`)) +
  geom_line() +
  geom_point() +
  labs(x = "", y = "O2 [%]") +
  theme_bw()

p3 <- ggplot() +
  #geom_sf(data = cur_coastline, fill = "grey", alpha = 0.5) +
  geom_sf(data = pts %>% filter(date == "2023-12-22"), size = 0.05, colour = "black") +
  geom_sf(data = pts %>% filter(date == "2023-12-22", 
                                `O2 (%)` < 5000 &  `O2 (%)` > 0,
                                #`conductivity (µS/cm)` > 50,
                                !is.na(`conductivity (µS/cm)`)), 
          #aes(fill = `conductivity (µS/cm)`), 
          aes(fill = `O2 (%)`), 
          colour = "black",
          stroke = NA,
          pch = 21,
          size = 3) +
  #scale_fill_viridis("EC [uS/cm]") +
  scale_fill_viridis("O2 sat [%]") +
  # coord_sf(xlim = c(-68.98, -68.96),
  #          ylim = c(12.12, 12.14)) +
  # ggtitle("Radon concentrations in groundwater on Curacao") + 
  # annotation_scale(location = "bl", width_hint = 0.5) +
  # annotation_north_arrow(location = "bl", which_north = "true",
  #                        pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  # theme(panel.grid.major = element_line(color = gray(.5), 
  #                                       linetype = "dashed", 
  #                                       linewidth = 0.5), 
  #       panel.background = element_rect(fill = "aliceblue"))
theme_mw()

p <- ggpubr::ggarrange(p1, p2, ncol = 1)
ggpubr::ggarrange(p, p3)



# EC/Temp/pH/O2 timeseries
# 18-12-2023
p1 <- ggplot(df %>% filter(date == "2023-12-18"), aes(x = datetime2, y = `conductivity (µS/cm)`)) +
  geom_line() +
  geom_point() +
  labs(x = "") +
  theme_bw()
p2 <- ggplot(df %>% filter(date == "2023-12-18", `temp EC (C)` > 0), 
             aes(x = datetime2, y = `temp EC (C)`)) +
  geom_line() +
  geom_point() +
  labs(x = "") +
  theme_bw()
p3 <- ggplot(df %>% filter(date == "2023-12-18", `O2 (%)` > 0), 
             aes(x = datetime2, y = `O2 (%)`)) +
  geom_line() +
  geom_point() +
  labs(x  = "") +
  theme_bw()
ggpubr::ggarrange(p1, p2, p3)

# 22-12-2023
p1 <- ggplot(df %>% filter(date == "2023-12-22",
                           `conductivity (µS/cm)` > 10), aes(x = datetime2, y = `conductivity (µS/cm)`)) +
  geom_line() +
  geom_point() +
  labs(x = "") +
  theme_bw()
p2 <- ggplot(df %>% filter(date == "2023-12-22", `temp EC (C)` > 0 & `temp EC (C)` < 5000), 
             aes(x = datetime2, y = `temp EC (C)`)) +
  geom_line() +
  geom_point() +
  labs(x = "") +
  theme_bw()
p3 <- ggplot(df %>% filter(date == "2023-12-22", `O2 (%)` > 0 & `O2 (%)` < 5000), 
             aes(x = datetime2, y = `O2 (%)`)) +
  geom_line() +
  geom_point() +
  labs(x  = "") +
  theme_bw()
ggpubr::ggarrange(p1, p2, p3)

# EC timeseries
ggplot(df %>% filter(!is.na(date),
                     `conductivity (µS/cm)` > 10), aes(x = datetime2, y = `conductivity (µS/cm)`)) +
  geom_line() +
  geom_point() +
  labs(x = "") +
  theme_bw() +
  facet_wrap(facets = "date", scale = "free_x") # only x scale free

# Temp timeseries
ggplot(pons %>% filter(`temp EC (C)` > 0,
                       `temp EC (C)` < 5000), aes(x = datetime2, y = `temp EC (C)`)) +
  geom_line() +
  geom_point() +
  labs(x = "") +
  theme_bw()

# O2 timeseries
ggplot(pons %>% filter(`O2 (%)` > 0), aes(x = datetime2, y = `O2 (%)`)) +
  geom_line() +
  geom_point() +
  labs(x  = "") +
  theme_bw()

## maps

# Radon
ggplot() +
  geom_sf(data = pts, size = 0.05, colour = "grey") +
  geom_sf(data = pts %>% filter(!is.na(radon_water)), 
          aes(fill = radon_water), 
          colour = "black",
          pch = 21,
          size = 3) +
  scale_fill_viridis("Radon [dpm/L]") +
  theme_bw()

# EC
p1 <- ggplot() +
  geom_sf(data = pts %>% filter(!is.na(`conductivity (µS/cm)`),
                                       date == "2023-12-18"), 
          aes(fill = `conductivity (µS/cm)`, shape = date), 
          colour = "black",
          pch = 21,
          size = 2) +
  scale_fill_viridis() +
  theme_bw()
p2 <- ggplot() +
  geom_sf(data = pts %>% filter(!is.na(`conductivity (µS/cm)`),
                                date == "2023-12-19"), 
          aes(fill = `conductivity (µS/cm)`, shape = date), 
          colour = "black",
          pch = 21,
          size = 2) +
  scale_fill_viridis() +
  theme_bw()
p3 <- ggplot() +
  geom_sf(data = pts %>% filter(!is.na(`conductivity (µS/cm)`),
                                date == "2023-12-20"), 
          aes(fill = `conductivity (µS/cm)`, shape = date), 
          colour = "black",
          pch = 21,
          size = 2) +
  scale_fill_viridis() +
  theme_bw()

ggpubr::ggarrange(p1, p2, p3)

ggplot() +
  geom_sf(data = pts %>% filter(!is.na(`conductivity (µS/cm)`)), 
          aes(fill = `conductivity (µS/cm)`, shape = date), 
          colour = "black",
          pch = 21,
          size = 2) +
  scale_fill_viridis() +
  theme_bw() +
  facet_wrap(facets = "date")

# Temp
ggplot() +
  geom_sf(data = pts %>% filter(`temp EC (C)` > 10), aes(fill = `temp EC (C)`), 
          colour = "black",
          pch = 21,
          size = 2) +
  scale_fill_viridis() +
  theme_bw()

# Oxygen
ggplot() +
  geom_sf(data = pts %>% filter(`O2 (%)` > 0), aes(fill = `O2 (%)`), 
          colour = "black",
          pch = 21,
          size = 2) +
  scale_fill_viridis() +
  theme_bw() +
  facet_wrap(facets = "date")

# RBR
# convert to sf for spatial plotting
rbr <- rbind(rbr1, rbr2, rbr3, rbr4) %>%
  mutate(datetime2 = force_tz(parse_date_time(Time_AST, "%Y/%m/%d %H:%M:%S"), tz_out = "AST"))

# merge with coordinates Mike
rbr <- left_join(df %>% select(datetime2, latitude, longitude),
                 rbr)
write.csv(rbr %>% filter(!is.na(EC_sp_uS_cm)), paste0(input, "data_rbr_combined.csv"))

pts_rbr <- st_as_sf(rbr %>% filter(!is.na(latitude)), coords = c("longitude", "latitude"),
                crs = 4326, agr = "constant")

ggplot() +
  geom_sf(data = pts_rbr %>%
            filter(!is.na(EC_sp_uS_cm)), aes(fill = `EC_sp_uS_cm`), 
          #colour = "black",
          alpha = 0.4,
          stroke = NA,
          pch = 21,
          size = 2) +
  scale_fill_viridis() +
  theme_bw()

ggplot() +
  geom_sf(data = pts_rbr %>% filter(!is.na(EC_sp_uS_cm)), aes(fill = `EC_sp_uS_cm`), 
          #colour = "black",
          pch = 21,
          stroke = NA,
          size = 2) +
  scale_fill_viridis() +
  facet_wrap(facets = "date") +
  labs(title = "RBR data at 0.3m below surface") +
  theme_bw()

ggplot() +
  geom_sf(data = pts_rbr %>%
            filter(!is.na(EC_sp_uS_cm),
                   date == "2023-12-22"), aes(fill = `EC_sp_uS_cm`), 
          colour = "black",
          stroke = NA,
          pch = 21,
          size = 2) +
  scale_fill_viridis() +
  theme_bw()

# Radon map with custom class breaks
ggplot() +
  geom_sf(data = cur_geology, aes(fill = Field), alpha = 0.5) +
  scale_fill_manual(name = "Geology", 
                    values = c("yellowgreen", "purple4", "khaki2", "orange", "lightskyblue1")) +
  new_scale_fill() +
  geom_sf(data = pts %>% filter(parameter == "Rn",
                                sampletype == "groundwater",
                                !is.na(value)) %>%
            mutate(value = value / 1000),
          aes(fill = cut(value, breaks = c(0, 1, 2, 5, 10, 53))),
          colour = "black",
          pch = 21,
          size = 2) +
scale_fill_viridis(name = "Rn [Bq/l]",
                   direction = 1,
                   discrete = T,
                   na.value = "grey",
                   labels = c("0 - 1",
                              "1 - 2",
                              "2 - 5",
                              "5 - 10",
                              ">10")) +
  coord_sf(xlim = c(-69.15, -68.75),
           ylim = c(12.0, 12.4)) +
  #theme(legend.position = c(0.75, 0.8)) +
  ggtitle("Radon concentrations in groundwater on Curacao") + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = "dashed", 
                                        linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))


# put parameters in long format
d_long <- d %>%
  pivot_longer(., cols = `conductivity (µS/cm)`:`redox (mV)`,
               names_to = "param",
               values_to = "value") %>%
  # set param names as factors for plotting order
  mutate(param = factor(param, levels = c("conductivity (µS/cm)",
                                          "salinity (ppt)",
                                          "O2 (mg/L)",
                                          "O2 (%)",
                                          "pH",
                                          "redox (mV)",
                                          "temp O2 (C)")))

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################

## all radon measurements pumping tests vs sampling time ##
p1 <- ggplot(dat %>% filter(!pumping_test %in% c(1, 2, 3, 5)), aes(x = sample_time, y = radon, colour = ID)) +
  geom_errorbar(aes(ymin = radon - radon_sd, ymax = radon + radon_sd),
                width = 8, colour = "darkgrey") +
  geom_line(linetype = "dashed") +
  geom_point(size = 2) +
  labs(title = "",
       x = "sample time [min]",
       y = "radon [dpm/L]") +
  theme_bw() +
  theme(legend.position = "none") 

# radon vs volume abstracted
p2 <- ggplot(dat %>% filter(!pumping_test %in% c(1, 2, 3, 5)), aes(x = flow_cum, y = radon, colour = ID)) +
  geom_errorbar(aes(ymin = radon - radon_sd, ymax = radon + radon_sd),
                width = 0.5, colour = "darkgrey") +
  geom_line(linetype = "dashed") +
  geom_point() +
  #scale_y_continuous(trans='log10') +
  #scale_x_continuous(trans='log10') +
  labs(title = "",
       x = "Cumulative outflow [m3]",
       y = "radon [dpm/L]") +
  theme_bw() +
  theme(legend.position = "none") 

# radon vs well flushes
p3 <- ggplot(dat %>% filter(!pumping_test %in% c(1, 2, 3, 5)), aes(x = well_flush, y = radon, colour = ID)) +
  geom_vline(xintercept = 3, linetype = "dashed", colour = "black") +
  geom_errorbar(aes(ymin = radon - radon_sd, ymax = radon + radon_sd),
                width = 0.2, colour = "darkgrey") +
  geom_line(linetype = "dashed") +
  geom_point(size = 2) +
  #scale_y_continuous(trans='log10') +
  scale_x_continuous(trans='log10') +
  labs(title = "",
       x = "well flushes",
       y = "") +
  theme_bw() +
  theme(legend.position = "none") 

ggarrange(p1, p3, ncol=2, common.legend = TRUE, legend="bottom")


## graph Mark ##
# multimeter
ggplot(d_long %>% filter(location == "Julianadorp"), aes(x = datetime2, y = value, color = param)) +
  geom_point(size = 1) +
  geom_line() +
  scale_color_manual(values = c("firebrick3", "firebrick3", 
                                "steelblue", "steelblue", 
                                "forestgreen", "forestgreen", 
                                "peru")) +
  scale_y_continuous("") +
  scale_x_datetime("", date_labels = "%H:%M") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(facets = "param",
             scales = "free",
             ncol = 2) 
ggtitle(paste(unique(d1$description), unique(date(d1$datetime2))))

# EC
ggplot(d_long %>% filter(param == "conductivity (µS/cm)",
                         value > 20), 
       aes(x = datetime2, y = value)) +
  geom_point(size = 1, colour = "firebrick3") +
  geom_line(colour = "firebrick3") +
  # scale_color_manual(values = c("firebrick3", "firebrick3", 
  #                               "steelblue", "steelblue", 
  #                               "forestgreen", "forestgreen", 
  #                               "peru")) +
  scale_y_continuous("") +
  scale_x_datetime("", date_labels = "%H:%M") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(facets = "location",
             scales = "free",
             ncol = 2) 
ggtitle(paste(unique(d1$description), unique(date(d1$datetime2))))

# radon
p1 <- ggplot(d_long %>% filter(param == "conductivity (µS/cm)"), 
             aes(x = datetime2, y = value, color = param)) +
  geom_point(size = 1) +
  geom_line(colour = "firebrick3") +
  scale_y_continuous("EC [uS/cm]") +
  scale_x_datetime("", date_labels = "%H:%M") +
  theme_bw() +
  labs(title = "Pumping test Mark outflow") +
  theme(legend.position = "none") 

p2 <- ggplot(d_long %>% filter(param == "O2 (%)"), 
             aes(x = datetime2, y = value)) +
  geom_point(size = 1, colour = "steelblue") +
  geom_line(colour = "steelblue") +
  scale_y_continuous("O2 [%]") +
  scale_x_datetime("", date_labels = "%H:%M") +
  theme_bw() +
  theme(legend.position = "none") 

p3 <- ggplot(dat %>% filter(location == "Mark"), 
             aes(x = sample_time, y = radon)) +
  geom_errorbar(aes(ymin = radon - radon_sd, ymax = radon + radon_sd),
                width = 4, colour = "darkgrey") +
  geom_point(size = 2) + 
  labs(x = "sample time [min]",
       y = "radon [dpm/L]") +
  theme_bw()

cowplot::plot_grid(p1, p2, p3, ncol = 1)







# merge 2 input files and continue index column from first file
dat2 <- dat2 %>%
  mutate(X. = X. + dat$X.[nrow(dat)])
dat <- rbind(dat, dat2)

# rename columns
d <- dat %>%
  rename("record" = "X.",
         "datetime" = "Date",
         "conductivity (µS/cm)" = "Conductivité..µS.cm.",
         "salinity (ppt)" = "Salinité..ppt.",
         "O2 (%)" = "O2...Sat.",
         "O2 (mg/L)" = "O2..mg.l.",
         "temp O2 (C)" = "Température...C.",
         "pH" = "PH..pH.",
         "redox (mV)" = "Redox..mV.") %>%
  #"temp pH (C)" = "Température...C..1") %>%
  # set date
  mutate(datetime2 = parse_date_time(datetime, "%m/%d/%Y %I:%M:%S %p")) %>%
  mutate(date = as.POSIXct(datetime, format = "%m/%d/%Y")) %>%
  mutate(time = format(datetime2, "%H:%M:%S")) %>%
  # add serie numbers
  ## automatic based on date
  # mutate(series = 1) %>%
  # mutate(series = ifelse(record == 1, 1, 
  #                        ifelse(date > lag(date), series + 1, series)))
  ## based on manual breaks
  mutate(series = case_when(
    record <= 621 ~ 1,    # bay sailing 
    record <= 727 ~ 2,
    record <= 829 ~ 3,
    record <= 868 ~ 4,
    record <= 872 ~ 5,
    record <= 1111 ~ 6,   # well in Rooi Jan Noorduynweg
    record <= 1257 ~ 7, # salt dilution method rooi Jan Noorduynweg
    record <= 1376 ~ 8, 
    record <= 1507 ~ 9,
    ## other time format..
    record <= 1738 ~ 10, # 05-01-2023 well GW102
    record <= 1821 ~ 11, # 06-01-2023 Hato spring salt dilution 11:25
    record <= 1944 ~ 12, # 06-01-2023 well GW103 15:45
    record <= 2031 ~ 13, # 06-01-2023 well GW104 17:20
    record <= 2183 ~ 14, # 09-01-2023 well GW105 16:20
    record <= 2333 ~ 15, # 11-01-2023 09:00 GW106
    record <= 3071 ~ 16, # 11-01-2023 Piscadera Bay boat
    # this is CUR3 file
    record <= 3357 ~ 17, # 12-01-2023 well GW107 CW 17:00
    record <= 3492 ~ 18, # 13-01-2023 well.. 11:00
    record <= 3655 ~ 19, # 13-01-2023 well.. 11:30
    record <= 3724 ~ 20, # 13-01-2023 well.. 12:30
    record <= 3992 ~ 21, # 16-01-2023 well.. 15:00
    record <= 4053 ~ 22, # 16-01-2023 well.. 16:00
    TRUE ~ NA_real_)) %>%
  mutate(description = case_when(
    series == 1 ~ "1. Piscadera Bay boat",
    series == 2 ~ "2. GW094, Amarildo well 1",
    series == 3 ~ "3. GW095, Amarildo well 2",
    series == 4 ~ "4. SR028, sewage outfall in front of Klein Hofje",
    series == 6 ~ "5. GW097, well in rooi Jan Noorduynweg",
    series == 7 ~ "6. SR031, rooi Jan Noorduynweg salt dilution",
    series == 8 ~ "7. SR032, rooi in Bloemhof salt dilution 1",
    series == 9 ~ "8. rooi in Bloemhof salt dilution 2",
    series == 10 ~ "9. GW102, well Porto Marie",
    series == 11 ~ "10. SP004, Hato Spring salt dilution",
    series == 12 ~ "11 . GW103, ", 
    series == 13 ~ "12. GW104, ", 
    series == 14 ~ "13. GW105, open well Barber", 
    series == 15 ~ "14. GW106, well on parking lot",
    series == 16 ~ "15. Piscadera Bay boat II",
    series == 17 ~ "16. GW107, Chris Winkel",
    series == 18 ~ "17. GW108",
    series == 19 ~ "18. GW109" ,
    series == 20 ~ "19. GW",
    series == 21 ~ "20. GW111",
    series == 22 ~ "21. GW112"
  )) %>%
  # drop records with zero value
  filter(!record %in% c(620, 621, 728, 830, 831, 832, 
                        868:873, 1508:1510, 1537:1540, 
                        1584:1587, 1622:1626, 1822:1835,
                        1945:1948, 2032:2034, 2184:2189,
                        2211, 2962:3021, 3070:3071,
                        3358:3385, 3493:3501, 3725:3726,
                        3993:3996))

## graphs
# EC
ggplot(d, aes(x = datetime2, y = `conductivity (µS/cm)`)) +
  geom_point() +
  geom_line() +
  scale_x_datetime(date_labels = "%d-%m\n%H:%M:%S") +
  theme_bw() +
  facet_wrap(facets = "series", scales = "free")

# pH
ggplot(d, aes(x = datetime2, y = `pH`)) +
  geom_point() +
  geom_line() +
  scale_x_datetime(date_labels = "%d-%m\n%H:%M:%S") +
  theme_bw() +
  facet_wrap(facets = "description", scales = "free")

# O2
ggplot(d, aes(x = datetime2, y = `O2 (mg/L)`)) +
  geom_point() +
  geom_line() +
  scale_x_datetime(date_labels = "%d-%m\n%H:%M:%S") +
  theme_bw() +
  facet_wrap(facets = "description", scales = "free")

# EC + O2
for(i in unique(d$series)) {
  
  print(i)
  d1 <- d %>% filter(series == i)
  
  ## set limits for y-axes
  ylim.prim <- c(min(d1$`conductivity (µS/cm)`), 
                 max(d1$`conductivity (µS/cm)`))   
  ylim.sec <- c(min(d1$`O2 (mg/L)`), 
                max(d1$`O2 (mg/L)`))    
  
  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- ylim.prim[1] - b*ylim.sec[1] 
  
  plt <- ggplot(d1, aes(datetime2, `conductivity (µS/cm)`)) +
    #geom_point() +
    geom_line(color = "firebrick3") +
    geom_line(aes(y = a + `O2 (mg/L)` * b), color = "steelblue") +
    scale_y_continuous("EC [uS/cm]", 
                       sec.axis = sec_axis(~ (. - a)/b, 
                                           name = "O2 [mg/L]")) +
    scale_x_datetime("", date_labels = "%d-%m\n%H:%M:%S") +
    theme_bw() +
    theme(axis.line.y.left = element_line(color = "firebrick3"),
          axis.ticks.y.left = element_line(color = "firebrick3"), 
          axis.text.y.left = element_text(color = "firebrick3"),
          axis.title.y.left = element_text(color = "firebrick3"), 
          axis.line.y.right = element_line(color = "steelblue"), 
          axis.ticks.y.right = element_line(color = "steelblue"),
          axis.text.y.right = element_text(color = "steelblue"), 
          axis.title.y.right = element_text(color = "steelblue")
    ) +
    ggtitle(unique(d1$description))
  ggsave(plot = plt, paste0(output, "Clean_data/salt_dilution/figures/",
                            "multimeter_EC_O2_", unique(d1$description), ".png"),
         device = "png",
         width = 5,
         height = 5)
  plot(plt)
  
}

## Plots below each other from sampling
# put parameters in long format
d_long <- d %>%
  pivot_longer(., cols = `conductivity (µS/cm)`:`redox (mV)`,
               names_to = "param",
               values_to = "value") %>%
  # set param names as factors for plotting order
  mutate(param = factor(param, levels = c("conductivity (µS/cm)",
                                          "salinity (ppt)",
                                          "O2 (mg/L)",
                                          "O2 (%)",
                                          "pH",
                                          "redox (mV)",
                                          "temp O2 (C)")))

# for each series, plot all parameters
for(i in unique(d_long$series)) {
  
  print(i)
  d1 <- d_long %>% filter(series == i)
  
  plt <- ggplot(d1, aes(x = datetime2, y = value, color = param)) +
    geom_point(size = 1) +
    geom_line() +
    scale_color_manual(values = c("firebrick3", "firebrick3", 
                                  "steelblue", "steelblue", 
                                  "forestgreen", "forestgreen", 
                                  "peru")) +
    scale_y_continuous("") +
    scale_x_datetime("", date_labels = "%H:%M") +
    theme_bw() +
    theme(legend.position = "none") +
    facet_wrap(facets = "param",
               scales = "free",
               ncol = 2) +
    ggtitle(paste(unique(d1$description), unique(date(d1$datetime2))))
  
  ggsave(plot = plt, paste0(output, "Clean_data/sampling_ponsel/figures/",
                            "multimeter", unique(d1$description), ".png"),
         device = "png",
         width = 5,
         height = 5)
  plot(plt)
  
}

# Select columns and combine datasets
d <- rbind(DOC1, DOC2) %>%
  rename(samplecode = Sample.ID)




# Calculate average value and SD
d <- d %>% 
  group_by(samplecode) %>%
  mutate(avg = mean(Conc.),
         sd = sd(Conc.)) %>%
  ungroup() %>%
  # only unique rows
  filter(!is.na(AVG)) %>%
  # add sampletype
  mutate(sampletype = substr(samplecode, start = 1, stop = 2))

# plot gw
ggplot(d %>% filter(sampletype == "GW"), 
       aes(x = samplecode, y = avg)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd),
                width = 0.8) +
  theme_bw()

# plot all
ggplot(d, aes(x = samplecode, y = avg)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd),
                width = 0.2) +
  theme_bw() +
  facet_wrap(facets = 'sampletype', scales = "free")

# put into format of other datasets


# Clean up the alkalinity data
d <- alk %>%
  # titrations are in duplicates, select the ones with the highest accuracy
  filter(Select == 1) %>%
  # add parameter column with HCO3
  mutate(parameter = "HCO3",
         `C.[meq/l]` = as.numeric(`C.[meq/l]`),
         `C.[mg/l]` = as.numeric(`C.[mg/l]`)) %>%
  # rename columns
  rename(samplecode = Sample.code,
         "mg/l" = `C.[mg/l]`,
         "meq/l" = `C.[meq/l]`,
         notes = Notes) %>%
  # place different units in long format
  pivot_longer(., cols = c(`mg/l`, `meq/l`, mmol),
               values_to = "value",
               names_to = "units") %>%
  # add limit symbol, detection limit column and method
  mutate(limit_symbol = "",
         detection_limit = NA,
         method = "Field titration") %>%
  # select only relevant columns 
  select(samplecode, parameter, value, limit_symbol, detection_limit, units, method, notes) 

# Check if every sample has only 1 value
check <- d %>%
  filter(units == "mg/l") %>%
  group_by(samplecode) %>%
  summarise(measurements = n_distinct(value)) %>%
  filter(measurements > 1)
if(nrow(check) > 0) {
  stop("More than 1 value for alkalinity in a sample")
}

# Check if every sample has a value for alkalinity 
#...


# 


###############################################################################
# save data
###############################################################################

# export salt dilution measurements
write.csv(d %>% filter(series == 6) %>% 
            select(record, datetime, date, time, 'conductivity (µS/cm)'),
          paste0(output, "Clean_data/salt_dilution/",
                 "salt_dilution_rooi_Jan_Noorduynweg.csv"))
write.csv(d %>% filter(series == 7) %>% 
            select(record, datetime, date, time, 'conductivity (µS/cm)'),
          paste0(output, "Clean_data/salt_dilution/",
                 "salt_dilution_rooi_Bloemhof_1.csv"))
write.csv(d %>% filter(series == 8) %>% 
            select(record, datetime, date, time, 'conductivity (µS/cm)'),
          paste0(output, "Clean_data/salt_dilution/",
                 "salt_dilution_rooi_Bloemhof_2.csv"))



