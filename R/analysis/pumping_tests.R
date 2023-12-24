#
# Input: Ponsel Multimeter raw dataset from ODEON VIEWER + pumping test data
#         
# Output: Cleaned Pumping data graphs
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 03-11-2022
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, lubridate, ggpubr)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/" 

# read data pumping tests
dat <- read.xlsx(paste0(input, "Pumping_tests/Pumping_tests.xlsx")) %>%
  mutate(datum = as.Date(datum, origin = "1899-12-30")) %>%
  mutate(ID = paste0(pumping_test, ". ", location, " ", geology)) 
  
# read ponsol multimeter data
pons_wacawa <- read.csv(paste0(input, "Ponsel_multimeter/", "Pumping_Test_Wacawa_27-11-2023.csv"),
                        skip = 3) %>%
  mutate(location = "Wacawa") %>%
  # drop records with zero value
  filter(!X. %in% c(1:9)) 
pons_bracelli <- read.csv(paste0(input, "Ponsel_multimeter/", "Pumping_Test_Bracelli_30-11-2023.csv"),
                          skip = 3) %>%
  mutate(location = "Bracelli", PH..pH. = NA, Redox..mV. = NA) %>%
  # drop records with zero value
  filter(!X. %in% c(1:437, 2450:2463))
pons_mark <- read.csv(paste0(input, "Ponsel_multimeter/", "Pumping_Test_Mark_01-12-2023.csv"),
                      skip = 3) %>%
  mutate(location = "Julianadorp") %>%
  # drop records with zero value
  filter(!X. %in% c(1:8, 2386:2404, 1537, 1716, 1842))

pons_mango <- read.csv(paste0(input, "Ponsel_multimeter/", "Pumping_Test_Hofi_Mango_05-12-2023.csv"),
                       skip = 3) %>%
  mutate(location = "Hofi Mango") %>%
  # drop records with zero value
  filter(!X. %in% c(1:7))

pons_henry <- read.csv(paste0(input, "Ponsel_multimeter/", "Pumping_Test_Henry_06-12-2023.csv"),
                       skip = 3) %>%
  mutate(location = "Willibrordus") %>%
  # drop records with zero value
  filter(!X. %in% c(1:22))

ponsel <- rbind(pons_wacawa, pons_bracelli, pons_mark, pons_mango, pons_henry)


# rename columns
d <- ponsel %>%
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
  mutate(time = format(datetime2, "%H:%M:%S")) 
  # drop records with zero value
  #filter(!record %in% c(1:8, 2386:2404)) # only for Mark

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



