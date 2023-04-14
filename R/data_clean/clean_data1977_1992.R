#
# Input: historic hydrochemical datasets of 1977 and 1992
#         
# Output: combined cleaned dataset
# 
# Dependencies: none
#
#
# Auteur: Mike Wit
# Datum: 17-09-2021
# Edit: XX-XX-XXXX
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
#pacman::p_load(tidyverse, dplyr, ggplot2, openxlsx, readr, ggmap, 
#               sf, tmap, tmaptools, leaflet)
pacman::p_load(tidyverse, openxlsx, ggmap, 
               sf, leaflet, data.table, cowplot, data.table)


###############################################################################
# load data
###############################################################################

input <- "C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/" 

# hydrochemical data 1977 and 1992
data1977 <- read.xlsx(paste0(input, "Hydrochemie/Well data Curacao 1977 & 1992.xlsx"),
                      sheet = "1977", startRow = 5)
data1992 <- read.xlsx(paste0(input, "Hydrochemie/Well data Curacao 1977 & 1992.xlsx"),
                      sheet = "1992", startRow = 4)

# all well locations data, use with caution
well_loc <- read.xlsx(paste0(input, "Put data Curacao/",
                             "Put locations non-checked version/",
                             "Putten_coordinates_usewithcaution.xlsx"),
                      detectDates = T)

output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################

## Wide to long for 1977
# add year column 
d1 <- data1977 %>%
  mutate(year = 1977) %>%
  mutate(K = as.numeric(ifelse(K == "<1", 1, K))) %>%
  pivot_longer(cols = TDS:RSC,
               names_to = "parameter",
               values_to = "value") %>%
  # add column with detection limits (dl). Values of 1 for K were <1
  mutate(dl = ifelse(parameter == "K" & value == 1,
                     "<", ""),
         # change EC from mS/cm to uS/cm
         value = ifelse(parameter == "EC",
                        value * 1000, value)) %>%
  # add column with units
  mutate(units = ifelse(parameter == "EC", "uS/cm",
                        ifelse(parameter == "pH", "",
                               "mg/l")),
         # remove whitespaces from putcode
         putcode = str_trim(putcode), 
         # add empty date column
         date = "") %>%
  # reoder columns
  select(putcode, xcoord, ycoord, geologie, putsoort,
         year, date, sample, parameter, dl, value, units)

## wide to long for 1992
d2 <- data1992 %>%
  mutate(year = 1992) %>%
  # Cd and NH4 column are characters
  mutate(Cd = as.numeric(Cd),
         NH4 = as.numeric(ifelse(NH4 == ">>400", 400, 
                                 ifelse(NH4 == "-999", NA, NH4)))) %>%
  pivot_longer(cols = Ca:clustermember,
               names_to = "parameter",
               values_to = "value") %>%
  # add column with detection limits (dl). Values of 400 for NH4 were >>400
  mutate(dl = ifelse(parameter == "NH4" & value == 400,
                     ">>", ""),
         # change EC from mS/cm to uS/cm
         value = ifelse(parameter == "EC",
                        value * 1000, value)) %>%
  # add column with units
  mutate(units = ifelse(parameter == "EC", "uS/cm",
                        ifelse(parameter == "pH", "",
                               ifelse(parameter == "Diepte", "m",
                                      ifelse(parameter == "Temp", "Degrees Celcius",
                                             "mg/l")))),
         # remove whitespaces from putcode
         putcode = gsub(" ", "", putcode)) %>%
  # remove "i" from putcodes starting with "i"
  mutate(putcode = gsub("i", "", putcode)) %>%
  rename(date = sampling.date) %>%
  # reoder columns
  select(putcode, xcoord, ycoord, geologie, putsoort,
         year, date, sample, parameter, dl, value, units)

# merge two datasets 
d <- rbind(d1, d2)

# add Decimal degrees coordinates from well data
well_loc <- well_loc %>%
  mutate(Wellno2 = tolower(Wellno2))
d$lat <- well_loc[match(d$putcode, well_loc$Wellno2), 16]
d$lon <- well_loc[match(d$putcode, well_loc$Wellno2), 15]
d$missingcoord <- ifelse(is.na(d$lat), T, F)

write.xlsx(d, paste0(output, "Clean_data/hydrochemistry1977-19922.xlsx"))


## Check hydrochemical statistics

# differences between years in tabel
# EC

d %>%
  filter(parameter == "EC",
         !is.na(value)) %>%
  group_by(year) %>%
  summarise(nr.measurements = n(),
            p10 = quantile(value, 0.10),
            p25 = quantile(value, 0.25),
            med = median(value),
            avg = mean(value) %>% round(),
            p75 = quantile(value, 0.75),
            p95 = quantile(value, 0.95)
  ) %>% view()

# now in boxplot
dat <- d %>%
  filter(parameter == "EC",
         !is.na(value))

n_fun <- function(x) {
  return(c(y = -2000,
           label = length(x)))
}

ggplot(data = dat, 
       aes(x = factor(year),
           y = value)) +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  geom_boxplot(fill = "lightgrey", width = 0.6, outlier.shape = NA) +
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "EC [uS/cm]") +
  coord_cartesian(ylim = c(0, quantile(dat$value, 0.9))) +
  theme_bw()

# Now for all parameters
# all in 1 image
ggplot(data = d %>% filter(!parameter %in% c('c1', 'c2', 'c3', 'c4', 
                                             'clustermember', 'Diepte',
                                             'RSC', 'SAR')), 
       aes(x = factor(year),
           y = value)) +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  geom_boxplot(fill = "lightgrey", width = 0.6, outlier.shape = NA) +
  #stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") + 
  #scale_y_continuous(name = "EC [uS/cm]") +
  theme_bw() +
  facet_wrap(~parameter, scales = "free")

# write and save seperate images
dat <- d %>%
  filter(!parameter %in% c('c1', 'c2', 'c3', 'c4', 
                           'clustermember', 'Diepte',
                           'RSC', 'SAR'))

for(i in unique(dat$parameter)) {
  j <- dat %>%
    filter(parameter == i)
  
  ggplot(data = j, 
         aes(x = factor(year),
             y = value)) +
    stat_boxplot(geom = 'errorbar', width = 0.4) +
    geom_boxplot(fill = "lightgrey", width = 0.6, outlier.shape = NA) +
    #stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = paste0(j$parameter[1], " [", j$units[1], "]")) +
    coord_cartesian(ylim = quantile(j$value, c(0.01, 0.99), na.rm = T)) +
    theme_bw()
  ggsave(paste0(output, "Output/Figures/Historical_data/boxplot_", j$parameter[1], ".png"))
  
  
}


# differences between geologies
# data tabel
dat <- d %>%
  filter(!parameter %in% c('c1', 'c2', 'c3', 'c4', 
                           'clustermember', 'Diepte',
                           'RSC', 'SAR'),
         !is.na(value)) %>%
  group_by(geologie, parameter, year) %>%
  summarise(nr.measurements = n(),
            p10 = quantile(value, 0.10),
            p25 = quantile(value, 0.25),
            med = median(value),
            avg = mean(value) %>% round(),
            p75 = quantile(value, 0.75),
            p95 = quantile(value, 0.95)
  ) %>% view()

# boxplots
# all in 1 image
dat <- d %>%
  filter(!parameter %in% c('c1', 'c2', 'c3', 'c4', 
                           'clustermember', 'Diepte',
                           'RSC', 'SAR'),
         !is.na(value))

ggplot(data = dat, 
       aes(x = factor(year),
           y = value,
           fill = geologie)) +
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot(outlier.shape = NA) +
  #stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") + 
  #coord_cartesian(ylim = quantile(dat$value, c(0.02, 0.99))) +
  #scale_y_continuous(name = "EC [uS/cm]") +
  theme_bw() +
  facet_wrap(~parameter, scales = "free")

dat <- d %>%
  filter(parameter == "EC",
         !is.na(value))

ggplot(data = dat, 
       aes(x = factor(year),
           y = value,
           fill = geologie)) +
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot(outlier.shape = NA) +
  #stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") + 
  coord_cartesian(ylim = quantile(dat$value, c(0.02, 0.99))) +
  #scale_y_continuous(name = "EC [uS/cm]") +
  theme_bw() 
#facet_wrap(~parameter, scales = "free")


# differences in land use





## Check coordinates and well locations 1977 - 1992
dat <- d %>%
  select(putcode, year, xcoord, ycoord, lat, lon, missingcoord) %>%
  unique()

write.xlsx(dat, paste0(output, "putten_hydrochemie.xlsx"))

dat %>%
  group_by(putcode) %>%
  mutate(jaren.putcode = length(year)) %>%
  ungroup() %>%
  mutate(coord = paste0(xcoord, ycoord)) %>%
  group_by(coord) %>%
  mutate(jaren.coord = length(year)) %>%
  ungroup() %>%
  #filter(missingcoord == T) %>%
  group_by(year) %>%
  summarise(n.putten = n_distinct(putcode),
            n.overlap.putcode = n_distinct(putcode[jaren.putcode == 2]),
            n.overlap.coord = n_distinct(putcode[jaren.coord == 2]),
            n.missingDecimalcoord = n_distinct(putcode[missingcoord == T])) %>%
  view()

dat %>%
  mutate(coord = paste0(xcoord, ycoord)) %>%
  group_by(coord) %>%
  summarise(aantal.putten = n_distinct(putcode),
            putcodes = paste(unique(putcode), collapse = ", ")) %>%
  view()

## Checks ##
# 233 wells in 1977
# 97 wells in 1992
# only 62 wells overlapping by putcode between 1977 and 1992..
d1 %>% filter(putcode %in% d2$putcode) %>% select(putcode) %>% n_distinct()
d2 %>% filter(putcode %in% d1$putcode) %>% select(putcode) %>% n_distinct()

# check using coordinates if more wells overlap  
d1 <- d1 %>%
  mutate(coord = paste0(xcoord, ycoord))
d2 <- d2 %>%
  mutate(coord = paste0(xcoord, ycoord))

# 51 unique xy-coordinates that match between the 2 years
d1 %>% filter(coord %in% d2$coord) %>% select(coord) %>% n_distinct()
d2 %>% filter(coord %in% d1$coord) %>% select(coord) %>% n_distinct()

d1 %>% 
  filter(coord %in% d2$coord) %>% 
  select(putcode, coord) %>% 
  distinct() %>%
  view()

d1_coord <- d1 %>%
  select(putcode, year, xcoord, ycoord)
d2_coord <- d2 %>%
  select(putcode, year, xcoord, ycoord)

test <- full_join(d1_coord, d2_coord, by = "putcode") %>% 
  unique() %>%
  view()


# folder <- paste0("C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/Put data Curacao/", 
#                  "File with individual put data non cleaned versions/")
# filenames <- list.files(path = folder)
# test <- filenames %>%
#   gsub(".XLS", "", .) %>%
#   tolower() %>%
#   head(., -1) %>%
#   data.frame() %>%
#   rename(putcode = ".")
# test$lon <- well_loc[match(test$putcode, well_loc$Wellno2), 15]
# test$lat <- well_loc[match(test$putcode, well_loc$Wellno2), 16]
# test$x1977 <- as.data.frame(d1)[match(test$putcode, d1$putcode), 2]
# test$y1977 <- as.data.frame(d1)[match(test$putcode, d1$putcode), 3]
# test$x1992 <- as.data.frame(d2)[match(test$putcode, d2$putcode), 2]
# test$y1992 <- as.data.frame(d2)[match(test$putcode, d2$putcode), 3]
# test$nocoord <- 
#   test$diff_77_92 <- 
#   
#   
#   
#   
  