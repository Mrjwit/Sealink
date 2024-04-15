#
# Input: field measurements of E.coli Petrifilms
#         
# Output: Cleaned E.coli file
# 
# Dependencies: none
#
#
# Author: Mike Wit
# Date: 12-01-2022
# Edit: 13-03-2023
# 
# 
###############################################################################

###############################################################################
# initialisation
###############################################################################

# Loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx)

###############################################################################
# load data
###############################################################################

# set data file location
input <- "C:/Users/mikewit/Documents/SEALINK/Data/Raw_data/" 

# E.coli data file fieldwork 2021-2022
ecoli <- read.xlsx(paste0(input, "E.coli/E.coli.xlsx"),
                   sheet = "Fieldwork1", startRow = 2)
# E.coli data file fieldwork 2022-2023
ecoli2 <- read.xlsx(paste0(input, "E.coli/E.coli.xlsx"),
                    sheet = "Fieldwork2", startRow = 2)
# E.coli data file fieldwork 2023-2024
ecoli3 <- read.xlsx(paste0(input, "E.coli/E.coli.xlsx"),
                    sheet = "Fieldwork3", startRow = 2)

# output file location
output <- "C:/Users/mikewit/Documents/SEALINK/Data/" 

###############################################################################
# edit data
###############################################################################

#### first fieldwork ####
# Clean up the E.coli data
d <- ecoli %>%
  # some plates were incubated with dilutions, select the ones with the highest accuracy
  filter(Select == 1) %>%
  # add parameter column with E.coli
  mutate(parameter = "E.coli",
         units = "CFU/100 ml") %>%
  # rename columns
  dplyr::rename(value = E..coli) %>%
  # select only relevant columns
  select(samplecode, parameter, value, units, notes)

# # check duplo values
duplos <- d %>%
  filter(str_detect(notes, "duplo")) %>%
  view()

d_set <- d %>%
  # select samples with duplo measurements
  filter(samplecode %in% duplos$samplecode) %>%
  # add column with duplos
  mutate(duplo = ifelse(!str_detect(notes, "duplo"),
                        "origineel", "duplo")) %>%
  mutate(duplo = ifelse(is.na(notes), "origineel", duplo)) %>%
  select(-notes) %>%
  pivot_wider(values_from = value,
              names_from = duplo) %>%
  # add average value from duplo's
  mutate(value = (origineel + duplo) / 2,
         notes = "value averaged from duplos")

ggplot(d_set, aes(x = origineel, y = duplo)) +
  geom_point() +
  scale_x_log10() + 
  scale_y_log10() +
  geom_abline(intercept = 0) + 
  theme_bw()

# add average duplo values to final file
d <- d %>%
  filter(!samplecode %in% duplos$samplecode) %>%
  rbind(., d_set %>% select(samplecode, parameter, value, units, notes)) %>%
  # add columns that are present in other dataset to merge later
  mutate(limit_symbol = "",
         detection_limit = NA,
         sd = NA,
         method = "Petrifilm plate") %>%
  select(samplecode, parameter, value, sd, limit_symbol, detection_limit, units, method, notes) %>%
  arrange(samplecode)
  
#### second fieldwork ####
d2 <- ecoli2 %>%
  # some plates were incubated with dilutions, select the ones with the highest accuracy
  filter(Select == 1) %>%
  # add parameter column with E.coli
  mutate(parameter = "E.coli",
         units = "CFU/100 ml") %>%
  # rename columns
  dplyr::rename(conc = E..coli) %>%
  # select only relevant columns
  select(samplecode, parameter, conc, units, notes) 

# plot duplo values against each other
ggplot(d2 %>% select(samplecode, conc) %>%
         group_by(samplecode) %>%
         mutate(duplo = paste("duplo", row_number())) %>%
         pivot_wider(names_from = duplo,
                     values_from = conc),
       aes(`duplo 1`, `duplo 2`)) +
  geom_abline(slope = 1) +
  geom_point() +
  coord_cartesian(xlim = c(0, 3e5), ylim = c(0, 3e5)) +
  theme_bw()

# Average the duplo/triplo samples
d2 <- d2 %>%
  group_by(samplecode) %>%
  mutate(value = mean(conc, na.rm = T),
         limit_symbol = "",
         detection_limit = NA,
         sd = NA,
         method = "Petrifilm plate") %>%
  ungroup() %>%
  select(samplecode, parameter, value, sd, limit_symbol, detection_limit, units, method, notes) %>%
  mutate(notes = case_when(
    samplecode == "GW098" ~ "large air bubble, 1 Petrifilm not fully incubated",
    samplecode == "SR028" ~ "1 Petrifilm diluted 1:10",
    samplecode == "WW005" ~ "1 petrifilm undiluted, another diluted 1:10",
    samplecode %in% c("WW004", "WW006", "WW007") ~ "1 petrifilm diluted 1:10 and another 1:100",
    TRUE ~ notes)) %>%
  distinct() %>%
  arrange(samplecode)

#### third fieldwork ####
d3 <- ecoli3 %>%
  # some plates were incubated with dilutions, select the ones with the highest accuracy
  filter(Select == 1) %>%
  # add parameter column with E.coli
  mutate(parameter = "E.coli",
         units = "CFU/100 ml") %>%
  # rename columns
  dplyr::rename(conc = E..coli) %>%
  # select only relevant columns
  select(samplecode, parameter, conc, units, notes) 

# plot duplo values against each other
ggplot(d3 %>% select(samplecode, conc) %>%
         group_by(samplecode) %>%
         mutate(duplo = paste("duplo", row_number())) %>%
         pivot_wider(names_from = duplo,
                     values_from = conc),
       aes(`duplo 1`, `duplo 2`)) +
  geom_abline(slope = 1) +
  geom_point() +
  coord_cartesian(xlim = c(0, 3e5), ylim = c(0, 3e5)) +
  theme_bw()

# Average the duplo/triplo samples
d3 <- d3 %>%
  group_by(samplecode) %>%
  mutate(value = mean(conc, na.rm = T),
         limit_symbol = "",
         detection_limit = NA,
         sd = NA,
         method = "Petrifilm plate") %>%
  ungroup() %>%
  select(samplecode, parameter, value, sd, limit_symbol, detection_limit, units, method, notes) %>%
  mutate(notes = case_when(
    samplecode == "GW098" ~ "large air bubble, 1 Petrifilm not fully incubated",
    samplecode == "SR028" ~ "1 Petrifilm diluted 1:10",
    samplecode == "WW005" ~ "1 petrifilm undiluted, another diluted 1:10",
    samplecode %in% c("WW004", "WW006", "WW007") ~ "1 petrifilm diluted 1:10 and another 1:100",
    TRUE ~ notes)) %>%
  distinct() %>%
  arrange(samplecode)

## merge datasets together ##
dat <- rbind(d, d2, d3) %>%
  distinct()

# Check if every sample has only 1 value
check <- dat %>%
  group_by(samplecode) %>%
  dplyr::summarise(measurements = length(value)) %>%
  filter(measurements > 1)
if(nrow(check) > 0) {
  stop("More than 1 value for E.coli in a sample")
}

# Check if every sample has a value for E.coli 



###############################################################################
# save data
###############################################################################

write.xlsx(dat, paste0(output, "Clean_data/ecoli_clean.xlsx"))


