## EDA
library(dplyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(openxlsx)

curDir <- dirname(rstudioapi::getSourceEditorContext()$path)

mobility <- read.csv(paste0(curDir, '/../data/raw/2020_US_Region_Mobility_Report.csv'),
                     stringsAsFactors = F)
stateInfo <- read.xlsx(paste0(curDir, '/../data/raw/COVID-19 US state policy database 3_17_2021.xlsx'),
                      sheet = 1, detectDates = T) %>%
  filter(!(STATE %in% c('State', 'category', 'type', 'unit')))

stateLevel <- mobility %>% filter(sub_region_1 != '', 
                                  sub_region_2 == '') %>%
  mutate(Date = ymd(date))

## Distribution by mobility type
stateLevel %>%
  filter(sub_region_1 %in% c('Alabama', 'California', 'Oregon', 'Washington', 'New York', 'Texas', 'Illinois', 'Florida')) %>%
  ggplot( aes(x = Date, y = retail_and_recreation_percent_change_from_baseline, group = sub_region_1, color = sub_region_1)) +
  geom_line()

stateLevel %>%
  filter(sub_region_1 %in% c('Alabama', 'California', 'Oregon', 'Washington', 'New York', 'Texas', 'Illinois', 'Florida')) %>%
  ggplot( aes(x = Date, y = grocery_and_pharmacy_percent_change_from_baseline, group = sub_region_1, color = sub_region_1)) +
  geom_line()

stateLevel %>%
  filter(sub_region_1 %in% c('Alabama', 'California', 'Oregon', 'Washington', 'New York', 'Texas', 'Illinois', 'Florida')) %>%
  ggplot( aes(x = Date, y = transit_stations_percent_change_from_baseline, group = sub_region_1, color = sub_region_1)) +
  geom_line()

stateLevel %>%
  filter(sub_region_1 %in% c('Alabama', 'California', 'Oregon', 'Washington', 'New York', 'Texas', 'Illinois', 'Florida')) %>%
  ggplot( aes(x = Date, y = parks_percent_change_from_baseline, group = sub_region_1, color = sub_region_1)) +
  geom_line()

stateLevel %>%
  filter(sub_region_1 %in% c('Alabama', 'California', 'Oregon', 'Washington', 'New York', 'Texas', 'Illinois', 'Florida')) %>%
  ggplot( aes(x = Date, y = workplaces_percent_change_from_baseline, group = sub_region_1, color = sub_region_1)) +
  geom_line()

stateLevel %>%
  filter(sub_region_1 %in% c('Alabama', 'California', 'Oregon', 'Washington', 'New York', 'Texas', 'Illinois', 'Florida')) %>%
  ggplot( aes(x = Date, y = residential_percent_change_from_baseline, group = sub_region_1, color = sub_region_1)) +
  geom_line()

## Dispersion by mobility type from 4/1-12/31
dispersion <- stateLevel %>% 
  filter(Date >= '2020-04-01',
         Date <= '2020-12-31') %>%
  group_by(sub_region_1) %>%
  summarise_at(vars(retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline),
               list(mean, median, sd), na.rm = T)


## Quick model test
modDat <- stateLevel %>% 
  filter(Date >= '2020-04-01',
         Date <= '2020-12-31') %>%
  group_by(sub_region_1) %>%
  summarise(y_var = mean(retail_and_recreation_percent_change_from_baseline, na.rm = T)) %>%
  left_join(stateInfo, by = c('sub_region_1' = 'STATE')) %>%
  mutate(stay = case_when(STAYHOME > 0 ~ '1',
                          STAYHOMENOGP > 0 ~ '0.5', 
                          TRUE ~ '0'),
         length = case_when(STAYHOME > 0 ~ ymd(END_STHM) - ymd(STAYHOME),
                            STAYHOMENOGP > 0 ~ ymd(END_STHM) - ymd(STAYHOMENOGP),
                            TRUE ~ 0))

mod <- lm(y_var ~ stay:length # + as.numeric(POPDEN18) + as.numeric(MINWAGE2019)
          , data = modDat)
summary(mod)
# car::vif(mod)
