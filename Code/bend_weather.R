# load libraries
library(rvest)
library(lubridate)
library(tidyverse)
library(glue)
library(janitor)
library(measurements)

bend_daily <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/USC00350694.csv.gz"

bend_weather <- read_csv(bend_daily,
                        col_names = c("station", "date", "variable", "value", "a", "b", "c", "d")) %>% # add column headers
  select(date, variable, value) %>% # remove unwanted variables
  pivot_wider(names_from = "variable", values_from = "value") %>% 
  select(date, TMAX, TMIN, PRCP, SNOW) %>%  # variable as columns
  mutate(date = ymd(date),
         TMAX = TMAX/10*(9/5)+32,
         TMIN = TMIN/10*(9/5)+32,#convert from tenths of a degree C to degrees F
         PRCP = PRCP/10/25.4, #convert from tenths of mm to inches
         SNOW = SNOW/25.4) %>% #convert from mm to inches
  clean_names() %>%
  mutate(tmax = if_else(tmax > -25, tmax, NA_real_)) %>% #remove outlier for tmax
  filter(year(date) >= 1912) %>% #start at 1912 due to lots of missing data between 1908-1911
  drop_na(tmax) # drop NA values where temperature data is not recorded

bend_weather %>%
  ggplot(aes(x=date, y=tmax)) +
  geom_line() # remove outlier in code above

bend_weather %>% 
  ggplot(aes(x=date, y=tmin)) +
  geom_line() #lots of missing data between 1908 & 1911. Removed in code above

bend_weather %>%
  ggplot(aes(x=date, y=prcp)) +
  geom_line()

bend_weather %>%
  ggplot(aes(x=prcp)) +
  geom_histogram() +
  scale_y_continuous(limits = c(0, 2500)) # no noticeable outliers

bend_weather %>%
  ggplot(aes(x=date, y=snow)) +
  geom_line()

bend_weather %>%
  slice_max(n=10, snow) # max snowfall for one day was 24.1 inches on November 6, 1973.
