# load libraries
library(rvest)
library(lubridate)
library(tidyverse)
library(glue)
library(janitor)
library(measurements)

# import data
pdx_daily <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/USW00024229.csv.gz"

pdx_weather <- read_csv(pdx_daily,
         col_names = c("station", "date", "variable", "value", "a", "b", "c", "d")) %>% # add column headers
  select(date, variable, value) %>% # remove unwanted variables
  pivot_wider(names_from = "variable", values_from = "value") %>% 
  select(date, TMAX, PRCP, SNOW) %>%  # variable as columns
  mutate(date = ymd(date),
         TMAX = TMAX/10*(9/5)+32, #convert from tenths of a degree C to degrees F
         PRCP = PRCP/10/25.4, #convert from tenths of mm to inches
         SNOW = SNOW/25.4) %>% #convert from mm to inches
  clean_names() %>% 
  drop_na(tmax) # drop NA values where temperature data is not recorded
tail(pdx_weather)
# check for outliers on all variables
# pdx_weather %>% 
#   ggplot(aes(x=date, y=tmax)) +
#   geom_line() 
# 
# pdx_weather %>% 
#   ggplot(aes(x=tmax)) +
#   geom_histogram(binwidth = 3) # no noticeable outliers
# 
# pdx_weather %>% 
#   ggplot(aes(x=date, y=prcp)) +
#   geom_line()
# 
# pdx_weather %>% 
#   ggplot(aes(x=prcp)) +
#   geom_histogram() +
#   scale_y_continuous(limits = c(0, 2500)) # no noticeable outliers
# 
# pdx_weather %>% 
#   ggplot(aes(x=date, y=snow)) +
#   geom_line()
# 
# pdx_weather %>% 
#   slice_max(n=10, snow) # max snowfall for one day was 14.4 inches on January 21, 1943. 
# 
# pdx_weather %>% 
#   ggplot(aes(x=snow)) +
#   geom_histogram() + 
#   scale_y_continuous(limits = c(0, 100)) # no noticeable outliers

# 
