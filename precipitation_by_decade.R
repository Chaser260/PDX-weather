source("pdx_weather.R")
library(ggtext)
library(scales)
library(precintcon)

pdx_weather %>% 
 mutate(year = year(date),
        month = month(date),
        day = day(date)) %>%
  select(-tmax, -snow) %>% 
  drop_na(prcp) %>% 
  filter(year != 1938 & year != 1939 & year != 2020 & year != 2021 & year != 2022) %>% 
  mutate(decade = case_when(
    year >=1940 & year <=1949 ~ "1940's",
    year >=1950 & year <=1959 ~ "1950's",
    year >=1960 & year <=1969 ~ "1960's",
    year >=1970 & year <=1979 ~ "1970's",
    year >=1980 & year <=1989 ~ "1980's",
    year >=1990 & year <=1999 ~ "1990's",
    year >=2000 & year <=2009 ~ "2000's",
    year >=2010 & year <=2019 ~ "2010's"
    )) %>% 
  group_by(decade) %>% 
  summarize(total_prcp = sum(prcp)) %>% 
  ggplot(aes(x = decade, y = total_prcp, fill = total_prcp)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 425),
                     expand = c(0,0)) +
  scale_fill_binned() +
  labs(y = "Total Precipitation (in)",
       title = "Total Precipitation By Decade",
       subtitle = "It has not been drier than usual since I moved to Portland in 2011") +
  theme(axis.title.x = element_blank(),
        axis.line = element_line(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot")

ggsave("figures/precipitation_by_decade.png")
