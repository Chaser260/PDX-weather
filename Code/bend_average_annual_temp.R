source("Code/bend_weather.R")
library(ggtext)
library(scales)
# Looking at Precipitation in Bend

this_year <- year(today())
this_month <- month(today(), label = T, abbr = F)
this_day <- ordinal(day(today()))
month_labels <- c("February", "April", "June", "August", "October", "December")

# Plotting average high temp trends
bend_weather %>% 
  select(date, tmax) %>%
  mutate(year = year(date)) %>% 
  filter(year != 2022) %>% 
  group_by(year) %>% 
  summarize(tmax = mean(tmax)) %>%
  mutate(normalize_range = (year >= 1951 & year <= 1981),
         normalize_mean = sum(tmax * normalize_range)/sum(normalize_range),
         t_diff = tmax - normalize_mean) %>% 
  ggplot(aes(x = year, y = t_diff)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1910, 2020, 10)) +
  geom_smooth(se = F, method = lm) +
  theme_classic() +
  labs(x = "Temperature Change", 
       y = "Year", 
       title = "Bend Temperature Change",
       subtitle = "The average high temperature in Bend has decreased since 1912") +
  theme(axis.ticks = element_blank())

ggsave("Figures/bend_temperature_trend.png", width = 6, height = 4)

# Average annual temp by month
bend_weather %>% 
  select(date, tmax) %>% #looking at temperature
  mutate(year = year(date),
         month = month(date)) %>% #add year and month columns
  group_by(year, month) %>% 
  summarize(tmax = mean(tmax), .groups = "drop") %>% #looking at average temp for each month of each year
  group_by(month) %>% 
  mutate(normalize_range = year >= 1951 & year <= 1981, #normalized range from my birth year until I moved to Portland
         normalized_temp = sum(tmax * normalize_range)/sum(normalize_range),
         t_diff = tmax - normalized_temp,
         is_this_year = year == 2022) %>% # add logic to show this year only for trend line
  ungroup() %>% 
  ggplot(aes(x = month, y = t_diff, group = year, color = is_this_year)) +
  geom_line() +
  scale_x_continuous (expand = c(0,0),
                      breaks = c(2, 4, 6, 8, 10, 12),
                      labels = month_labels) +
  scale_y_continuous(breaks = seq(-15, 15, 5)) +
  scale_color_manual(breaks = c(F, T),
                     values = c("light gray", "dodgerblue"),
                     guide = "none") +
  theme_classic() +
  labs(x = "Month", 
       y = "Temperature Difference", 
       title = "Bend Temperature Change By Month", 
       subtitle = "2022 is trending hotter than average") +
  theme(axis.ticks = element_blank())

ggsave("Figures/bend_temp_change.png", width = 6, height = 4)
