source("Code/pdx_weather.R")

month_labels <- c("February", "April", "June", "August", "October", "December")

# Visualize how temperature in 2022 compares to historical temperatures.

# create 'year' column and remove partial years
pdx_weather %>% 
  select(date, tmax) %>%
  mutate(year = year(date)) %>% 
  filter(year != 1938 & year != 2022) %>% 
  group_by(year) %>% 
  summarize(tmax = mean(tmax)) %>%
  mutate(normalize_range = (year >= 1988 & year <= 2010),
         normalize_mean = sum(tmax * normalize_range)/sum(normalize_range),
         t_diff = tmax - normalize_mean) %>% 
  ggplot(aes(x = year, y = t_diff)) +
  geom_line() +
  geom_smooth(se = F, method = lm) +
  theme_classic() +
  labs(x = NULL, y = "Temperature Change", 
       title = "PDX Temperature Change",
       subtitle = "The average temperature in Portland has been steadily increasing over the years") +
  theme(axis.ticks = element_blank())

ggsave("Figures/pdx_temperature change.png")


pdx_weather %>% 
  select(date, tmax) %>% #looking at temperature
  mutate(year = year(date),
         month = month(date)) %>% #add year and month columns
  filter(year != 1938) %>% # remove partial year
  group_by(year, month) %>% 
  summarize(tmax = mean(tmax), .groups = "drop") %>% #looking at average temp for each month of each year
  group_by(month) %>% 
  mutate(normalize_range = year >= 1988 & year <= 2010, #normalized range from my birth year until I moved to Portland
         normalized_temp = sum(tmax * normalize_range)/sum(normalize_range),
         t_diff = tmax - normalized_temp,
         is_this_year = year == 2022) %>% # add logic to show this year only for trend line
  ungroup() %>% 
  # filter(month == 1) %>% slice_min(t_diff, n = 5) %>% # check 5 coldest Januarys
  ggplot(aes(x = month, y = t_diff, group = year, color = is_this_year)) +
  geom_line() +
  scale_x_continuous(expand = c(0,0),
                     breaks = c(2, 4, 6, 8, 10, 12),
                     labels = month_labels) +
  scale_y_continuous(breaks = seq(-15, 15, 5)) +
  scale_color_manual(breaks = c(F, T),
                     values = c("light gray", "dodgerblue"),
                     guide = "none") +
  theme_classic() +
  labs(x = "Month", 
       y = "Temperature Difference", 
       title = "PDX Temperature Change By Month", 
       subtitle = "") +
  theme(axis.ticks = element_blank())

ggsave("Figures/pdx_average_annual_temp.png")
                    
