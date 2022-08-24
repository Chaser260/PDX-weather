source("Code/bend_weather.R")

# Max snowfall by year and month in Bend
max_snow <- bend_weather %>% 
  drop_na(snow) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>%
  group_by(year, month) %>% 
  summarize(total_snow = sum(snow)) %>% 
  ungroup() %>%
  arrange(desc(total_snow)) %>% 
  slice_max(total_snow) # Bend shares the same year as Portland with the highest total snowfall in 1950 

# Min snowfall
min_snow <- bend_weather %>% 
  drop_na(snow) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  group_by(year) %>% 
  summarize(total_snow = sum(snow), .groups = "drop") %>%
  filter(year != 2022) %>% 
  arrange(total_snow) # 8 of the 10 lowest snowfall years have occurred in the last 20 years.

#Latest snowfall
bend_weather %>% 
  drop_na(snow) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  select(year, month, day, snow) %>% 
  filter(month == 5 & snow >0) #The latest it has snowed in Bend is May 23, in 1953

#Earliest snowfall
bend_weather %>% 
  drop_na(snow) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  select(year, month, day, snow) %>% 
  filter(month == 08 & snow >0) #The earliest snowfall in Bend was August 9, 1950 with nearly 1 inch.

# Snow year calendar

# Filtering snow calendar for 2021 (since 2022 snow year hasn't happened yet)
bend_snow_data <- bend_weather %>% 
  select(date, snow) %>% #looking at date and snow
  drop_na(snow) %>% #drop any rows with NA values for snow
  mutate(cal_year = year(date), 
         month = month(date),
         snow_year = if_else(date < ymd(glue("{cal_year}-08-01")),
                             cal_year - 1,
                             cal_year)) %>% 
  filter(snow_year != 2022) %>% #remove partial year
  select(month, snow_year, snow) #remove unwanted columns

# Create dummy data frame to fill snow NA values with 0 to fix an issue with line plot where data did not exist.
dummy_df <- crossing(snow_year = 1912:2021,
                     month = 1:12) %>% 
  mutate(dummy = 0)

# Plotting snow year
bend_snow_data %>%
  right_join(., dummy_df, by = c("snow_year", "month")) %>%
  mutate(snow = if_else(is.na(snow), dummy, snow)) %>% #join the dummy_df and overwrite snow column to add zeroes.
  group_by(snow_year, month) %>% 
  summarize(snow = sum(snow), .groups = "drop") %>% 
  mutate(month = factor(month, levels = c(8:12, 1:7)),
         is_this_year = 2021 == snow_year) %>%
  ggplot(aes(x = month, y = snow, group = snow_year, color = is_this_year)) +
  geom_line() +
  scale_color_manual(name = NULL,
                     labels = c("2021 Snow Year", "All Other Years"),
                     breaks = c(T,F),
                     values = c("dodgerblue", "gray")) +
  scale_x_discrete(breaks = c(10, 11, 12, 1, 2, 3, 4, 5),
                   labels = month.abb[c(10, 11, 12, 1, 2, 3, 4, 5)],
                   expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 60, 5)) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(),
        axis.title.x = element_blank()) +
  labs(title = "Bend Snow By Snow Year",
       subtitle = "Bend typically sees snow between October and May. \nJune and July are the only months Bend has never seen snow.",
       y = "Snow Accumulation (in)")

ggsave("Figures/bend_snow_by_month.png", width = 6, height = 4)
