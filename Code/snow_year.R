source("Code/pdx_weather.R")
library(ggtext)
library(scales)
# Looking at Snow in Portland

this_year <- year(today()) #create variables to be able to highlight the line for this year
this_month <- month(today(), label = T) 
this_day <- ordinal(day(today()))

# Max snowfall by year and month at PDX
max_snow <- pdx_weather %>% 
  drop_na(snow) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  filter(year != 1938) %>% 
  group_by(year, month) %>% 
  summarize(total_snow = sum(snow)) %>% 
  ungroup() %>%
  arrange(desc(total_snow)) %>% 
  slice_max(total_snow) # January of 1950 dumped 41.5 in. of snow at PDX

#Min snowfall by year and month at PDX
min_snow <- pdx_weather %>% 
  drop_na(snow) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  group_by(year) %>% 
  summarize(total_snow = sum(snow), .groups = "drop") %>%
  filter(year != 1938 & total_snow == 0) # Portland has 9 years on record with zero snowfall.

# Latest snow
pdx_weather %>% 
  drop_na(snow) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  select(year, month, day, snow) %>% 
  filter(month == 5 & snow >0) # 1953 is the only year on record with snowfall in May.

# Earliest snow
pdx_weather %>% 
  drop_na(snow) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  select(year, month, day, snow) %>% 
  filter(month == 10 & snow >0) # 1950 was the only year with snow recorded in October.

# Plotting cumulative snowfall
pdx_weather %>% 
  select(date, snow) %>% #looking at date and precipitation
  drop_na(snow) %>% #drop any rows with NA values for precipitation
  mutate(year = year(date), 
         month = month(date),
         day = day(date),
         max_year = year == 1950) %>% #add year, month & day columns
  filter(year != 1938 & (month != 2 & day != 29)) %>% #remove partial year and leap days
  group_by(year) %>% 
  mutate(cum_snow = cumsum(snow)) %>% #use mutate() instead of summarize() to keep month & day columns
  ungroup() %>% 
  mutate(new_date = ymd(glue("2022-{month}-{day}"))) %>% #create pseudo date column so x axis only contains one year worth of dates
  ggplot(aes(x = new_date, y = cum_snow, group = year, 
             color = max_year, size = max_year)) + #highlight snowiest year
  geom_line(show.legend = F) +
  geom_smooth(aes(group = 1), 
              color = "black",
              size = 0.5,
              se = F) +
  scale_color_manual(breaks = c(F, T),
                     values = c("lightgray", "dodgerblue")) +
  scale_size_manual(breaks = c(F,T),
                    values = c(0.3, 1)) +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = NULL, y = "Cumulative Snow (in)",
       title = "Max Annual Snowfall",
       subtitle = glue("The year <span style = 'color: dodgerblue'>1996</span> had the most cumulative snowfall on record in Portland")) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(margin = margin(b=5)),
    plot.subtitle = element_textbox_simple(margin = margin(b=10)),
  )

ggsave("Figures/pdx_cumulative_snowfall.png", width = 6, height = 4)

# Snow year calendar

# Filtering snow calendar for 2021 (since 2022 snow year hasn't happened yet)
snow_data <- pdx_weather %>% 
  select(date, snow) %>% #looking at date and snow
  drop_na(snow) %>% #drop any rows with NA values for snow
  mutate(cal_year = year(date), 
         month = month(date),
         snow_year = if_else(date < ymd(glue("{cal_year}-08-01")),
                             cal_year - 1,
                             cal_year)) %>% 
  filter(snow_year != 1938 & snow_year != 2022) %>% #remove partial year
  select(month, snow_year, snow) #remove unwanted columns

# Plot Snowfall by year
snow_data %>% 
  group_by(snow_year) %>% 
  summarize(total_snow = sum(snow)) %>% 
  ggplot(x = (snow_year), y = total_snow) +
  geom_line(aes(x = snow_year, y = total_snow), show.legend = F) +
  scale_x_continuous(breaks = seq(1940, 2021, 10),
                     minor_breaks = seq(1040, 2020, 1),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 45, 5), 
                     expand = c(0,0)) +
  labs(x = NULL, y = "Total Snow (in)",
       title = "PDX Snowfall",
       subtitle = glue("January of 1950 delivered 41.5 inches of snow at PDX, Portland's single-month record.")) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(margin = margin(b=5)),
    plot.subtitle = element_textbox_simple(margin = margin(b=10)),
  )

ggsave("Figures/total_snow_by_year.png", width = 6, height = 4)

# Snowfall by month from 1939 - 2021
snow_data %>% 
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
  scale_y_continuous(breaks = seq(0, 40, 5)) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(),
        axis.title.x = element_blank()) +
  labs(title = "Portland Snow By Snow Year",
       subtitle = "Portland gets the most snowfall in January. \nIt normally only snows in Portland between November and April",
       y = "Snow Accumulation (in)")

ggsave("Figures/pdx_snow_by_month.png", width = 6, height = 4)
