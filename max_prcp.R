source("pdx_weather.R")
library(ggtext)
library(scales)
# Looking at Precipitation in Portland

# Finding the wettest year on record
max_prcp <- pdx_weather %>% 
  drop_na(prcp) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  filter(year != 1938) %>% 
  group_by(year) %>% 
  summarize(total_prcp = sum(prcp)) %>% 
  ungroup() %>% 
  arrange(desc(total_prcp)) %>% 
  slice_max(total_prcp)

# Plotting the wettest year on record (1996)
pdx_weather %>% 
  select(date, prcp) %>% #looking at date and precipitation
  drop_na(prcp) %>% #drop any rows with NA values for precipitation
  mutate(year = year(date), 
         month = month(date),
         day = day(date),
         max_year = year == 1996) %>% #add year, month & day columns
  filter(year != 1938 & (month != 2 & day != 29)) %>% #remove partial year and leap days
  group_by(year) %>% 
  mutate(cum_prcp = cumsum(prcp)) %>% #use mutate() instead of summarize() to keep month & day columns
  ungroup() %>% 
  mutate(new_date = ymd(glue("2022-{month}-{day}"))) %>% #create pseudo date column so x axis only contains one year worth of dates
  ggplot(aes(x = new_date, y = cum_prcp, group = year, 
             color = max_year, size = max_year)) + #highlight wettest year
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
  labs(x = NULL, y = "Cumulative Precipitation (in)",
       title = "Maximum Precipitation",
       subtitle = glue("The year <span style = 'color: dodgerblue'>1996</span> was the was the wettest year on record in Portland")) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(margin = margin(b=5)),
    plot.subtitle = element_textbox_simple(margin = margin(b=10)),
  )

ggsave("Figures/max_prcp.png", width = 6, height = 5)
