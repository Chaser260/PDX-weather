source("Code/pdx_weather.R")
library(ggtext)
library(scales)
# Looking at Precipitation in Portland

this_year <- year(today())
this_month <- month(today(), label = T, abbr = F)
this_day <- ordinal(day(today()))

# Plotting cumulative precipitation for 2022
pdx_weather %>% 
  select(date, prcp) %>% #looking at date and precipitation
  drop_na(prcp) %>% #drop any rows with NA values for precipitation
  mutate(year = year(date), 
         month = month(date),
         day = day(date),
         is_this_year = year == this_year) %>% #add year, month & day columns
  filter(year != 1938) %>% #remove partial year 
  group_by(year) %>% 
  mutate(cum_prcp = cumsum(prcp)) %>% #use mutate() instead of summarize() to keep month & day columns
  ungroup() %>% 
  mutate(new_date = ymd(glue("2022-{month}-{day}"))) %>% #create pseudo date column so x axis only contains one year worth of dates
  ggplot(aes(x = new_date, y = cum_prcp, group = year, 
             color = is_this_year, size = is_this_year)) + #highlight current year
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
       title = "PDX Cumulative Precipitation",
       subtitle = glue("Through {this_month} {this_day}, the cumulative precipitation in Portland is <span style = 'color: dodgerblue'>above average</span> for {this_year}")) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(margin = margin(b=5)),
    plot.subtitle = element_textbox_simple(margin = margin(b=10)),
  )
           
ggsave("Figures/current_prcp.png", width = 6, height = 5)


