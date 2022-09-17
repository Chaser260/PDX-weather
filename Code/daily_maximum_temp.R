source("Code/pdx_weather.R")
library(ggtext)

annotation <- data.frame(
  x = as.Date("2022-09-01"),
  y = 45,
  label = "Normalized temperature from 1951-1980"
)

# normalized_temp <- pdx_weather %>% 
#   select(date, tmax) %>% 
#   mutate(year = year(date), 
#          month = month(date),
#          day = day(date)) %>%
#   filter(year >= 1988 & year <=2011) %>% #remove partial year and leap days
#   group_by(year, month, day) %>% 
#   summarize(tmax = mean(tmax), .groups = "drop") %>% 
#   mutate(new_date = ymd(glue("2022-{month}-{day}")))

pdx_weather %>% 
  select(date, tmax) %>% #looking at date and precipitation
  drop_na(tmax) %>% #drop any rows with NA values for precipitation
  mutate(year = year(date), 
         month = month(date),
         day = day(date),
         is_this_year = year == 2022) %>%
  filter(year != 1938) %>% #remove partial year and leap days
  group_by(year) %>% 
  mutate(new_date = ymd(glue("2022-{month}-{day}"))) %>% #create pseudo date column so x axis only contains one year worth of dates
  ggplot(aes(x = new_date, y = tmax, group = year, 
             color = is_this_year)) + 
  geom_point(show.legend = F, alpha = .75) +
  geom_smooth(data = normalized_temp,
            aes(x = new_date, y = tmax, group = 1),
            color = "white", size = .75,
            se = F,
            linetype = 5,
            method = 'gam',
            formula = y~s(x, bs = "cs")) +
  geom_label(data = annotation, 
             aes(x = as.Date("2022-08-15"), y = 50, label = "1951-1980 \naverage temperature"), 
             inherit.aes = F,
             size = 3) +
  annotate("segment", x = as.Date("2022-08-15"), y = 55, 
           xend = as.Date("2022-09-25"), yend = 70,
           arrow = arrow(type = "open"),
           size = 1,
           linejoin = "mitre") +
  # annotate("text", x = as.Date("2022-08-15"), y = 50,
  #          label = "1988-2011 \naverages",
  #          size = 3) +
  scale_color_manual(breaks = c(F, T),
                     values = c("gray", "dodgerblue")) +
  scale_x_date(date_labels = "%b", breaks = "1 month",
               expand = c(0,0)) +
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100, 120)) +
  labs(x = NULL, y = "Daily Maximum Temperature (\u00B0F)",
       title = "Record Heat",
       subtitle = glue("September of <span style = 'color: dodgerblue'>2022</span> is the hottest September on record so far")) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(margin = margin(b=5)),
    plot.subtitle = element_textbox_simple(margin = margin(b=10)),
  )

ggsave("Figures/PDX_Record_Heat.png") 


