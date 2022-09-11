source("Code/pdx_weather.R")
library(ggtext)

pdx_weather %>% 
  select(date, tmax) %>% #looking at date and precipitation
  drop_na(tmax) %>% #drop any rows with NA values for precipitation
  mutate(year = year(date), 
         month = month(date),
         day = day(date),
         is_this_year = year == 2022) %>% #add year, month & day columns
  filter(year != 1938) %>% #remove partial year and leap days
  group_by(year) %>% 
  mutate(new_date = ymd(glue("2022-{month}-{day}"))) %>% #create pseudo date column so x axis only contains one year worth of dates
  ggplot(aes(x = new_date, y = tmax, group = year, 
             color = is_this_year)) + 
  geom_point(show.legend = F, alpha = .75) +
  geom_smooth(linetype = 5,
              aes(group = 1),
              color = "white", size = .75,
              se = F) +
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


