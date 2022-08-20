source("Code/pdx_weather.R")

pretty_labels <- c(tmax = "Average high temperature",
                   prcp = "Average amount of precipitation")

tmax_prcp <- pdx_weather %>%
  mutate(year = year(date)) %>% 
  filter(year != 1938 & year != year(today())) %>% #remove partial years to not skew average temp and prcp
  group_by(year) %>% 
  summarize(tmax = mean(tmax, na.rm = T), #calculate mean high temp and sum prcp. remove na values for respective columns.
            prcp = sum(prcp, na.rm = T))

# Faceted line plots
tmax_prcp %>% 
  pivot_longer(-year) %>% # need tmax and prcp to be in single column for facet
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  facet_wrap(~name, ncol = 1, scales = "free_y", strip.position = "left", # move the facet label to the left as titles
             labeller = labeller(name = pretty_labels)) + #ncol to arrange plots virtically. free_y to give each y axis proper scales.
  geom_smooth(se = F) +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = NULL,
       y = NULL) +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line()) +
  labs(title = "Precipitation Vs. Temperature",
       subtitle = "While average temperature appears to be rising, precipitation \nseems to be the same")

ggsave("Figures/temp_precipitation_line.png")

# Scatterplot
tmax_prcp %>%
  ggplot(aes(x = tmax, y = prcp)) +
  geom_point() +
  geom_smooth(se = F, method = "lm") +
  theme_classic() +
  theme(plot.title.position = "plot") +
  labs(x = "Average Temperature (\u00B0F)",
       y = "Precipitation (in)",
       title = "Temperature Vs. Precipitation",
       subtitle = "There is no significant correlation between average temperature and amount of \nprecipitation")

ggsave("Figures/temp_precipitation_scatterplot.png")

cor.test(tmax_prcp$tmax, tmax_prcp$prcp) # Does not appear to be a correlation between average temperature and amount of precipitation.

  
  