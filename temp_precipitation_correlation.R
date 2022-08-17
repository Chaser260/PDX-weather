source("pdx_weather.R")

tmax_prcp <- pdx_weather %>%
  mutate(year = year(date)) %>% 
  filter(year != 1938 & year != year(today())) %>% #remove partial years to not skew average temp and prcp
  group_by(year) %>% 
  summarize(tmax = mean(tmax, na.rm = T), #calculate mean high temp and sum prcp. remove na values for respective columns.
            prcp = sum(prcp, na.rm = T))

tmax_prcp %>% 
  pivot_longer(-year) %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  facet_wrap(~name, ncol = 1, scales = "free_y") +
  geom_smooth(se = F) +
  theme_classic()

  