library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)
temp_carbon <- temp_carbon %>% filter(!is.na(temp_anomaly))
View(temp_carbon)

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  pull(year) %>%
  min()

temp_carbon %>% filter(year %in% c(2018, 1880)) %>% .$temp_anomaly

temp <- temp_carbon %>% select(temp_anomaly, ocean_anomaly, land_anomaly)

temp_carbon %>% 
  ggplot(aes(year, temp, col=temp)) + 
  geom_line() + geom_hline(aes(yintercept = 0), col = "blue") + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue") + facet_grid(.~year)












library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)
View(greenhouse_gases)


greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept = 1850), col = "blue") +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")



historic_co2 %>% ggplot(aes(year, co2, col = source)) + geom_line()












































































