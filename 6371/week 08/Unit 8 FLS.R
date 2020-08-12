
# Crickets ----------------------------------------------------------------

library(tidyverse)

crickets <- read_csv("Documents/DataScience@SMU/MSDS-6371-Stat-Foundations/Unit 8/Crickets.csv")

crickets %>% 
  ggplot(aes(chirps, temp)) +
  geom_point(color = "indianred") +
  labs(title = "Cricket Chirps per Minute by Temperature",
       x = "Chirps",
       y = "Temp (ºF)",
       caption = "Matt Farrow, FLS Unit 8") +
  theme_minimal() +
  NULL

# Calculate critical value
round(qt(.975, 6), 3)

# Run correlation test
cor.test(crickets$chirps, crickets$temp, method = "pearson")

# Marathons ---------------------------------------------------------------

marathon <- read_csv("Documents/DataScience@SMU/MSDS-6371-Stat-Foundations/Unit 8/Marathon.csv")

marathon %>% 
  ggplot(aes(temp, time)) +
  geom_point(color = "indianred") +
  labs(title = "Marathon Times by Temperature",
       x = "Temp (ºF)",
       y = "Time",
       caption = "Matt Farrow, FLS Unit 8") +
  theme_minimal() +
  NULL

# Calculate critical value
round(qt(.975, 6), 3)

# Run correlation test
cor.test(marathon$temp, marathon$time, method = "pearson")

# Calculate r^2
0.1831275^2
