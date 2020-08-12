library(tidyverse)

crickets <- tibble(chirps = c(882, 1188, 1104, 864, 1200, 1032, 960, 900),
                   temp = c(69.7, 93.3, 84.3, 76.3, 88.6, 82.6, 71.6, 79.6))

scatter.smooth(x = crickets$chirps, y = crickets$temp)

cor(crickets$chirps, crickets$temp)

linearMod <- lm(temp ~ chirps, data = crickets)
print(linearMod)
summary(linearMod)

confint(linearMod, level = 0.05)
