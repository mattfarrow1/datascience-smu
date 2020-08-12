library(tidyverse)
library(hrbrthemes)
library(scales)
library(png)
library(grid)

df <- read_csv("Documents/DataScience@SMU/MSDS-6371-Stat-Foundations/Unit 8/Baseball_Data.csv")

rangers <- png::readPNG("/Users/mattfarrow/Downloads/Texas_Rangers_logo.png")
rangers <- rasterGrob(rangers, interpolate = TRUE)
cubs <- png::readPNG("/Users/mattfarrow/Downloads/Chicago_Cubs_logo.png")
cubs <- rasterGrob(cubs, interpolate = TRUE)

df %>% 
  ggplot(aes(Payroll, Wins)) +
  geom_point() +
  scale_x_continuous(labels = dollar) +
  labs(title = "Texas Rangers & Chicago Cubs",
       x = "Payroll ($ millions)",
       caption = "Matt Farrow, Week 8 Homework\ndata from 2010 season") +
  annotation_custom(rangers, xmin = 50, xmax = 60, ymin = 85, ymax = 95) +
  annotation_custom(cubs, xmin = 141, xmax = 151, ymin = 70, ymax = 80) +
  theme_ipsum() +
  NULL

ggsave("plot.png", units = "in", width = 10, height = 8, dpi = "retina")
