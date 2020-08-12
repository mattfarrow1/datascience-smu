library(tidyverse)   # for everything
library(hrbrthemes)  # for pretty plotting
library(broom)       # to tidy linear model results
library(kableExtra)  # for creating pretty tables
library(investr)     # for calculating calibration intervals

# Q1: Black-Eared Wheatears -----------------------------------------------

# Load data
birds <- read_csv("Documents/DataScience@SMU/MSDS-6371-Stat-Foundations/Unit 10/Male Display Data Set.csv")

# Fit a simple linear regression model
model <- lm(Tcell ~ Mass, data = birds)

# View summary of fitted model
summary(model) 

# # Save table
# kable(tidy(model)) %>% 
#   kable_styling(bootstrap_options = "striped", full_width = FALSE) %>% 
#   save_kable("kable.png")

# Create prediction intervals
predictions <- predict.lm(model, interval = "predict", level = 0.99)

# Merge bids and predictions
df <- cbind(birds, predictions)

# Plot
ggplot(df, aes(Mass, Tcell)) +
  geom_point() +
  geom_smooth(method = "lm", level = 0.99) +
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") +
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") +
  labs(title = "Unit 10 Homework: Question 1",
       x = "Stone Mass (g)",
       y = "T-Cell Response Measurement (mm)") +
  theme_ipsum() +
  NULL

# ggsave("q1scatter.png", height = 4, width = 6, units = "in", dpi = "retina")

# Critical Value
qt(0.995, 19)

# Confidence Intervals
confint(model, level = 0.99)

# Confidence & Prediction When Stone = 4.5g -------------------------------
new_point <- tibble(Mass = c(4.5), Tcell = NA)
predict(model, new_point, interval = "confidence", level = 0.99)
predict(model, new_point, interval = "prediction", level = 0.99)

# Calibration Intervals ---------------------------------------------------

# Mean t-cell response of 0.3
calibrate(model, y0 = 0.3, interval = "inversion", mean.response = TRUE, level = 0.99)

# Single t-cell response of 0.3
calibrate(model, y0 = 0.3, interval = "inversion", level = 0.99)

ggplot(df, aes(Mass, Tcell)) +
  geom_point() +
  geom_smooth(method = "lm", level = 0.99) +
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") +
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") +
  # geom_segment(aes(x = -4.389857, y = 0.305, xend = 8.342649, yend = 0.305, color = "Calibration Interval: Mean")) +
  # geom_segment(aes(x = -17.968869, y = 0.3, xend = 21.921661, yend = 0.3, color = "Calibration Interval: Single")) +
  geom_segment(aes(x = 0, y = 0.305, xend = 8.342649, yend = 0.305, color = "Calibration Interval: Mean")) +
  geom_segment(aes(x = 0, y = 0.3, xend = 21.921661, yend = 0.3, color = "Calibration Interval: Single")) +
  labs(title = "Unit 10 Homework: Question 1",
       x = "Stone Mass (g)",
       y = "T-Cell Response Measurement (mm)",
       color = "",
       caption = "X-axis truncated at 0 since stone mass cannot be less than 0") +
  xlim(0, 22) +
  theme_ipsum() +
  NULL

ggsave("q1calibration.png", height = 4, width = 6, units = "in", dpi = "retina")

# Plot residuals
ggplot(df, aes(Mass, Tcell)) +
  geom_point() +
  geom_smooth(method = "lm", level = 0.99, se = FALSE, color = "lightgrey") +
  labs(title = "Unit 10 Homework: Question 1",
       subtitle = "1.j. - Scatterplot of Residuals",
       x = "Stone Mass (g)",
       y = "T-Cell Response Measurement (mm)") +
  theme_ipsum() +
  NULL

ggsave("q1residuals.png", height = 4, width = 6, units = "in", dpi = "retina")

# Plot histogram with normal distribution
x <- seq(3.3, 9.95, length.out = 100)
x_norm <- with(df, data.frame(x = x, y = dnorm(x, mean(Mass), sd(Mass))))

ggplot(df, aes(Mass, ..density..)) +
  geom_histogram(binwidth = 0.35, color = "darkblue", fill = "lightblue") +
  geom_line(data = x_norm, aes(x = x, y = y), color = "red") +
  labs(title = "Unit 10 Homework: Question 1",
       subtitle = "1.k. - Histogram & Normal Distribution",
       x = "Stone Mass (g)",
       y = "Density") +
  theme_ipsum() +
  NULL
ggsave("q1histogram.png", height = 4, width = 6, units = "in", dpi = "retina")
