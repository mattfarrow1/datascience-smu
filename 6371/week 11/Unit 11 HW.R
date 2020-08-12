library(tidyverse)
library(hrbrthemes)
library(scales)

# From Problem 26, Chapter 8: ---------------------------------------------

# Load Data
metabolism <- read_csv("Documents/DataScience@SMU/MSDS-6371-Stat-Foundations/Unit 11/Metabolism Data Prob 26.csv")

# Clean Names
metabolism <- janitor::clean_names(metabolism)

# Inital Scatter Plot
metabolism %>% 
  mutate(mass_34 = mass^.75) %>% 
  ggplot(aes(metab, mass_34)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = comma) +
  labs(title = "Is it reasonable that the metabolic rate of an animal species, on average, \nis proportional to its mass raised to the power of ¾?",
       x = "Metabolism",
       y = "Mass^¾ Power") +
  theme_ipsum() +
  NULL

# From Problem 29, Chapter 8 ----------------------------------------------

# Load Data
df <- read_csv("Documents/DataScience@SMU/MSDS-6371-Stat-Foundations/Unit 11/Autism Data Prob 29.csv")

# Clean Names
df <- janitor::clean_names(df)

# Create Model
model <- lm(prevalence ~ year, data = df)
summary(model)

# Append Predictions to Dataset
autism <- cbind(df, predict.lm(model, interval = "predict", level = 0.95))

# Inital Scatter Plot
autism %>% 
  ggplot(aes(year, prevalence)) +
  geom_point() +
  geom_smooth(method = "lm", level = 0.95) +
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") +
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") +
  labs(title = "How has the distribution of the prevalence of autism changed over the years?",
       x = "Year",
       y = "Prevalence") +
  theme_ipsum() +
  NULL

# Log Transform the Data
log_autism <- df %>% 
  mutate(log_prevalence = log(prevalence))

# Create Model
log_model <- lm(log_prevalence ~ year, data = log_autism)
summary(log_model)
confint(log_model)

# Append Predictions to Dataset
log_autism <- cbind(log_autism, predict.lm(log_model, interval = "predict", level = 0.95))

# Log Transformed Data
log_autism %>% 
  ggplot(aes(year, log_prevalence)) +
  geom_point() +
  geom_smooth(method = "lm", level = 0.95) +
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") +
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") +
  labs(title = "How has the distribution of the prevalence of autism changed over the years?",
       x = "Year",
       y = "Prevalence (log)") +
  theme_ipsum() +
  NULL

# Plot Residuals
log_autism %>% 
  mutate(residuals = resid(log_model)) %>% 
  ggplot(aes(year, residuals)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  labs(title = "Autism Residuals",
       x = "Year",
       y = "Residuals (log)") +
  theme_ipsum() +
  NULL

# Plot Residual Histogram & Normal Distribution

log_autism <- log_autism %>% 
  mutate(residuals = resid(log_model))

log_autism %>% 
  ggplot(aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 3, color = "darkblue", fill = "lightblue") + 
  stat_function(fun = dnorm,
                args = list(mean = mean(log_autism$residuals),
                            sd = sd(log_autism$residuals)),
                geom = "density",
                fill = "firebrick2",
                alpha = 0.5) +
  labs(title = "Autism Histogram",
       x = "Residuals (log)",
       y = "Density") +
  theme_ipsum() +
  NULL

log_autism %>% 
  mutate(residuals = resid(log_model)) %>% 
  ggplot(aes(x = residuals)) +
  geom_density(aes(x = ))
