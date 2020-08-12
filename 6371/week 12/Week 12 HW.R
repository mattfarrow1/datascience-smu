library(tidyverse)
library(hrbrthemes)
library(GGally)
library(patchwork)
library(broom)

# Load data
brain <- read_csv("Documents/DataScience@SMU/MSDS-6371-Stat-Foundations/Unit 12/Brain.csv")

# Clean column names
brain <- janitor::clean_names(brain)

# Look at the data
head(brain)

# Plot the data
brain %>% 
  select(-1) %>% 
  ggpairs() +
  theme_ipsum()

# Log transform the data
brain <- brain %>% 
  mutate(log_brain = log(brain),
         log_body = log(body),
         log_gestation = log(gestation),
         log_litter = log(litter))

# Plot the log-transformed data
brain %>% 
  select(6:9) %>% 
  ggpairs() +
  theme_ipsum()

# Create the model
model <- lm(log_brain ~ log_body + log_gestation + log_litter, data = brain)
summary(model)

# Plot residuals
a <- model %>% 
  ggplot(aes(.fitted, .resid)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "blue") +
  labs(title = "Residuals") +
  theme_ipsum()

# Plot studentized residuals
b <- model %>% 
  augment() %>% 
  ggplot(aes(.fitted, .std.resid)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "blue") +
  labs(title = "Studentized Residuals") +
  theme_ipsum()

# Q-Q Plot of Residuals
c <- model %>% 
  ggplot(aes(sample = .resid)) +
  stat_qq(alpha = 0.2, size = 3) +
  stat_qq_line(color = "darkblue") +
  labs(title = "Q-Q Plot of Residuals") +
  theme_ipsum()

# Histogram of residuals
d <- model %>% 
  ggplot(aes(.resid, ..density..)) +
  geom_histogram(fill = "lightblue", color = "darkblue") +
  geom_density() +
  labs(title = "Histogram of Residuals") +
  theme_ipsum()

(a + b) / (c + d)

# Bonus -------------------------------------------------------------------

# Load data
crabs <- read_csv("Documents/DataScience@SMU/MSDS-6371-Stat-Foundations/Unit 12/Crab17.csv")
crabs <- janitor::clean_names(crabs)

# Initial Scatterplot
crabs %>% 
  ggplot(aes(height, force, color = species)) +
  geom_point(size = 3) +
  labs(title = "Height vs. Force in Crab Species",
       x = "Height",
       "y = Force",
       color = "") +
  theme_ipsum() +
  theme(legend.position = "bottom") +
  NULL

# Convert species to factor and set levels
crabs$species <- as_factor(crabs$species)
crabs$species <- fct_relevel(crabs$species,
                             "Lophopanopeus bellus",
                             "Cancer productus",
                             "Hemigrapsus nudus")

# Create model
crab_model <- lm(force ~ height*species, data = crabs)
summary(crab_model)
