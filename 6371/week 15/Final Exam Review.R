library(tidyverse)
library(broom)
library(GGally)
library(hrbrthemes)

# Load data
df <- read_csv("Documents/DataScience@SMU/MSDS-6371-Stat-Foundations/Unit 15/Twitterdata.csv")

# Clean column names
df <- janitor::clean_names(df)

# Add gender label column & log followers
df <- df %>% 
  mutate(sex = if_else(gender == 0, "M", "F"),
         log_followers = log(followers)) %>% 
  rename(follow_ees = follow_e_es)

# Create a model
model <- lm(followers ~ follow_ees + tweets_per_wk, data = df)
summary(model)

# Augment the model
model_plus <- augment(model)

# Exammine the data
model %>% 
  ggpairs()

# Create a model of log-transformed followers
model <- lm(log_followers ~ follow_ees + tweets_per_wk, data = df)
model %>% 
  ggpairs() + 
  theme_ipsum()
