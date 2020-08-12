library(readr)
library(dplyr)

# Load data
df <- read_csv(here::here("6371", "week 15", "Twitterdata.csv"))

# Clean column names
df <- janitor::clean_names(df)

# Add gender label column & log followers
df <- df %>% 
  mutate(sex = if_else(gender == 0, "M", "F"),
         log_followers = log(followers)) %>% 
  rename(follow_ees = follow_e_es)

# Build model
model <- lm(log_followers ~ follow_ees + tweets_per_wk + gender + (gender * follow_ees) + (gender * tweets_per_wk), data = df)
model_reduced <- lm(log_followers ~ gender + follow_ees + tweets_per_wk, data = df)

# Build Your Own ANOVA Function
build_own_anova <- function(x, y){
  
  library(dplyr)
  
  # Get df and sum of squares from full model
  full <- x %>% 
    anova() %>% 
    slice_tail(1) %>% 
    select(1:2)
  
  # Get df and sum of squares from reduced model
  reduced <- y %>% 
    anova() %>% 
    slice_tail(1) %>% 
    select(1:2)
  
  # Create empty tibble
  byo_anova <- tibble(
    "source" = character(),
    "df" = numeric(),
    "SS" = numeric(),
    "MS" = numeric(),
    "F" = numeric(),
    "P-Value" = numeric()
  )
  
  # Label rows
  byo_anova[1,1] <- "Model"
  byo_anova[2,1] <- "Error"
  byo_anova[3,1] <- "Total"
  
  # Copy ANOVA values for df
  byo_anova[3,2] <- reduced$Df
  byo_anova[2,2] <- full$Df
  byo_anova[1,2] <- byo_anova$df[3] - byo_anova$df[2]
  
  # Copy ANOVA values for sum of squares
  byo_anova[3,3] <- reduced$`Sum Sq`
  byo_anova[2,3] <- full$`Sum Sq`
  byo_anova[1,3] <- byo_anova$SS[3] - byo_anova$SS[2]
  
  # Calculate MS
  byo_anova[2,4] <- byo_anova$SS[2] / byo_anova$df[2]
  byo_anova[1,4] <- byo_anova$SS[1] / byo_anova$df[1]
  
  # Calculate F
  byo_anova[1,5] <- byo_anova$MS[1] / byo_anova$MS[2]
  
  # Calculate P-value
  byo_anova[1,6] <- pf(byo_anova$F[1], byo_anova$df[2], byo_anova$df[1], lower.tail = FALSE)
  
  byo_anova
}

byo <- build_own_anova(model, model_reduced)