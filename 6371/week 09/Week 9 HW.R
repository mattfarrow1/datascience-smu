library(tidyverse)

# Baseball Data -----------------------------------------------------------

baseball <- read_csv("Documents/DataScience@SMU/MSDS-6371-Stat-Foundations/Unit 9/Baseball_Data.csv")

fit <- lm(Wins ~ Payroll, data = baseball)

sum(residuals(fit)^2)

summary(fit)

confint(fit)

# Test Data ---------------------------------------------------------------

# Load data
df <- read_csv("Documents/DataScience@SMU/MSDS-6371-Stat-Foundations/Unit 9/TEST DATA.csv")

# Scatterplot
df %>% 
  ggplot(aes(science, math)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Simple Linear Regression of Science & Math Scores",
       x = "Science Scores",
       y = "Math Scores",
       caption = "Matt Farrow — Unit 9 HW") +
  theme_minimal() +
  NULL

# Correlation
fit <- lm(math ~ science, data = df)
sum(residuals(fit)^2)
summary(fit)
confint(fit)

qplot(.fitted, .resid, data = fit) +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  ggtitle("Residuals Against Fitted Values")

qplot(sample = .resid, data = fit) +
  stat_qq_line() +
  ggtitle("Normal Probability Plot of Residuals")
