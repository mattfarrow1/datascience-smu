byo_anova <-
  tibble(
    "source" = c("Model", "Error", "Total"),
    "df" = c(2, 393, 395),
    "SS" = c(436.6, 14.51, 451.11),
    "MS" = c(218.3, 0.0369, 0),
    "F" = c(5915.9892, 0, 0),
    "P-Value" = c(0, 0, 0)
  )

byo_anova[1,6] <- pf(byo_anova$F[1], byo_anova$df[2], byo_anova$df[1], lower.tail = FALSE)

byo_anova <- na_if(byo_anova, 0)

options(knitr.kable.NA = '')

byo_anova %>% 
  kable() %>% 
  kable_styling("striped")