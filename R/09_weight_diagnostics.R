# 09_weight_diagnostics.R — why does weighting halve the age slope?
# Adults ≥ 20, two-day recall (long_adults from 06_long.R)
# Balanced complete pairs => svyglm/lm coefficients equal svylme/lmer (see 08),
# so marginal models are used here: fast, and the design SEs are standard.

library(tidyverse)
library(survey)

long_adults <- readRDS("data/processed/long_adults.rds") %>%
  left_join(select(readRDS("data/processed/merged_all.rds"), SEQN, RIDRETH3),
            by = "SEQN") %>%
  mutate(age_c = RIDAGEYR - mean(RIDAGEYR),   # centred at unweighted sample mean (51.36)
         day_f = factor(day),
         age10 = cut(RIDAGEYR, c(20, 30, 40, 50, 60, 70, 80, Inf), right = FALSE),
         w10k  = WTDR2D / 1e4)                # weight in 10,000s (DuMouchel–Duncan test)

stopifnot(!anyNA(long_adults$RIDRETH3))

# Designs built after mutate() so they carry every new column
des_psu <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA,
                     weights = ~WTDR2D, nest = TRUE, data = long_adults)
des_u   <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA,   # unweighted, design-aware
                     weights = ~1, nest = TRUE, data = long_adults)

compare <- function(f) {
  tibble(term        = names(coef(lm(f, data = long_adults))),
         unweighted  = coef(lm(f, data = long_adults)),
         weighted    = coef(svyglm(f, design = des_psu)),
         weighted_SE = SE(svyglm(f, design = des_psu)))
}

# 1. Sex x age: is the attenuation a sex-composition effect? (No: within each sex)
res_sex_age <- compare(kcal ~ RIAGENDR * age_c + day_f)

# 2. Mean intake by age decade: weighted vs unweighted
res_age10 <- svyby(~kcal, ~age10, des_psu, svymean) %>%
  as_tibble() %>%
  mutate(unweighted = tapply(long_adults$kcal, long_adults$age10, mean),
         n_persons  = as.vector(table(long_adults$age10)) / 2)

# 3. Adjusting for race/ethnicity: does it close the gap? (No)
res_race <- compare(kcal ~ RIAGENDR + age_c + day_f + RIDRETH3)

# 4. DuMouchel–Duncan test: are the weights informative for this model?
dd      <- svyglm(kcal ~ (RIAGENDR + age_c + day_f) * w10k, design = des_u)
dd_test <- regTermTest(dd, ~ w10k + RIAGENDR:w10k + age_c:w10k + day_f:w10k)

res_dd <- tibble(F       = as.numeric(dd_test$Ftest),
                 df_num  = dd_test$df,
                 df_den  = dd_test$ddf,
                 p       = as.numeric(dd_test$p),
                 age_x_w = coef(dd)[["age_c:w10k"]])

res_sex_age; res_age10; res_race; res_dd

write_csv(res_sex_age, "output/09_sex_age_interaction.csv")
write_csv(res_age10,   "output/09_age_decades.csv")
write_csv(res_race,    "output/09_race_adjusted.csv")
write_csv(res_dd,      "output/09_dumouchel_duncan.csv")
