# R/05_estimates.R — weighted estimates and design-vs-naive comparisons
# Depends on: R/03_design.R (diet_design)

library(survey)

# Mean day-1 energy intake
svymean(~DR1TKCAL, diet_design, na.rm = TRUE)                      # 2075.8 (SE 13.88)
confint(svymean(~DR1TKCAL, diet_design, na.rm = TRUE))             # default: normal
confint(svymean(~DR1TKCAL, diet_design, na.rm = TRUE),
        df = degf(diet_design))                                    # correct: t, 15 df
mean(merged_all$DR1TKCAL, na.rm = TRUE)                            # unweighted: 1985.2

# Energy intake by sex
svyby(~DR1TKCAL, ~RIAGENDR, diet_design, svymean, na.rm = TRUE)
svyttest(DR1TKCAL ~ RIAGENDR, diet_design)                         # -546.0, df 14
t.test(DR1TKCAL ~ RIAGENDR, data = merged_all)                     # -493.0, df 6569

# Energy intake by age group
# NOTE: age_grp is created here, not in 02_merge.R, so it is not in the
# saved .rds. Re-running 03_design.R without this block loses the column.
merged_all <- merged_all %>%
  mutate(age_grp = case_when(
    RIDAGEYR < 6  ~ "0-5",
    RIDAGEYR < 20 ~ "6-19",
    RIDAGEYR < 65 ~ "20-64",
    TRUE          ~ "65+"
  ))

svyby(~DR1TKCAL, ~age_grp, diet_design, svymean, na.rm = TRUE)
# 0-5: 1425.5 (27.4) | 6-19: 1990.3 (19.4) | 20-64: 2206.2 (20.2) | 65+: 1955.4 (48.3)