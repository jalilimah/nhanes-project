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