# R/05_estimates.R — weighted estimates and design-vs-naive comparisons
# Input: data/processed/kcal_design.rds, diet_design.rds (03_design.R), merged_all.rds (02_merge.R)
# Target population: records with a day-1 kcal total (157 breastfed infants excluded in 03).
# subset() keeps excluded rows at zero weight (preserving design df), so NA kcal values
# remain in kcal_design and na.rm = TRUE is still required.

library(survey)
options(survey.lonely.psu = "adjust")

merged_all  <- readRDS("data/processed/merged_all.rds")
diet_design <- readRDS("data/processed/diet_design.rds")
kcal_design <- readRDS("data/processed/kcal_design.rds")

# The domain and na.rm on the full design give the same estimate and the same df
stopifnot(all.equal(svymean(~DR1TKCAL, kcal_design, na.rm = TRUE),
                    svymean(~DR1TKCAL, diet_design, na.rm = TRUE)),
          degf(kcal_design) == degf(diet_design))

# Mean day-1 energy intake
m_kcal <- svymean(~DR1TKCAL, kcal_design, na.rm = TRUE)            # 2075.8 (SE 13.88)
confint(m_kcal)                                                    # default: normal
confint(m_kcal, df = degf(kcal_design))                            # correct: t, 15 df
mean(merged_all$DR1TKCAL, na.rm = TRUE)                            # unweighted: 1985.2

# Energy intake by sex
svyby(~DR1TKCAL, ~RIAGENDR, kcal_design, svymean, na.rm = TRUE)
svyttest(DR1TKCAL ~ RIAGENDR, kcal_design)                         # -546.0, df 14
t.test(DR1TKCAL ~ RIAGENDR, data = merged_all)                     # -493.0, df 6569

# Energy intake by age