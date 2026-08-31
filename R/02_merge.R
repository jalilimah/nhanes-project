# Merge DEMO_J with day 1 dietary recall
# Left join onto DEMO: never inner_join before building the survey design
library(nhanesA)
library(tidyverse)

demo_j  <- nhanes("DEMO_J")
diet1_j <- nhanes("DR1TOT_J")

merged_all <- demo_j %>%
  left_join(diet1_j, by = "SEQN")

# Diagnostic only: shows what an inner_join would cost (550 people)
merged_diet <- demo_j %>%
  inner_join(diet1_j, by = "SEQN")

# Dietary recall nonresponse by age: mild U-shape, 5.0% to 7.8%
merged_all %>%
  mutate(has_recall = !is.na(WTDRD1),
         age_grp = cut(RIDAGEYR, c(0, 5, 20, 60, 80),
                       include.lowest = TRUE)) %>%
  count(age_grp, has_recall) %>%
  group_by(age_grp) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  filter(!has_recall)

# WTDRD1 is NA for the 550 DEMO_J participants who never attended the MEC
# and so have no dietary recall. Recoded to 0 rather than dropped: zero-weight
# cases must stay in the design so the PSU/strata structure stays intact.
# They contribute nothing to estimates but keep the variance structure correct.
# NOTE: must run AFTER the nonresponse diagnostic above, which tests is.na().
merged_all$WTDRD1[is.na(merged_all$WTDRD1)] <- 0

saveRDS(merged_all, "data/processed/merged_all.rds")