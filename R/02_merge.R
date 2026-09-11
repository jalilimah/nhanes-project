
# Merge DEMO_J with day 1 dietary recall
# Left join onto DEMO: never inner_join before building the survey design
# Depends on: R/01_download.R (demo_j, diet1_j)

merged_all <- demo_j %>%
  left_join(diet1_j, by = "SEQN")

# Diagnostic only: shows what an inner_join would cost (550 people)
merged_diet <- demo_j %>%
  inner_join(diet1_j, by = "SEQN")


# Both dietary weights are recoded from NA to 0 rather than dropped:
# zero-weight cases must stay in the design so the PSU/strata structure
# stays intact. They contribute nothing to estimates but keep the
# variance structure correct.
#
# The two NA patterns arise from different mechanisms:
#   WTDRD1 — 550 NAs, all DEMO_J participants who never attended the MEC.
#            MEC attendees without a usable day-1 recall (976 not done,
#            87 unreliable) already have an exact zero from NCHS.
#   WTDR2D — 1,613 NAs = everyone outside the day-1 analytic sample
#            (550 non-attendees + 976 not done + 87 unreliable).
#            Day-2 attrition is the 1,002 exact zeros, coded by NCHS.
#            6,639 positive. Total: 1,613 + 1,002 + 6,639 = 9,254.

merged_all$WTDRD1[is.na(merged_all$WTDRD1)] <- 0
merged_all$WTDR2D[is.na(merged_all$WTDR2D)] <- 0

saveRDS(merged_all, "data/processed/merged_all.rds")