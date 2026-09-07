
# Merge DEMO_J with day 1 dietary recall
# Left join onto DEMO: never inner_join before building the survey design
# Depends on: R/01_download.R (demo_j, diet1_j)

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

# Both dietary weights are NA for participants with no corresponding recall.
# Recoded to 0 rather than dropped: zero-weight cases must stay in the design
# so the PSU/strata structure stays intact. They contribute nothing to
# estimates but keep the variance structure correct.
#
# The two NA patterns arise from different mechanisms:
#   WTDRD1 — 550 NAs, all DEMO_J participants who never attended the MEC.
#            Non-response is inherited from the examination stage, not dietary.
#   WTDR2D — 1,613 NAs, a superset: MEC non-attendance plus day-2 recall
#            attrition. A further 1,002 have an exact zero rather than NA,
#            already coded by NCHS as day-2 nonresponse. 6,639 positive.
#
# Same operation, same design-structure justification, different populations.
# NOTE: must run AFTER the nonresponse diagnostic above, which tests is.na().
merged_all$WTDRD1[is.na(merged_all$WTDRD1)] <- 0
merged_all$WTDR2D[is.na(merged_all$WTDR2D)] <- 0

saveRDS(merged_all, "data/processed/merged_all.rds")