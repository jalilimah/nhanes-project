# R/03_design.R — build survey design objects
# Input:  data/processed/merged_all.rds (02_merge.R)
# Output: data/processed/diet_design.rds, kcal_design.rds (read by 05_estimates.R)

library(survey)
options(survey.lonely.psu = "adjust")

merged_all <- readRDS("data/processed/merged_all.rds")

diet_design <- svydesign(
  ids     = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTDRD1,
  nest    = TRUE,
  data    = merged_all
)

# Domain subset: exclude the 157 breastfed infants (DR1DRSTZ = "Reported
# consuming breast-milk"). These carry a valid positive dietary weight —
# the recall succeeded — but breast milk is not quantified, so DR1TKCAL is
# NA by design. Structural, not nonresponse. Excluding them redefines the
# target population as "non-breastfed", which must be stated explicitly.
# Subset the DESIGN, never the data — subsetting the data would drop strata
# and shrink the design df, silently narrowing every standard error.
# Domain = records with a kcal total. Among positive-weight records this excludes
# exactly the 157 breastfed infants; the zero-weight NA records also dropped
# (non-attendees, recall not done/unreliable) contribute nothing to estimates.
stopifnot(sum(merged_all$WTDRD1 > 0 & is.na(merged_all$DR1TKCAL)) == 157)
kcal_design <- subset(diet_design, !is.na(DR1TKCAL))

saveRDS(diet_design, "data/processed/diet_design.rds")
saveRDS(kcal_design, "data/processed/kcal_design.rds")