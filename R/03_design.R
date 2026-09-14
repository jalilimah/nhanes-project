# R/03_design.R — build survey design objects
# Depends on: R/02_merge.R (merged_all)

library(survey)
options(survey.lonely.psu = "adjust")

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
kcal_design <- subset(diet_design, !(WTDRD1 > 0 & is.na(DR1TKCAL)))