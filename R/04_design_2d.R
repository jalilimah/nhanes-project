# 04_design_2d.R — two-day dietary design object
# Depends on: merged_all (from 02_merge.R)

library(survey)

options(survey.lonely.psu = "adjust")

diet_design_2d <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTDR2D,
  nest    = TRUE,
  data    = merged_all
)

degf(diet_design_2d)
