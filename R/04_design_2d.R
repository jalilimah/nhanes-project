# R/04_design_2d.R — two-day dietary design object (reference only)
# Full-sample design with the two-day weight WTDR2D. Not used downstream:
# 07-09 build their own designs from long_adults (adults, long format).
# Kept to document the two-day design and its degrees of freedom.
# Input: data/processed/merged_all.rds (02_merge.R)

library(survey)
options(survey.lonely.psu = "adjust")

merged_all <- readRDS("data/processed/merged_all.rds")

diet_design_2d <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTDR2D,
  nest    = TRUE,
  data    = merged_all
)

degf(diet_design_2d)   # 15, same as the day-1 design