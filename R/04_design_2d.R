list.files()
list.files("R")
list.files("data/processed")
library(survey)
library(tidyverse)
merged_all <- readRDS("data/processed/merged_all.rds")
dim(merged_all)

sum(is.na(merged_all$WTDRD1))

table(merged_all$DR1TKCAL == 0, useNA = "ifany")

summary(merged_all$WTDR2D)
sum(merged_all$WTDR2D > 0, na.rm = TRUE)

# who is the zero-kcal person?
merged_all[which(merged_all$DR1TKCAL == 0),
           c("SEQN", "RIDAGEYR", "DR1DRSTZ", "WTDRD1", "WTDR2D")]

# prediction: this should return 7641
sum(merged_all$WTDRD1 > 0)

# 04_design_2d.R — two-day dietary design object
# Depends on: merged_all (from 02_merge.R)

library(survey)

options(survey.lonely.psu = "adjust")

merged_all$WTDR2D[is.na(merged_all$WTDR2D)] <- 0

diet_design_2d <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTDR2D,
  nest    = TRUE,
  data    = merged_all
)

degf(diet_design_2d)