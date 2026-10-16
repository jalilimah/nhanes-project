# 06_long.R — long format for mixed models: one row per adult per recall day
# Adults ≥ 20 with a positive two-day weight (WTDR2D > 0)
# Input:  data/processed/merged_all.rds (02_merge.R)
# Output: data/processed/long_adults.rds (read by 07 and 08)

library(tidyverse)

merged_all <- readRDS("data/processed/merged_all.rds")

long_adults <- merged_all %>%
  filter(WTDR2D > 0, RIDAGEYR >= 20) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTDR2D, RIAGENDR, RIDAGEYR,
         DR1TKCAL, DR2TKCAL) %>%
  pivot_longer(c(DR1TKCAL, DR2TKCAL),
               names_to = "day", values_to = "kcal") %>%
  mutate(day = if_else(day == "DR1TKCAL", 1L, 2L))

# Complete pairs: every person has exactly two rows and intake on both days.
# 07/08 rely on this (balanced pairs => mixed-model fixed effects equal OLS).
stopifnot(all(table(long_adults$SEQN) == 2),
          !anyNA(long_adults$kcal))

saveRDS(long_adults, "data/processed/long_adults.rds")
