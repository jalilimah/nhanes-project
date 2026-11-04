# R/01_download.R — fetch NHANES 2017-2018 tables and save raw copies
# NOTE: object named demo_j, not demo. A bare `demo` masks utils::demo().
#
# Expected sizes: demo_j 9,254 rows; diet1_j and diet2_j 8,704 rows each.
# DR1DRSTZ includes 157 breast-milk cases: positive weight, no kcal total.
# Output: data/raw/{demo_j,diet1_j,diet2_j}.rds (read by 02_merge.R)

library(nhanesA)
library(tidyverse)

demo_j  <- nhanes("DEMO_J")     # demographics
diet1_j <- nhanes("DR1TOT_J")   # day 1 dietary recall
diet2_j <- nhanes("DR2TOT_J")   # day 2 dietary recall

stopifnot(nrow(demo_j) == 9254, nrow(diet1_j) == 8704, nrow(diet2_j) == 8704)

dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
saveRDS(demo_j,  "data/raw/demo_j.rds")
saveRDS(diet1_j, "data/raw/diet1_j.rds")
saveRDS(diet2_j, "data/raw/diet2_j.rds")