# R/01_download.R — fetch NHANES 2017-2018 tables
# NOTE: object named demo_j, not demo. A bare `demo` masks utils::demo().
#
# Expected sizes: demo_j 9,254 rows; diet1_j and diet2_j 8,704 rows each.
# DR1DRSTZ includes 157 breast-milk cases: positive weight, no kcal total.

library(nhanesA)
library(tidyverse)

demo_j  <- nhanes("DEMO_J")     # demographics
diet1_j <- nhanes("DR1TOT_J")   # day 1 dietary recall
diet2_j <- nhanes("DR2TOT_J")   # day 2 dietary recall