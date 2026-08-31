# R/01_download.R — fetch NHANES 2017-2018 tables
# NOTE: object named demo_j, not demo. A bare `demo` masks utils::demo().

library(nhanesA)
library(tidyverse)

demo_j  <- nhanes("DEMO_J")     # demographics
diet1_j <- nhanes("DR1TOT_J")   # day 1 dietary recall

dim(demo_j)
dim(diet1_j)
head(demo_j)
summary(diet1_j$DR1TKCAL)     # total calories, day 1
table(diet1_j$DR1DRSTZ)       # recall status — note the 157 breast-milk cases