library(nhanesA)
library(tidyverse)

demo  <- nhanes("DEMO_J")     # demographics
diet1 <- nhanes("DR1TOT_J")   # day 1 dietary recall
dim(demo)
dim(diet1)

head(demo)

summary(diet1$DR1TKCAL)     # total calories, day 1
table(diet1$DR1DRSTZ)       # was the recall complete?