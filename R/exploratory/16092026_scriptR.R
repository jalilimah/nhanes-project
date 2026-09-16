getwd()

library(survey)
svymean(~DR1TKCAL, diet_design, na.rm = TRUE)

source("R/01_download.R")
source("R/02_merge.R")
source("R/03_design.R")
source("R/04_design_2d.R")

confint(svymean(~DR1TKCAL, diet_design, na.rm = TRUE))

confint(svymean(~DR1TKCAL, diet_design, na.rm = TRUE), df = degf(diet_design))

mean(merged_all$DR1TKCAL, na.rm = TRUE)

svyby(~DR1TKCAL, ~RIAGENDR, diet_design, svymean, na.rm = TRUE)

svyttest(DR1TKCAL ~ RIAGENDR, diet_design)

t.test(DR1TKCAL ~ RIAGENDR, data = merged_all)

file.edit("R/05_estimates.R")

svyby(~DR1TKCAL, ~RIDRETH3, diet_design, svymean, na.rm = TRUE)