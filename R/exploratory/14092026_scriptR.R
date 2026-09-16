source("R/01_download.R")
source("R/02_merge.R")
source("R/03_design.R")
source("R/04_design_2d.R")
degf(diet_design)
sum(weights(diet_design_2d) > 0)

length(readLines("07092026_scripts.R"))
cat(head(readLines("07092026_scripts.R"), 30), sep = "\n")

cat(tail(readLines("07092026_scripts.R"), 38), sep = "\n")
dir.exists("R/exploratory")

dir.create("R/exploratory")
file.rename("07092026_scripts.R", "R/exploratory/2026-09-07_console_log.R")
list.files()

file.info(list.files("data/processed", full.names = TRUE))["size"]