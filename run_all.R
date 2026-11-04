# run_all.R — reproduce the full pipeline from a fresh R session
# Each script is standalone (reads its inputs from data/ or output/), so they can
# also be run individually. 01 needs internet; 07, 08 and 10 take several minutes.
# 10 reuses output/10_sim_res_R1000.rds if present (set rerun <- TRUE inside to re-simulate).

scripts <- c("01_download", "02_merge", "03_design", "04_design_2d", "05_estimates",
             "06_long", "07_null_models", "08_covariate_models",
             "09_weight_diagnostics", "10_simulation")

for (s in scripts) {
  message("── Running ", s, " ──")
  t0 <- Sys.time()
  source(file.path("R", paste0(s, ".R")), local = new.env())   # fresh env: no leakage between scripts
  message("   done in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
}