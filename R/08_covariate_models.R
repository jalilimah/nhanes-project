# 08_covariate_models.R — covariate mixed models: lmer vs svylme
# Adults ≥ 20, two-day recall (long_adults from 06_long.R)
# Model: kcal ~ sex + centred age + day + (1 | SEQN)

library(tidyverse)
library(lme4)
library(WeMix)
library(survey)
library(svylme)

long_adults <- readRDS("data/processed/long_adults.rds") %>%
  mutate(w_norm = WTDR2D / mean(WTDR2D),       # rescaled weights (misapplied lmer)
         w_day  = 1,                           # conditional day-level weight (WeMix)
         age_c  = RIDAGEYR - mean(RIDAGEYR),   # centred at unweighted sample mean (51.36)
         day_f  = factor(day))                 # day 2 vs day 1

# 1. Plain lmer — no weights, no design
m1_lmer <- lmer(kcal ~ RIAGENDR + age_c + day_f + (1 | SEQN), data = long_adults)

# 2. svylme — pairwise likelihood, person as sampling unit
#    Design built after mutate() so it carries age_c and day_f
#    Warning "assuming no subsampling within PSUs" is expected: both days come
#    with each sampled person (complete pairs, 06_long.R)
des_person <- svydesign(ids = ~SEQN, strata = ~SDMVSTRA,
                        weights = ~WTDR2D, data = long_adults)
m1_svylme <- svy2lme(kcal ~ RIAGENDR + age_c + day_f + (1 | SEQN),
                     design = des_person, return.devfun = TRUE)

# 3. Design-based SE: bootstrap over PSUs within strata (as in 07)
set.seed(2017)
des_psu <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA,
                     weights = ~WTDR2D, nest = TRUE, data = long_adults)
rep_des <- as.svrepdesign(des_psu, type = "subbootstrap", replicates = 500)
m1_boot <- boot2lme(m1_svylme, rep_des)

# 4. Cross-check: marginal svyglm, person vs PSU clustering
#    Bootstrap SEs should match PSU linearisation SEs
g_person <- svyglm(kcal ~ RIAGENDR + age_c + day_f, design = des_person)
g_psu    <- svyglm(kcal ~ RIAGENDR + age_c + day_f, design = des_psu)

# 5. Balanced complete pairs: mixed-model fixed effects equal OLS
#    lmer = unweighted lm; svylme = svyglm (weighted)
fe_check <- cbind(svyglm = coef(g_person), svylme = coef(m1_svylme),
                  lm     = coef(lm(kcal ~ RIAGENDR + age_c + day_f, data = long_adults)),
                  lmer   = fixef(m1_lmer))
stopifnot(all.equal(fe_check[, "svyglm"], fe_check[, "svylme"]),
          all.equal(fe_check[, "lm"],     fe_check[, "lmer"]))

results_cov <- tibble(term       = names(fixef(m1_lmer)),
                      lmer       = fixef(m1_lmer),
                      lmer_SE    = sqrt(diag(vcov(m1_lmer))),
                      svylme     = coef(m1_svylme),
                      svylme_SE  = sqrt(diag(vcov(m1_svylme))),
                      boot_SE    = sqrt(diag(vcov(m1_boot))),
                      psu_lin_SE = SE(g_psu))

results_cov
write_csv(results_cov, "output/08_covariate_models_results.csv")
