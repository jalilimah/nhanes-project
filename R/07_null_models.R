# 07_null_models.R — intercept-only mixed models, four estimators
# Adults ≥ 20, two-day recall (long_adults from 06_long.R)

library(tidyverse)
library(lme4)
library(WeMix)
library(survey)
library(svylme)

long_adults <- readRDS("data/processed/long_adults.rds") %>%
  mutate(w_norm = WTDR2D / mean(WTDR2D),   # rescaled weights (misapplied lmer)
         w_day  = 1)                       # conditional day-level weight (WeMix)

# 1. Plain lmer — no weights, no design
m0 <- lmer(kcal ~ 1 + (1 | SEQN), data = long_adults)

# 2. Misapplied weighted lmer — sampling weights read as precision weights
m0_w  <- lmer(kcal ~ 1 + (1 | SEQN), data = long_adults, weights = WTDR2D)  # warns
m0_wn <- lmer(kcal ~ 1 + (1 | SEQN), data = long_adults, weights = w_norm)  # silent

# 3. WeMix — weights at person level, day weight conditional = 1
m0_wemix <- mix(kcal ~ 1 + (1 | SEQN), data = long_adults,
                weights = c("w_day", "WTDR2D"), cWeights = TRUE)

# 4. svylme — pairwise likelihood, person as sampling unit
des_person <- svydesign(ids = ~SEQN, strata = ~SDMVSTRA,
                        weights = ~WTDR2D, data = long_adults)
m0_svylme <- svy2lme(kcal ~ 1 + (1 | SEQN), design = des_person,
                     return.devfun = TRUE)

# Design-based SE: bootstrap over PSUs within strata
set.seed(2017)
des_psu <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA,
                     weights = ~WTDR2D, nest = TRUE, data = long_adults)
rep_des <- as.svrepdesign(des_psu, type = "subbootstrap", replicates = 500)
m0_boot <- boot2lme(m0_svylme, rep_des)
vc <- function(m) as.data.frame(VarCorr(m))$sdcor

lmer_row <- function(m, name) {
  tibble(estimator = name,
         intercept = fixef(m)[[1]],
         SE        = sqrt(vcov(m)[1, 1]),
         sd_between = vc(m)[1],
         sd_within  = vc(m)[2])
}

sv_between <- as.numeric(sqrt(m0_svylme$s2 * as.vector(m0_svylme$L)[1]))
sv_within  <- as.numeric(sqrt(m0_svylme$s2))

results_null <- bind_rows(
  lmer_row(m0,    "lmer"),
  lmer_row(m0_w,  "lmer, raw weights"),
  lmer_row(m0_wn, "lmer, rescaled weights"),
  tibble(estimator  = "WeMix",
         intercept  = m0_wemix$coef[[1]],
         SE         = m0_wemix$SE[[1]],
         sd_between = sqrt(m0_wemix$vars[[1]]),
         sd_within  = sqrt(m0_wemix$vars[[2]])),
  tibble(estimator  = "svylme",
         intercept  = coef(m0_svylme)[[1]],
         SE         = sqrt(vcov(m0_svylme)[1, 1]),
         sd_between = sv_between,
         sd_within  = sv_within),
  tibble(estimator  = "svylme + PSU bootstrap",
         intercept  = coef(m0_svylme)[[1]],
         SE         = sqrt(vcov(m0_boot)[1, 1]),
         sd_between = sv_between,
         sd_within  = sv_within)
) %>%
  mutate(ICC = sd_between^2 / (sd_between^2 + sd_within^2))

results_null