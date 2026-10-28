# 10_simulation.R — four estimators vs known truth (unbalanced pairs, informative weights)
library(tidyverse); library(lme4); library(WeMix); library(survey); library(svylme); library(furrr)
long_adults <- readRDS("data/processed/long_adults.rds")
set.seed(2026)

# 1. Population: 15 strata x 20 PSUs; subgroup g (25%) has steeper age slope
N <- 100000
pop <- tibble(id      = 1:N,
              female  = rbinom(N, 1, 0.5),
              age     = sample(long_adults$RIDAGEYR, N, replace = TRUE),
              g       = rbinom(N, 1, 0.25),
              u       = rnorm(N, 0, 620),
              stratum = sample(rep(1:15, length.out = N)),
              psu_id  = stratum * 100 + sample(1:20, N, replace = TRUE)) %>%
  mutate(age_c = age - 51.36, slope = if_else(g == 1, -12, -2))
pop_long <- pop %>% crossing(day = 1:2) %>%
  mutate(kcal = 2473 - 619 * female + slope * age_c - 166 * (day == 2) +
           u + rnorm(n(), 0, 696))

terms <- c("(Intercept)", "female", "age_c", "day_f2")
truth <- tibble(term = terms,
                truth = unname(coef(lm(kcal ~ female + age_c + factor(day), data = pop_long))))
m_pop <- lmer(kcal ~ female + age_c + factor(day) + (1 | id), data = pop_long)
truth <- bind_rows(truth,
                   tibble(term = c("sigma2_u", "sigma2_e"),
                          truth = as.data.frame(VarCorr(m_pop))$vcov))
# 2-3. One sample: 2 PSUs per stratum, g oversampled 3x, 12% lose day 2
draw_sample <- function() {
  chosen  <- pop %>% distinct(stratum, psu_id) %>% group_by(stratum) %>%
    slice_sample(n = 2) %>% ungroup()
  persons <- pop %>% filter(psu_id %in% chosen$psu_id) %>%
    mutate(p_in = if_else(g == 1, 0.81, 0.27)) %>%
    filter(runif(n()) < p_in) %>%
    mutate(w = 1 / (0.1 * p_in))                     # PSU prob 2/20 x person prob
  drop2 <- persons$id[runif(nrow(persons)) < 0.12]
  pop_long %>% inner_join(select(persons, id, w), by = "id") %>%
    filter(!(day == 2 & id %in% drop2)) %>%
    mutate(day_f = factor(day), w_norm = w / mean(w), w1 = 1,
           kcal_k = kcal / 1000, age_c10 = age_c / 10)
}

# 4. Fit the four estimators
fit_all <- function(s) {
  f    <- kcal ~ female + age_c + day_f + (1 | id)
  m_l  <- lmer(f, data = s)
  m_wl <- lmer(f, data = s, weights = w_norm)       # misapplied weights
  m_wm <- mix(kcal_k ~ female + age_c10 + day_f + (1 | id), data = s,
              weights = c("w1", "w"), cWeights = TRUE)
  des  <- svydesign(ids = ~id, strata = ~stratum, weights = ~w, data = s)
  m_sv <- suppressWarnings(svy2lme(f, design = des))
  sc   <- c(1000, 1000, 100, 1000)
  # NEW: variance components; svylme L is the ratio sigma2_u / sigma2_e
  vc <- tibble(est  = rep(c("lmer", "weighted lmer", "WeMix", "svylme"), each = 2),
               term = rep(c("sigma2_u", "sigma2_e"), 4),
               b    = c(as.data.frame(VarCorr(m_l))$vcov,
                        as.data.frame(VarCorr(m_wl))$vcov,
                        unname(m_wm$vars) * 1e6,
                        as.numeric(m_sv$s2) * c(as.numeric(m_sv$L), 1)),
               se   = NA_real_)
  bind_rows(
    tibble(est = "lmer",          term = terms, b = fixef(m_l),  se = sqrt(diag(as.matrix(vcov(m_l))))),
    tibble(est = "weighted lmer", term = terms, b = fixef(m_wl), se = sqrt(diag(as.matrix(vcov(m_wl))))),
    tibble(est = "WeMix",         term = terms, b = unname(coef(m_wm)[1:4] * sc),
           se = unname(summary(m_wm)$coef[1:4, "Std. Error"] * sc)),
    tibble(est = "svylme",        term = terms, b = coef(m_sv),  se = sqrt(diag(vcov(m_sv)))),
    vc)                                             # NEW: append vc rows
}

# 5. Replicate (set R <- 8 for a timing test; R <- 1000 for the full run)
R <- 8
plan(multisession, workers = 4)
print(system.time(
  res <- future_map_dfr(1:R, function(r) {
    out <- tryCatch(fit_all(draw_sample()), error = function(e) NULL)   # failed fit -> dropped, counted in n_ok
    if (!is.null(out)) mutate(out, rep = r)
  }, .options = furrr_options(seed = TRUE,
                              packages = c("tidyverse", "lme4", "WeMix", "survey", "svylme")))
))
plan(sequential)
saveRDS(res, sprintf("output/sim_res_R%d_%s.rds", R, Sys.Date()))

# 6. Compare with truth
sim_summary <- res %>% left_join(truth, by = "term") %>%
  group_by(est, term) %>%
  summarise(n_ok     = n(),
            bias     = mean(b - truth),
            rel_bias = bias / first(truth),
            MCSE     = sd(b) / sqrt(n()),
            emp_SE   = sd(b),
            SE_ratio = mean(se) / sd(b),
            coverage = mean(abs(b - truth) < 1.96 * se), .groups = "drop")
print(truth)
print(sim_summary, n = Inf)

