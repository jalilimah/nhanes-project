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

merged_all <- merged_all %>%
  mutate(age_grp = case_when(
    RIDAGEYR <= 5  ~ "0-5",
    RIDAGEYR <= 19 ~ "6-19",
    RIDAGEYR <= 64 ~ "20-64",
    TRUE           ~ "65+"
  ))

table(merged_all$age_grp, useNA = "ifany")

"age_grp" %in% names(diet_design$variables)

svyby(~DR1TKCAL, ~age_grp, diet_design, svymean, na.rm = TRUE)

file.edit("R/05_estimates.R")

table(merged_all$DRDINT, useNA = "ifany")

long <- merged_all %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTDR2D, RIAGENDR, RIDAGEYR, age_grp, DR1TKCAL) %>%
  left_join(diet2_j %>% select(SEQN, DR2TKCAL), by = "SEQN") %>%
  pivot_longer(c(DR1TKCAL, DR2TKCAL), names_to = "day", values_to = "kcal")

nrow(long)

long %>% filter(SEQN == 93703) %>% select(SEQN, day, kcal, WTDR2D)

long %>% filter(SEQN == 93711) %>% select(SEQN, day, kcal, WTDR2D)

long %>% count(has_kcal = !is.na(kcal), positive_wt = WTDR2D > 0)

install.packages("lme4")

library(lme4)
m1 <- lmer(kcal ~ day + (1 | SEQN), data = long)
summary(m1)

nobs(m1)

long %>% filter(!is.na(kcal)) %>% nrow()

weighted
m2 <- lmer(kcal ~ day + (1 | SEQN), data = long, weights = WTDR2D)
fixef(m2)

summary(m2)$coefficients

library(tidyverse)

diet_long <- diet_design_2d$variables %>%
  select(SEQN, DR1TKCAL, DR2TKCAL) %>%
  pivot_longer(
    cols      = c(DR1TKCAL, DR2TKCAL),
    names_to  = "day",
    values_to = "kcal"
  ) %>%
  mutate(day = if_else(day == "DR1TKCAL", 1L, 2L))

dim(diet_long)
sum(is.na(diet_long$kcal))
n_distinct(diet_long$SEQN)


source("R/02_merge.R")
source("R/03_design.R")
source("R/04_design_2d.R")
sum(weights(diet_design_2d) > 0)

getwd()
list.files(recursive = TRUE, pattern = "\\.R$")

source("R/02_merge.R")
source("R/03_design.R")
source("R/04_design_2d.R")
sum(weights(diet_design_2d) > 0)

diet_long <- diet_design_2d$variables %>%
  select(SEQN, DR1TKCAL, DR2TKCAL) %>%
  pivot_longer(cols = c(DR1TKCAL, DR2TKCAL), names_to = "day", values_to = "kcal") %>%
  mutate(day = if_else(day == "DR1TKCAL", 1L, 2L))

dim(diet_long)
diet_long %>% count(day, missing = is.na(kcal))

grep("^DR2", names(diet_design_2d$variables), value = TRUE)

exists("dr2tot_j")

grep("^DR2", names(merged_all), value = TRUE)

grep("^DR2", names(diet2_j), value = TRUE) %>% head(20)

length(intersect(merged_all$SEQN, diet2_j$SEQN))
nrow(merged_all)

long_kcal <- merged_all %>%
  filter(WTDR2D > 0) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTDR2D, RIAGENDR, RIDAGEYR,
         DR1TKCAL, DR2TKCAL) %>%
  pivot_longer(c(DR1TKCAL, DR2TKCAL),
               names_to = "day", values_to = "kcal") %>%
  mutate(day = if_else(day == "DR1TKCAL", 1L, 2L))

nrow(long_kcal)
n_distinct(long_kcal$SEQN)
sum(is.na(long_kcal$kcal))

long_kcal <- merged_all %>%
  filter(WTDR2D > 0) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTDR2D, RIAGENDR, RIDAGEYR,
         DR1TKCAL, DR2TKCAL) %>%
  pivot_longer(c(DR1TKCAL, DR2TKCAL),
               names_to = "day", values_to = "kcal") %>%
  mutate(day = if_else(day == "DR1TKCAL", 1L, 2L))

nrow(long_kcal)
n_distinct(long_kcal$SEQN)
sum(is.na(long_kcal$kcal))

long_kcal %>% filter(is.na(kcal)) %>% count(day)

na_day1 <- long_kcal %>% filter(day == 1, is.na(kcal)) %>% pull(SEQN)

merged_all %>%
  filter(SEQN %in% na_day1) %>%
  count(DRABF, DR1DRSTZ, age_under2 = RIDAGEYR < 2)

long_adults <- long_kcal %>% filter(RIDAGEYR >= 20)

n_distinct(long_adults$SEQN)
long_adults %>% filter(is.na(kcal)) %>% count(day)

na_day2 <- long_kcal %>% filter(day == 2, is.na(kcal)) %>% pull(SEQN)

length(na_day2)
sum(na_day2 %in% na_day1)

merged_all %>%
  filter(SEQN %in% setdiff(na_day2, na_day1)) %>%
  select(SEQN, RIDAGEYR, DRABF, DR1DRSTZ, DR2DRSTZ)

# 05_long.R — long format (person × day) for mixed models
# Adults ≥ 20 with positive two-day weight; all missing kcal are breastfed children

library(tidyverse)

merged_all <- readRDS("data/processed/merged_all.rds")

long_adults <- merged_all %>%
  filter(WTDR2D > 0, RIDAGEYR >= 20) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WTDR2D, RIAGENDR, RIDAGEYR,
         DR1TKCAL, DR2TKCAL) %>%
  pivot_longer(c(DR1TKCAL, DR2TKCAL),
               names_to = "day", values_to = "kcal") %>%
  mutate(day = if_else(day == "DR1TKCAL", 1L, 2L))

saveRDS(long_adults, "data/processed/long_adults.rds")

nrow(long_adults)
sum(is.na(long_adults$kcal))

source("R/05_long.R")
nrow(long_adults)
sum(is.na(long_adults$kcal))

library(lme4)

m0 <- lmer(kcal ~ 1 + (1 | SEQN), data = long_adults)

summary(m0)

m0_w <- lmer(kcal ~ 1 + (1 | SEQN), data = long_adults, weights = WTDR2D)

summary(m0_w)

mean(long_adults$WTDR2D)
sigma(m0_w)^2 / mean(long_adults$WTDR2D)

sigma(m0_w)^2 * mean(1 / long_adults$WTDR2D)

range(long_adults$WTDR2D)
range(sigma(m0_w) / sqrt(long_adults$WTDR2D))

long_adults <- long_adults %>% mutate(w_norm = WTDR2D / mean(WTDR2D))

m0_wn <- lmer(kcal ~ 1 + (1 | SEQN), data = long_adults, weights = w_norm)

summary(m0_wn)

library(WeMix)

long_adults <- long_adults %>% mutate(w_day = 1)

m0_wemix <- mix(kcal ~ 1 + (1 | SEQN), data = long_adults,
                weights = c("w_day", "WTDR2D"), cWeights = TRUE)

summary(m0_wemix)

weighted.mean(long_adults$kcal, long_adults$WTDR2D)


library(survey)
library(svylme)

des_long <- svydesign(ids = ~SDMVPSU + SEQN, strata = ~SDMVSTRA,
                      weights = ~WTDR2D, nest = TRUE, data = long_adults)

m0_svylme <- svy2lme(kcal ~ 1 + (1 | SEQN), design = des_long)

m0_svylme

set.seed(2017)

des_psu <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA,
                     weights = ~WTDR2D, nest = TRUE, data = long_adults)

rep_des <- as.svrepdesign(des_psu, type = "subbootstrap", replicates = 100)

m0_boot <- boot2lme(m0_svylme, rep_des)

m0_boot