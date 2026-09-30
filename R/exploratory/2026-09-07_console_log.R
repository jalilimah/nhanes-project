source("R/02_merge.R")
merged_all <- readRDS("data/processed/merged_all.rds")
source("R/04_design_2d.R")
degf(diet_design_2d)
sum(weights(diet_design_2d) > 0)

demo_j %>%
  left_join(diet1_j, by = "SEQN") %>%
  summarise(na_2d_with_pos_1d = sum(is.na(WTDR2D) & WTDRD1 > 0, na.rm = TRUE))

demo_j %>%
  left_join(diet1_j, by = "SEQN") %>%
  summarise(exact_match = all(is.na(WTDR2D) == !(WTDRD1 > 0 & !is.na(WTDRD1))))

merged_all %>%
  filter(WTDRD1 > 0, DR1TKCAL == 0) %>%
  select(SEQN, RIDAGEYR, RIAGENDR, DR1DRSTZ, DR1TKCAL, WTDRD1)

merged_all %>%
  filter(WTDRD1 > 0, !is.na(DR1TKCAL)) %>%
  summarise(
    kcal_0    = sum(DR1TKCAL == 0),
    under_100 = sum(DR1TKCAL < 100),
    under_500 = sum(DR1TKCAL < 500),
    n         = n()
  )

merged_all %>% filter(WTDRD1 > 0, DR1TKCAL < 500, WTDR2D > 0) %>% select(SEQN, DR1TKCAL, DR2TKCAL) %>% head(15)


grep("^DR2", names(merged_all), value = TRUE)

diet2_j <- nhanes("DR2TOT_J")
dim(diet2_j)

sum(diet2_j$SEQN %in% merged_all$SEQN)

"DR2TKCAL" %in% names(diet2_j)
sum(!is.na(diet2_j$DR2TKCAL))

diet2_j %>%
  left_join(select(merged_all, SEQN, RIDAGEYR, WTDR2D), by = "SEQN") %>%
  filter(WTDR2D > 0, is.na(DR2TKCAL)) %>%
  summarise(
    n         = n(),
    max_age   = max(RIDAGEYR),
    under_2   = sum(RIDAGEYR < 2),
    age_2plus = sum(RIDAGEYR >= 2)
  )

diet2_j %>%
  left_join(select(merged_all, SEQN, RIDAGEYR), by = "SEQN") %>%
  filter(WTDR2D > 0, is.na(DR2TKCAL)) %>%
  summarise(
    n         = n(),
    max_age   = max(RIDAGEYR),
    under_2   = sum(RIDAGEYR < 2),
    age_2plus = sum(RIDAGEYR >= 2)
  )

diet2_j %>%
  left_join(select(merged_all, SEQN, RIDAGEYR), by = "SEQN") %>%
  filter(WTDR2D > 0, is.na(DR2TKCAL), RIDAGEYR >= 2) %>%
  select(SEQN, RIDAGEYR, DR2DRSTZ, DR2EXMER, DR2TKCAL, WTDR2D)

merged_all %>%
  filter(WTDRD1 > 0, is.na(DR1TKCAL)) %>%
  count(DR1DRSTZ)