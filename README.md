# Habitual Dietary Intake from NHANES 24-Hour Recalls
Complex survey designs and mixed-effects models rest on incompatible foundations — design-based and model-based inference respectively — and no fully satisfactory reconciliation exists.
This repository documents a simulation study of that problem, using the two-day dietary recall structure of NHANES 2017–2018 as a demonstration example.



**Data:** NHANES 2017–2018 (DEMO_J, DR1TOT_J, DR2TOT_J, BMX_J)
**Status:** In development

## Reproducing

Raw data is not tracked in this repo. Run `R/01-download.R`
to fetch the NHANES files from CDC into `data/raw/`.
