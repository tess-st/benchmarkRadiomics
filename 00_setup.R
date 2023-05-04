# Install and load all necessary packages

# necessary since mlr3proba is removed from CRAN
# devtools::install_github("mlr-org/mlr3proba")
# devtools::install_github("alan-turing-institute/distr6")

PACKAGES = c(
  "mlr3learners",
  "mlr3",
  "mlr3extralearners",
  "mlr3proba",
  "mlr3pipelines",
  "mlr3filters",
  "praznik",
  "FSelectorRcpp",
  "mlr3tuning",
  # "mlr3hyperband",
  "bbotk",
  # "mlrintermbo",
  "mlr3misc",
  "paradox",
  # "batchtools",
  "dplyr",
  # "mlr3batchmark",
  "data.table"
)

# load all required packages
sapply(PACKAGES, require, character.only = TRUE)
