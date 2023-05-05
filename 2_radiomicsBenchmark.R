# Install and load all necessary packages

# necessary since mlr3proba is removed from CRAN
# devtools::install_github("mlr-org/mlr3proba")

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
  "mlr3misc",
  "paradox",
  # "batchtools",
  "dplyr",
  # "mlr3batchmark",
  "data.table"
)

# load all required packages
sapply(PACKAGES, require, character.only = TRUE)


###################################################
# Load and preprocess data set
###################################################

# breast cancer data set from mlr3proba
bc = mlr3proba::gbcs

# remove irrelevant variables
# analysis of overall survival -> remove recurrence variables
# rename time and event variable to
# as factor: menopause, hormone, grade
# death: event variable needs to be numeric instead of factor for survival
bc = bc %>%
  dplyr::select(-c("id", "diagdate", "recdate", "deathdate", "censrec", "rectime"))%>%
  rename("osdays" = "survtime", "death" = "censdead") %>%
  mutate_at(c("menopause", "hormone", "grade"), factor)

# name all radiomics and clinical datasets for the benchmark analyses here
TASK_IDS = c("bc")


###################################################
# Initialization of ML settings
###################################################

# Define measure(s) for tuning and evaluation
# "surv.graf" = integrated Brier-score (IBS)
TUNING_MEASURE = "surv.graf"
SCORE_MEASURES = c("surv.graf", "surv.cindex") # c-index actually not needed

# Learners
# provide the name of all learners with settings as names in a vector
LEARNER_IDS = c(
  "surv.cv_glmnet-tuned",
  "surv.cv_glmnet-untuned",
  "surv.cv_glmnet-pca-tuned",
  "surv.cv_glmnet-pca-untuned",
  "surv.rfsrc-tuned",
  "surv.rfsrc-untuned",
  "surv.rfsrc-pca-tuned",
  "surv.rfsrc-pca-untuned",
  "surv.xgboost-tuned",
  "surv.xgboost-untuned",
  "surv.xgboost-pca-tuned",
  "surv.xgboost-pca-untuned"
)

# set some default hyperparamter settings
LEARNER_PAR_VALUES = list(
  "surv.cv_glmnet" = list(parallel = TRUE),
  "surv.rfsrc" = list(ntree = 50L, na.action = "na.impute"), # sirflox: ntree  500L
  "surv.xgboost" = list(nrounds = 50L, verbose = 0)# sirflox: nrounds  500L
)

# define a fallback learner, such that benchmark does not throw an error and stops the benchmark experiment
FALLBACK_LEARNER_ID = "surv.kaplan"


###################################################
# Setup pipeline
###################################################

# Pipeline
getGraphLearner = function(learner_id, learner) {
  # PCA
  no_pca = po("nop", "no_pca")
  # pipe for pca, keep principal components explaining 90% of the variance (filter.frac)
  pca = po("pca", scale. = TRUE) %>>% po("filter",
    filter = mlr3filters::flt("variance", task_type = "surv"), filter.frac = 0.9)
  # define the two options pca vs. no pca
  pca_opts = list(PCA = pca, PCA_None = no_pca)
  # set up branch with pca options
  pca_branch = po("branch", id = "pca_branch",
    options = names(pca_opts)) %>>%
    gunion(pca_opts) %>>%
    po("unbranch", id = "pca_unbranch", options = names(pca_opts))

  # feature selection filters
  # correlation based
  fs_corr = po("filter", id = "corr_filter", mlr3filters::flt("find_correlation", task_type = "surv"),
    affect_columns = selector_type(c("numeric", "integer")))
  # based on feature importance of a rf
  fs_rfimp = po("filter", id = "rfimp_filter", mlr3filters::flt("importance", learner = lrn("surv.ranger",
    importance = "permutation")))
  # based on information gain
  fs_ig = po("filter", id = "ig_filter", mlr3filters::flt("information_gain", equal = TRUE, task_type = "surv"))
  # list all feature selection methods
  fs_opts = list(FS_Corr = fs_corr, FS_RFimp = fs_rfimp, FS_IG = fs_ig, FS_None = po("nop", "no_fs"))
  # define the feature selection branch
  fs_branch = po("branch", id = "fs_branch", options = names(fs_opts)) %>>%
    gunion(fs_opts) %>>%
    po("unbranch", id = "fs_unbranch", options = names(fs_opts))

  # combine the different pipelines in a graph models
  if (learner_id == "surv.cv_glmnet" || learner_id == "surv.xgboost") {
    return(
      as_learner(
        po("imputemode") %>>%
          po("encode") %>>%
          po("imputehist") %>>%
          po("removeconstants") %>>%
          po("fixfactors") %>>%
          fs_branch %>>%
          pca_branch %>>%
          po("copy", outnum = 2) %>>%
          gunion(list(po("learner", lrn("surv.kaplan")), learner)) %>>%
          po("compose_distr") # separate calculation of distr for glmnet and xgboost to calculate surv.graf -> need for if statement
      )
    )
  } else {
    return(
      as_learner(
        po("imputeoor") %>>%
          po("encode") %>>%
          po("imputehist") %>>%
          po("removeconstants") %>>%
          po("fixfactors") %>>%
          fs_branch %>>%
          pca_branch %>>%
          po("learner", learner) # returns distr anyway
      )
    )
  }
}

# Resampling for evaluation, 10-fold cross validation
RESAMPLING_OUTER = rsmp("cv", folds = 10L)

# Resampling for tuning, 5-fold cross validation
RESAMPLING_INNER = rsmp("cv", folds = 5L)


###################################################
# Setup tuner and terminator
###################################################

# Settings for tuners
ETA = 2
N_EVALS = 30L # sirflox: 200L

# tuner: for xgboost and rfsrc hyperband, in case of glmnet no meaningful eta definable
# -> use random search instead
getTuner = function(learner_id) {
  if (learner_id == "surv.cv_glmnet") {
    return(tnr("random_search"))
  }
  else {
    return(tnr("hyperband", eta = ETA))
  }
}

# set a terminator after N_EVALS rounds for cv_glmnet
getTerminator = function(learner_id) {
  if (learner_id == "surv.cv_glmnet") {
    return(trm("evals", n_evals = N_EVALS))
  }
  else {
    return(trm("none"))
  }
}


###################################################
# Function of extracting tuning params
###################################################

# function for extracting all tuning parameters depending on each learner
getTuningParams = function(learner_id, task) {
  if (learner_id == "surv.cv_glmnet") {
    search_space = ps(
      surv.cv_glmnet.alpha = p_dbl(lower = 0, upper = 1),
      fs_branch.selection = p_fct(levels = c("FS_Corr", "FS_RFimp", "FS_IG", "FS_None")), # necessary for tuning over fs methods
      FS_Corr.corr_filter.filter.cutoff = p_dbl(0.1, 0.9),
      FS_RFimp.rfimp_filter.filter.frac = p_dbl(0.1, 0.9),
      FS_IG.ig_filter.filter.frac = p_dbl(0.1, 0.9)
    )
  } else if (learner_id == "surv.rfsrc") {
    search_space = ps(
      surv.rfsrc.ntree = p_int(lower = 20L, upper = 200L, tags = "budget"),
      # sirflox: lower = 20L, upper = 1500L
      surv.rfsrc.mtry = p_int(lower = 3L, upper = ceiling((task$ncol)^(1/1.5))),
      surv.rfsrc.nodesize = p_int(lower = 1L, upper = task$nrow),
      fs_branch.selection = p_fct(levels = c("FS_Corr", "FS_RFimp", "FS_IG", "FS_None")),
      FS_Corr.corr_filter.filter.cutoff = p_dbl(0.1, 0.9),
      FS_RFimp.rfimp_filter.filter.frac = p_dbl(0.1, 0.9),
      FS_IG.ig_filter.filter.frac = p_dbl(0.1, 0.9)
    )
  } else if (learner_id == "surv.xgboost") {
    search_space = ps(
      surv.xgboost.nrounds = p_int(lower = 20L, upper = 200L, tags = "budget"),
      # sirflox: lower = 20L, upper = 1500L
      surv.xgboost.eta = p_dbl(lower = -4L, upper = 0, trafo = function(x) 10^x),
      surv.xgboost.max_depth = p_int(lower = 1L, upper = 20),
      surv.xgboost.colsample_bylevel = p_dbl(lower = 0.1, upper = 1),
      surv.xgboost.lambda = p_int(lower = -10, upper = 10, trafo = function(x) 2^x),
      surv.xgboost.alpha = p_int(lower = -10, upper = 10, trafo = function(x) 2^x),
      surv.xgboost.subsample = p_dbl(lower = 0.1, upper = 1),
      fs_branch.selection = p_fct(levels = c("FS_Corr", "FS_RFimp", "FS_IG", "FS_None")),
      FS_Corr.corr_filter.filter.cutoff = p_dbl(0.1, 0.9),
      FS_RFimp.rfimp_filter.filter.frac = p_dbl(0.1, 0.9),
      FS_IG.ig_filter.filter.frac = p_dbl(0.1, 0.9)
    )
  }

  search_space$add_dep("FS_Corr.corr_filter.filter.cutoff",
    "fs_branch.selection", CondEqual$new("FS_Corr")) # necessary for tuning over fs methods
  search_space$add_dep("FS_RFimp.rfimp_filter.filter.frac",
    "fs_branch.selection", CondEqual$new("FS_RFimp"))
  search_space$add_dep("FS_IG.ig_filter.filter.frac",
    "fs_branch.selection", CondEqual$new("FS_IG"))

  return(search_space)
}


###################################################
# Function of extracting task
###################################################

# function to extract each task (varying data sets)
getTask = function(task_id) {
  # pick each data set based on task_id and remove column "pat_id"
  data = get(task_id)
  # define task
  task = TaskSurv$new(
    id = paste(task_id),
    backend = data,
    time    = "osdays",
    event   = "death"
  )

  # stratum
  task$col_roles$stratum = "death"
  return(task)
}


###################################################
# Function of extracting individual pipeline
###################################################

# function to extract specific graph learner set up, mainly from the defined names in learner_id
getLearner = function(learner_id, task) {
  fallback_learner = lrn(FALLBACK_LEARNER_ID)
  # split the name in learner_id according to "-" in multiple parts, defining each pipeline
  id = stringr::str_split(learner_id, "-", simplify = TRUE)[1, ]
  PCA = ifelse("pca" %in% id, "PCA", "PCA_None")
  TUNING = ifelse("tuned" %in% id, TRUE, FALSE)
  # the learner name is the first character in the vector
  learner_id = id[1]
  # the rsf provides the prediction type distribution anyway, which is necessary for calculating the IBS
  # for the other two learners one needs a work around
  predict_type = ifelse(learner_id == "surv.rfsrc", "distr", "crank")
  learner = lrn(learner_id, predict_type = predict_type)
  # extract and set the predefined hyperparameter values according to the id
  learner$param_set$values = LEARNER_PAR_VALUES[[learner_id]]
  # learner$fallback = fallback_learner

  # linear graph learner
  glearner = getGraphLearner(learner_id, learner)
  glearner$param_set$values$pca_branch.selection = PCA

  # if the pipeline does not include the tuning step, only "no feature selection" is allowed in the preprocessing part
  if (!TUNING)
    glearner$param_set$values$fs_branch.selection = "FS_None"
  # glearner$encapsulate = ENC

  glearner$id = paste(learner_id, PCA, sep = "_")

  # tuning
  if (TUNING) {
    search_space = getTuningParams(learner_id, task)

    # autotuner for hyperparameters, search space and tuner/terminator depending on learner
    at = AutoTuner$new(
      learner = glearner,
      resampling = RESAMPLING_INNER,
      measure = msr(TUNING_MEASURE, integrated = TRUE),# eventually add times = seq(0, quantile(dat$osdays, .80)))
      search_space = search_space,
      tuner = getTuner(learner_id),
      terminator = getTerminator(learner_id)
      # eventually add:
      # store_tuning_instance = TRUE,
      # store_benchmark_result = TRUE,
      # store_models = TRUE
    )
    # at$encapsulate = ENC
    return (at)
  } else {
    # no tuning
    return(glearner)
  }
}


###################################################
# Setup benchmark experiment
###################################################

# list of tasks
tasks = lapply(TASK_IDS, getTask)
# list of learners
learners = unlist(lapply(tasks, function(t) lapply(LEARNER_IDS, function(l) {
  learner = getLearner(l, t)
  learner$id = paste(learner$id, t$id, sep = "_")
  return(learner)})))

# resampling with instantiation
set.seed(29102021)
resamplings = RESAMPLING_OUTER$clone(deep = TRUE)$instantiate(tasks[[1]])

# design of the benchmark/batchmark
design = data.table(
  task = rep(tasks, each = length(LEARNER_IDS)),
  learner = learners,
  resampling = rep(list(resamplings), length(learners))
)

# # setup for batchmark (use this on cluster instead of benchmark)
# unlink("radiomics-bmr-filters", recursive = TRUE)
#
# reg = batchtools::makeExperimentRegistry(
#   file.dir = "radiomics-bmr-filters",
#   packages = PACKAGES,
#   source = c("3_2_1_benchmarkSetup_filters.R", "server_settings.R"),
#   seed = 123
# )
#
# # registry
# reg = loadRegistry("radiomics-bmr-filters", writeable = TRUE)
# reg$default.resources = list(
#   walltime = 3600L * 48L,
#   memory = 1024L * 48L,
#   ntasks = 1L,
#   ncpus = 56L,
#   nodes = 1L,
#   clusters = "cm2_tiny",
#   partition = "cm2_tiny"
# )

# batchmark(design, reg = reg)#, store_models = TRUE)
# getStatus()

bench = benchmark(design, store_models = TRUE)
# save(bench, "data/bmr_breastCancer")
