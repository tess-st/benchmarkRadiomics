# Load datasets
# only radiomics datsets from liver (rd_l)and tumor /rd-t) provided, clinical data  (cd) and combinations not available
TASK_IDS = c("rd_l", "rd_t")# , "cd", "cd_rd_l", "cd_rd_t")

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
  "surv.rfsrc" = list(ntree = 500L, na.action = "na.impute"),
  "surv.xgboost" = list(nrounds = 500L, verbose = 0)
)

# define a fallback learner, such that benchmark does not throw an error and stops the benchmark experiment
FALLBACK_LEARNER_ID = "surv.kaplan"

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

# Settings for tuners
ETA = 2
N_EVALS = 200L

# tuner: for xgboost and rfsrc hyperband, in case of glmnet no meaningful eta definable -> use random search instead
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
      surv.rfsrc.ntree = p_int(lower = 20L, upper = 1500L, tags = "budget"),
      surv.rfsrc.mtry = p_int(lower = 3L, upper = ceiling((task$ncol)^(1/1.5))),
      surv.rfsrc.nodesize = p_int(lower = 1L, upper = task$nrow),
      fs_branch.selection = p_fct(levels = c("FS_Corr", "FS_RFimp", "FS_IG", "FS_None")),
      FS_Corr.corr_filter.filter.cutoff = p_dbl(0.1, 0.9),
      FS_RFimp.rfimp_filter.filter.frac = p_dbl(0.1, 0.9),
      FS_IG.ig_filter.filter.frac = p_dbl(0.1, 0.9)
    )
  } else if (learner_id == "surv.xgboost") {
    search_space = ps(
      surv.xgboost.nrounds = p_int(lower = 20L, upper = 1500L, tags = "budget"),
      surv.xgboost.eta = p_dbl(lower = -4L, upper = 0, trafo = function(x) 10^x),
      surv.xgboost.max_depth = p_int(lower = 1L, upper = 20),
      surv.xgboost.colsample_bylevel = p_dbl(lower = 0.1, upper = 1),
      surv.xgboost.lambda = p_int(lower = -10, upper = 10, trafo = function(x) 2^x),
      surv.xgboost.alpha = p_int(lower = -10, upper = 10, trafo = function(x) 2^x),
      surv.xgboost.subsample = p_dbl(lower = 0.1, upper = 1),
      fs_branch.selection = p_fct(levels = c("FS_Corr", "FS_RFimp", "FS_IG", "FS_None")),
      FS_Corr.corr_filter.filter.cutoff = p_dbl(0.1, 0.9),
      FS_RFimp.rfimp_filter.filter.frac = p_dbl(0.1, 0.9),
      FS_IG.ig_filter.filter.frac = p_dbl(0.1, 0.9)#,
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
