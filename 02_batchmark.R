# load predefined data sets and functions
source("0_1_benchmarkSetup.R", local = TRUE)

# function to extract each task (varying data sets)
getTask = function(task_id) {
  # pick each data set based on task_id and remove column "pat_id"
  # event variable needs to be numeric instead of factor for survival
  data = get(task_id) %>%
    dplyr::select(-c("pat_id")) %>%
    mutate(death = (as.numeric(death) - 1))

  # clinical data: remove all binary variables created from a numeric
  # if(grepl("cd", task_id))
  #   data = data %>%
  #     dplyr::select(!(names(cd)[grep("bin", names(cd))]))

  # define task
  task = TaskSurv$new(
    id = paste("sirflox", task_id, sep = "-"),
    backend = data,
    time    = "osdays",
    event   = "death"
  )

  # stratum
  task$col_roles$stratum = "death"
  return(task)
}

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
      measure = msr(TUNING_MEASURE, integrated = TRUE, times = seq(0, quantile(cd$osdays, .80))),
      search_space = search_space,
      tuner = getTuner(learner_id),
      terminator = getTerminator(learner_id)#,
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

# list of tasks
tasks = lapply(TASK_IDS, getTask)
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
save(bench, "data/bmr_breastCancer")
