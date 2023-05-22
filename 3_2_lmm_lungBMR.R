###################################################
# Settings
###################################################

# load packages
library(mlr3)
library(mlr3proba)
library(jtools)
library(interactions)
library(readr)
library(lme4)
library(insight)
library(emmeans)
library(ggplot2)
library(dplyr)
library(stringr)
library(kableExtra)

# load .RDS file with IBS scores
bmr = readRDS("data/bmr_lung.RDS")

################################################################################
############################# EXTRACT BMR RESULTS ##############################
################################################################################

# Extract bmr results benchmark object
scores = as.data.table(bmr$score(list(msr("surv.graf"), msr("surv.cindex"))))
# scores_table_aggr_final = as.data.table(bmr$aggregate(list(msr("surv.graf"), msr("surv.cindex"))))


scores = scores %>% select(-c(nr, prediction, resampling, resampling_id, uhash, task, task_id, learner)) %>%
  mutate(learner_id = factor(gsub(".*surv.(.+)*", "\\1", learner_id))) %>%
  # extract algorithm
  mutate(algo = factor(gsub("(.+)_PCA.*", "\\1", learner_id))) %>%
  # extract PCA setting
  mutate(PCA = factor(ifelse(grepl("None",learner_id), "no PCA", "PCA"))) %>%
  # extract tuning setting
  mutate(Tuning = factor(ifelse(grepl("tuned",learner_id), "tuned", "untuned"), levels = c("untuned", "tuned")))

scores$surv.cindex <- as.numeric(scores$surv.cindex)
scores$surv.graf <- as.numeric(scores$surv.graf)


################################################################################
############################# ML RESULTS ANALYSIS ##############################
################################################################################

###################################################
# Mixed Model Approach
###################################################

# mixed model with lme4; set CV folds = iterations as random effect
lmm = lmer(formula = surv.graf ~ 1 + learner_id + (1|iteration), data = scores)
qqline(resid(lmm), col = "red") # add a perfect fit line

# compare to null model
# lmm0 = lmer(formula = surv.graf ~ 1 + (1|iteration), data = tab)
# anova(lmm0, lmm)
anova(lmm)
summary(lmm)

# variance explained by CV fold / iterations
# variance iteration / (total variance); total var = var_iteration + var_residual
performance::icc(lmm)

# test model assumptions
x11()
performance::check_model(lmm)
performance::check_normality(lmm)
performance::check_homogeneity(lmm)
performance::check_posterior_predictions(lmm)
dev.off()
# adjustments necessary -> e.g boxcox

performance::check_model(lmm, check = c("qq", "normality", "linearity", "homogeneity"))

joint_tests(lmm)
joint_tests(lmm, by = "learner_id")


###################################################
# Interaction-style plot EMMs
###################################################

# plot mixed model with interactions via emmip
emmip(lmm, ~ learner_id, CIs=TRUE)

# use coefficient plot