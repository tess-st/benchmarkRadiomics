###################################################
# Settings
###################################################

# load packages
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

# load .csv file with IBS scores
tab = read_csv("data/bmr_scores_sirfloxRadiomics.csv")

################################################################################
############################# ML RESULTS ANALYSIS ##############################
################################################################################

###################################################
# Mixed Model Approach
###################################################

# mixed model with lme4; set CV folds = iterations as random effect
lmm = lmer(formula = surv.graf ~ 1 + task_id*learner_id + (1|iteration), data = tab)
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

performance::check_model(lmm, check = c("qq", "normality", "linearity", "homogeneity"))

joint_tests(lmm)
joint_tests(lmm, by = "learner_id")


###################################################
# Interaction-style plot EMMs
###################################################

# plot mixed model with interactions via emmip
emmip(lmm, task_id ~ learner_id, CIs=TRUE)
emmip(lmm, learner_id ~ task_id, CIs=TRUE)

# reorganize structure of plot
interact = emmip(lmm, task_id ~ learner_id, CIs=TRUE, dotarg = list(size = 3, shape = 5))
interact$labels$x = "Pipeline Configuration"
interact$labels$y = "IBS"
interact$labels$colour = "Task / Data Set"
interact$plot_env$dotarg$position$width = 0.45
interact$layers[[3]]$aes_params$size = 3
interact$data = interact$data %>% mutate(model = ifelse(str_detect(learner_id, "RSF"), "RSF",   ifelse(str_detect(learner_id, "glm"), "glmnet", "xgboost")))

interact = interact + scale_color_manual(values = c(viridis(6)[-6])) +
  scale_x_discrete(labels = c("RSF (no PCA, untuned)" = "no PCA, untuned",
    "RSF (no PCA, tuned)" = "no PCA, tuned",
    "RSF (PCA, untuned)" = "PCA, untuned",
    "RSF (PCA, tuned)" = "PCA, tuned",
    "glmnet (no PCA, untuned)" = "no PCA, untuned",
    "glmnet (no PCA, tuned)" = "no PCA, tuned",
    "glmnet (PCA, untuned)" = "PCA, untuned",
    "glmnet (PCA, tuned)" = "PCA, tuned",
    "xgboost (no PCA, untuned)" = "no PCA, untuned",
    "xgboost (no PCA, tuned)" = "no PCA, tuned",
    "xgboost (PCA, untuned)" = "PCA, untuned",
    "xgboost (PCA, tuned)" = "PCA, tuned")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), text = element_text(size=15)) +
  facet_grid(.~model, scales = "free", space='free')

interact


###################################################
# EMMs with simultaneous CIs
###################################################

# address the issue of multiple comparisons when testing several hypotheses simultaneously
emms = emmeans(lmm, pairwise ~ task_id | learner_id)
# adjust cis
adj.ci = confint(emms, adjust = "sidak")$emmeans
dat.ci = as.data.frame(adj.ci)
interact.dat.adj = merge(interact$data, dat.ci[, c("task_id", "learner_id", "lower.CL", "upper.CL")], by = c("task_id", "learner_id"))
interact.dat.adj = interact.dat.adj %>%
  mutate(LCL = lower.CL, UCL = upper.CL)
interact.adj = interact
interact.adj$data = interact.dat.adj

interact.adj + theme(text = element_text(size = 20), legend.position = "bottom")

# overview table EMMs
emm_dat = interact.adj$data

# reorganize data for a structured table output
emm_dat$Tuning = ifelse(grepl("untuned", emm_dat$xvar), "untuned", "tuned")
emm_dat$PCA = ifelse(grepl("no PCA", emm_dat$xvar), "no PCA", "PCA")
emm_dat = emm_dat %>%
  dplyr::select(c("task_id", "model", "Tuning", "PCA", "yvar", "LCL", "UCL")) %>%
  rename("Dataset" = "task_id", "EMM" = "yvar", "Sim. LCL" = "LCL", "Sim. UCL" = "UCL")
emm_dat_untuned = emm_dat %>%
  filter(Tuning == "untuned") %>%
  select(-c(model, Tuning))
emm_dat_tuned = emm_dat %>%
  filter(Tuning == "tuned") %>%
  select(-c(Dataset, model, Tuning, PCA))
emm_dat_wide = cbind(emm_dat_untuned, emm_dat_tuned)

# create final table
emm_tab = emm_dat_wide %>%
  kbl() %>%
  kable_styling() %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  add_header_above(c("Setting" = 2, "Untuned" = 3, "Tuned" = 3),
    bold = TRUE, font_size = 18) %>%
  # column_spec(2, border_right = T) %>%
  column_spec(1, width = "15em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "5em") %>%
  # column_spec(5, border_right = T) %>%
  column_spec(6, width = "5em") %>%
  column_spec(7, width = "5em") %>%
  # column_spec(1, bold = T, background = "#91D1C233") %>%
  pack_rows("glmnet", 1, 10, bold = TRUE, background = "#91D1C233") %>%
  pack_rows("RSF", 11, 20, bold = TRUE, background = "#91D1C233") %>%
  pack_rows("xgboost", 21, 30, bold = TRUE, background = "#91D1C233")

emm_tab

# save as .html table
# emm_tab %>% cat(., file = "results/emms.html")
