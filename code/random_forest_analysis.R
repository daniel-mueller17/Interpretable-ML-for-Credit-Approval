
# Load packages
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3tuningspaces)
library(ggplot2)
library(forcats)
library(iml)
library(dplyr)
library(scales)
library(future)
mlr_learners$get("classif.ranger")

# Load functions to calculate LOCO and LOCI
source("./code/loco_loci_function.R")

# Read data
data <- readRDS("./data/credit_approval_data.rds")

# Set seed for reproducibility
set.seed(123)


# Set up task, learner, resampling and measure
task <- as_task_classif(data, target = "action_taken", positve = "Loan approved", id = "credit_approval")
task$set_col_roles("action_taken", add_to = "stratum") # Target variable is kind of imbalanced -> stratify

learner <- lrn("classif.ranger", predict_type = "prob")

resampling = rsmp("subsampling", ratio = 0.75, repeats = 50)

measures <- msrs(c("classif.acc", "classif.precision", "classif.recall", "classif.specificity", "classif.fbeta", "classif.bbrier", "classif.logloss", "classif.auc"))

# Tuning
search_space <- lts("classif.ranger.default", min.node.size = to_tune(p_int(1, 100)), num.trees = to_tune(p_int(1,1000))) # num.tress, replace, sample.fraction, mtry.ratio, min.node.size

tuner = tnr("random_search", batch_size = 100)

terminator <- trm("evals", n_evals = 500)

future::plan("multisession", workers = 10)

instance <- tune(
  task = task,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.logloss"),
  tuner = tuner,
  terminator = terminator,
  search_space = search_space
)

best_par <- instance$result_learner_param_vals
# best min.nodes.size = 20, mtry.ratio = 0.375, num.trees = 877, replace = TRUE, sample.fraction = 0.949

# Save parameters
write.csv(as.data.frame(best_par), file = "./data/hyperparameter_models/rf.csv")

learner_tuned <- lrn("classif.ranger", predict_type = "prob")
learner_tuned$param_set$set_values(.values = best_par)

# Evolution of model
rr <- resample(task, learner_tuned, resampling)

future::plan("sequential")

evaluation <- rr$aggregate(measures)

evaluation


# Feature Importance
loss_fi <- msr("classif.logloss")

future::plan("multisession", workers = 10)

# LOCO
res_loco <- loco(task, learner_tuned, resampling, loss_fi)
df_loco = data.frame(feature = names(res_loco),
                     importance = res_loco,
                     type = "LOCO")

# LOCI
res_loci = loci(task, learner_tuned, resampling, loss_fi)
df_loci = data.frame(feature = names(res_loci),
                     importance = res_loci,
                     type = "LOCI")

future::plan("sequential")


# Save values
write.csv(df_loco, file = "./data/feature_importance/rf_loco.csv")
write.csv(data, file = "./data/feature_importance/rf_loci.csv")

# Plot results
theme_set(theme_bw(base_size = 18))

loco_plot = df_loco %>% 
  ggplot(aes(x = importance, y = fct_reorder(feature, importance))) +
  geom_col(position = "identity", fill = "steelblue") +
  labs(
    title = "LOCO",
    y = "features",
    x = "importance"
  )
loco_plot

loci_plot = df_loci %>% 
  ggplot(aes(x = importance, y = fct_reorder(feature, importance))) +
  geom_col(position = "identity", fill = "steelblue") +
  labs(
    title = "LOCI",
    y = "features",
    x = "importance"
  )
loci_plot


# Partial Dependence Plot

# Train model for pdp
splits = partition(task, ratio = 0.75)
learner_pdp <- lrn("classif.ranger", predict_type = "prob")
learner_pdp$param_set$set_values(.values = best_par)
learner_pdp$train(task, row_id = splits$train)


credit_x = task$data(rows = splits$test,
                     cols = task$feature_names)
credit_y = task$data(rows = splits$test,
                     cols = task$target_names)
predictor <- Predictor$new(learner_pdp, data = credit_x, y = credit_y)

# visualize pdps
effect_debt <- FeatureEffect$new(predictor, feature = "debt_to_income_ratio", method = "pdp")
effect_plot_debt <- effect_debt$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = debt_to_income_ratio, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_debt

effect_purpose <- FeatureEffect$new(predictor, feature = "loan_purpose", method = "pdp")
effect_plot_purpose <- effect_purpose$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = loan_purpose, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_purpose

effect_race <- FeatureEffect$new(predictor, feature = "applicant_race", method = "pdp")
effect_plot_race <- effect_race$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = applicant_race, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_race

effect_sex <- FeatureEffect$new(predictor, feature = "applicant_sex", method = "pdp")
effect_plot_sex <- effect_sex$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = applicant_sex, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_sex

effect_income <- FeatureEffect$new(predictor, feature = "income_log", method = "pdp")
effect_plot_income <- effect_income$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = income_log, y = .value)) +
  geom_line() +
  facet_wrap(~"Loan approved")
effect_plot_income

effect_amount <- FeatureEffect$new(predictor, feature = "loan_amount_log", method = "pdp")
effect_plot_amount <- effect_amount$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = loan_amount_log, y = .value)) +
  geom_line() +
  scale_x_continuous(labels = label_comma()) +
  facet_wrap(~"Loan approved")
effect_plot_amount

effect_property <- FeatureEffect$new(predictor, feature = "property_value_log", method = "pdp")
effect_plot_property <- effect_property$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = property_value_log, y = .value)) +
  geom_line() +
  scale_x_continuous(labels = label_comma()) +
  facet_wrap(~"Loan approved")
effect_plot_property

effect_co <- FeatureEffect$new(predictor, feature = "has_co.applicant", method = "pdp")
effect_plot_co <- effect_co$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = has_co.applicant, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_co

effect_lien <- FeatureEffect$new(predictor, feature = "lien_status", method = "pdp")
effect_plot_lien <- effect_lien$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = lien_status, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_lien

