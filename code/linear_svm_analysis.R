
# Load packages
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3tuningspaces)
library(mlr3pipelines)
library(ggplot2)
library(forcats)
library(iml)
library(dplyr)
library(scales)
library(future)
mlr_learners$get("classif.svm")

# Load functions to calculate LOCO and LOCI
source("./code/loco_loci_function.R")

# Read data
data <- readRDS("./data/credit_approval_data.rds")

# Set seed for reproducibility
set.seed(123)


# Set up task, learner, resampling and measure
task <- as_task_classif(data, target = "action_taken", positve = "Loan approved", id = "credit_approval")
task$set_col_roles("action_taken", add_to = "stratum") # Target variable is kind of imbalanced -> stratify

learner <- lrn("classif.svm", predict_type = "prob", kernel = "linear", type = "C-classification")

resampling = rsmp("subsampling", ratio = 0.75, repeats = 50)

measures <- msrs(c("classif.acc", "classif.precision", "classif.recall", "classif.specificity", "classif.fbeta", "classif.logloss", "classif.auc"))

# Set up pipeline to encode factors
factor_pipeline <- 
  po("encode", method = "one-hot",
     affect_columns = selector_cardinality_greater_than(2),
     id = "one_hot") %>>% 
  po("encode", method = "treatment",
     affect_columns = selector_type("factor"), id = "treatment")

learner_pipe <- as_learner(factor_pipeline %>>% learner)
learner_pipe$id <- "learner_pipe"

# Tuning
search_space <- ps(cost = p_dbl(lower = 1e-6, upper = 1e6, logscale = TRUE)) # cost

tuner = tnr("random_search", batch_size = 100)

terminator <- trm("evals", n_evals = 200)

future::plan("multisession", workers = 10)

instance <- tune(
  task = task,
  learner = learner_pipe,
  resampling = rsmp("holdout"),
  measure = msr("classif.logloss"),
  tuner = tuner,
  terminator = terminator,
  search_space = search_space
)

best_par <- instance$result_learner_param_vals
# cost = 0.016

# Save parameters
write.csv(as.data.frame(best_par[5:6]), file = "./data/hyperparameter_models/linear_svm.csv")

learner_tuned <- as_learner(factor_pipeline %>>% learner)
learner_tuned$param_set$set_values(classif.svm.cost = best_par$cost)

# Evolution of model
set.seed(456)

rr <- resample(task, learner_tuned, resampling)

future::plan("sequential")

evaluation <- rr$aggregate(measures)

evaluation

write.csv(as.data.frame(evaluation), file = "./data/performance_models/linear_svm.csv")


# Feature Importance
loss_fi <- msr("classif.logloss")
resampling_fi = rsmp("subsampling", ratio = 0.75, repeats = 30) # For computational reasons only 30 repeats

set.seed(789)

future::plan("multisession", workers = 10)

# LOCO
res_loco <- loco(task, learner_tuned, resampling_fi, loss_fi)
df_loco = data.frame(feature = names(res_loco),
                     importance = res_loco,
                     type = "LOCO")

# LOCI
res_loci = loci(task, learner_tuned, resampling_fi, loss_fi)
df_loci = data.frame(feature = names(res_loci),
                     importance = res_loci,
                     type = "LOCI")

future::plan("sequential")


# Save values
write.csv(df_loco, file = "./data/feature_importance/linear_svm_loco.csv")
write.csv(data, file = "./data/feature_importance/linear_svm_loci.csv")

# Plot results
theme_set(theme_bw(base_size = 18))

loco_plot = df_loco %>% 
  ggplot(aes(x = importance/max(importance), y = fct_reorder(feature, importance))) +
  geom_col(position = "identity", fill = "steelblue") +
  labs(
    title = "LOCO",
    y = "Features",
    x = "Importance"
  )
loco_plot

loci_plot = df_loci %>% 
  ggplot(aes(x = importance/max(importance), y = fct_reorder(feature, importance))) +
  geom_col(position = "identity", fill = "steelblue") +
  labs(
    title = "LOCI",
    y = "Features",
    x = "Importance"
  )
loci_plot


# Partial Dependence Plot

# Train model for pdp
splits = partition(task, ratio = 0.75)
learner_pdp<- as_learner(factor_pipeline %>>% learner)
learner_pdp$param_set$set_values(classif.svm.cost = best_par$cost)
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

effect_type <- FeatureEffect$new(predictor, feature = "loan_type", method = "pdp")
effect_plot_type <- effect_type$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = loan_type, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_type

effect_race <- FeatureEffect$new(predictor, feature = "applicant_race", method = "pdp")
effect_plot_race <- effect_race$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = applicant_race, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_race

effect_eth <- FeatureEffect$new(predictor, feature = "applicant_ethnicity", method = "pdp")
effect_plot_eth <- effect_eth$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = applicant_ethnicity, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_eth

effect_sex <- FeatureEffect$new(predictor, feature = "applicant_sex", method = "pdp")
effect_plot_sex <- effect_sex$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = applicant_sex, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_sex

effect_age <- FeatureEffect$new(predictor, feature = "applicant_age_above_62", method = "pdp")
effect_plot_age <- effect_age$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = applicant_age_above_62, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_age

effect_occ <- FeatureEffect$new(predictor, feature = "occupancy_type", method = "pdp")
effect_plot_occ <- effect_occ$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = occupancy_type, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_occ

effect_lien <- FeatureEffect$new(predictor, feature = "lien_status", method = "pdp")
effect_plot_lien <- effect_lien$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = lien_status, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_lien

effect_pre <- FeatureEffect$new(predictor, feature = "preapproval", method = "pdp")
effect_plot_pre <- effect_pre$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = preapproval, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_pre

effect_co <- FeatureEffect$new(predictor, feature = "has_co.applicant", method = "pdp")
effect_plot_co <- effect_co$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = has_co.applicant, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_co

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
