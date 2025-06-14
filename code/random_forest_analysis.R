
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

measures <- msrs(c("classif.acc", "classif.precision", "classif.recall", "classif.specificity", "classif.fbeta", "classif.logloss", "classif.auc"))

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

evaluation <- data.frame(as.list(round(evaluation, 3)))

names(evaluation) <- c("Accuracy", "Precision", "Recall", "Specificity", "F1-score", "log loss", "AUC")

evaluation

write.csv(evaluation[1:6], file = "./data/performance_models/rf.csv", row.names = FALSE, quote = FALSE)


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
write.csv(df_loci, file = "./data/feature_importance/rf_loci.csv")

# Plot results
theme_set(theme_bw(base_size = 28))

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
    y = element_blank(),
    x = "Importance"
  )
loci_plot

# Save plots
ggsave(file = "./plots/rf/loco.pdf", plot = loco_plot)
ggsave(file = "./plots/rf/loci.pdf", plot = loci_plot)


# Partial Dependence Plot

# Train model for pdp
set.seed(247)

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
theme_set(theme_bw(base_size = 19))

effect_debt <- FeatureEffect$new(predictor, feature = "debt_income_ratio", method = "pdp")
effect_plot_debt <- effect_debt$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = debt_income_ratio, y = .value)) +
  geom_col(fill = "steelblue") +
  labs(
    y = "Predicted probability",
    x = element_blank()
  ) +
  ylim(0, 1)
effect_plot_debt

effect_purpose <- FeatureEffect$new(predictor, feature = "loan_purpose", method = "pdp")
effect_plot_purpose <- effect_purpose$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = loan_purpose, y = .value)) +
  geom_col(fill = "steelblue") +
  labs(
    y = element_blank(),
    x = element_blank()
  ) +
  ylim(0, 1)
effect_plot_purpose

effect_type <- FeatureEffect$new(predictor, feature = "loan_type", method = "pdp")
effect_plot_type <- effect_type$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = loan_type, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_type

effect_race <- FeatureEffect$new(predictor, feature = "race", method = "pdp")
effect_plot_race <- effect_race$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = race, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_race

effect_sex <- FeatureEffect$new(predictor, feature = "sex", method = "pdp")
effect_plot_sex <- effect_sex$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = sex, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_sex

effect_age <- FeatureEffect$new(predictor, feature = "age_above_62", method = "pdp")
effect_plot_age <- effect_age$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = age_above_62, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_age

effect_income <- FeatureEffect$new(predictor, feature = "income", method = "pdp")
effect_plot_income <- effect_income$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = income, y = .value)) +
  geom_line() +
  facet_wrap(~"Loan approved")
effect_plot_income

effect_amount <- FeatureEffect$new(predictor, feature = "loan_amount", method = "pdp")
effect_plot_amount <- effect_amount$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = loan_amount, y = .value)) +
  geom_line() +
  scale_x_continuous(labels = label_comma()) +
  labs(
    y = element_blank(),
    x = element_blank()
  ) +
  ylim(0, 1)
effect_plot_amount

effect_property <- FeatureEffect$new(predictor, feature = "property_value", method = "pdp")
effect_plot_property <- effect_property$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = property_value, y = .value)) +
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
  labs(
    y = "Predicted probability",
    x = element_blank()
  ) +
  ylim(0, 1)
effect_plot_lien

effect_occ <- FeatureEffect$new(predictor, feature = "occupancy_type", method = "pdp")
effect_plot_occ <- effect_occ$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = occupancy_type, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_occ

effect_pre <- FeatureEffect$new(predictor, feature = "preapproval", method = "pdp")
effect_plot_pre <- effect_pre$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = preapproval, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_pre

# Save plots
ggsave(file = "./plots/rf/effect_debt.pdf", plot = effect_plot_debt)
ggsave(file = "./plots/rf/effect_purpose.pdf", plot = effect_plot_purpose)
ggsave(file = "./plots/rf/effect_lien.pdf", plot = effect_plot_lien)
ggsave(file = "./plots/rf/effect_amount.pdf", plot = effect_plot_amount)
