
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
mlr_learners$get("classif.rpart")

# Load functions to calculate LOCO and LOCI
source("./code/loco_loci_function.R")

# Read data
data <- readRDS("./data/credit_approval_data.rds")

# Set seed for reproducibility
set.seed(123)


# Set up task, learner, resampling and measure
task <- as_task_classif(data, target = "action_taken", positve = "Loan approved", id = "credit_approval")
task$set_col_roles("action_taken", add_to = "stratum") # Target variable is kind of imbalanced -> stratify

learner <- lrn("classif.rpart", predict_type = "prob")

resampling = rsmp("subsampling", ratio = 0.75, repeats = 50)

measures <- msrs(c("classif.acc", "classif.precision", "classif.recall", "classif.specificity", "classif.fbeta", "classif.logloss", "classif.auc"))

# Tuning
# We dont have NAs in the data -> dont need surrogate splits -> no tuning for surrogate splits parameter

search_space <- lts("classif.rpart.rbv2") # Tune over cp, maxdepth, minbucket, minsplit

tuner = tnr("random_search", batch_size = 100)

terminator <- trm("evals", n_evals = 500)

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
# cp = 0.0014, maxdepth = 24, minbucket = 15, minsplit = 63

# Save parameters
write.csv(as.data.frame(best_par), file = "./data/hyperparameter_models/tree.csv")

learner_tuned <- lrn("classif.rpart", predict_type = "prob")
learner_tuned$param_set$set_values(.values = best_par)

# Evolution of model
rr <- resample(task, learner_tuned, resampling)

evaluation <- rr$aggregate(measures)

evaluation <- data.frame(as.list(round(evaluation, 3)))

names(evaluation) <- c("Accuracy", "Precision", "Recall", "Specificity", "F1-score", "log loss", "AUC")

evaluation

write.csv(evaluation[1:6], file = "./data/performance_models/tree.csv", row.names = FALSE, quote = FALSE)


# Feature Importance
loss_fi <- msr("classif.logloss")

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


# Save values
write.csv(df_loco, file = "./data/feature_importance/tree_loco.csv")
write.csv(df_loci, file = "./data/feature_importance/tree_loci.csv")

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
ggsave(file = "./plots/tree/loco.pdf", plot = loco_plot)
ggsave(file = "./plots/tree/loci.pdf", plot = loci_plot)


# Partial Dependence Plot

# Train model for pdp
splits = partition(task, ratio = 0.75)
learner_pdp <- lrn("classif.rpart", predict_type = "prob")
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

effect_eth <- FeatureEffect$new(predictor, feature = "ethnicity", method = "pdp")
effect_plot_eth <- effect_eth$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = ethnicity, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_eth

effect_income <- FeatureEffect$new(predictor, feature = "income", method = "pdp")
effect_plot_income <- effect_income$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = income, y = .value)) +
  geom_line() +
  labs(
    y = "Predicted probability",
    x = element_blank()
  ) +
  ylim(0, 1)
effect_plot_income

effect_amount <- FeatureEffect$new(predictor, feature = "loan_amount", method = "pdp")
effect_plot_amount <- effect_amount$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = loan_amount, y = .value)) +
  geom_line() +
  scale_x_continuous(labels = label_comma()) +
  labs(
    y = "Predicted probability",
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
  labs(
    y = element_blank(),
    x = element_blank()
  ) +
  ylim(0, 1)
effect_plot_property

effect_co <- FeatureEffect$new(predictor, feature = "has_co.applicant", method = "pdp")
effect_plot_co <- effect_co$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = has_co.applicant, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_co

effect_conforming <- FeatureEffect$new(predictor, feature = "conforming_loan_limit", method = "pdp")
effect_plot_conforming <- effect_conforming$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = conforming_loan_limit, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_conforming

effect_occ <- FeatureEffect$new(predictor, feature = "occupancy_type", method = "pdp")
effect_plot_occ <- effect_occ$results %>% 
  filter(.class == "Loan approved") %>% 
  ggplot(aes(x = occupancy_type, y = .value)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~"Loan approved")
effect_plot_occ

# Save plots
ggsave(file = "./plots/tree/effect_debt.pdf", plot = effect_plot_debt)
ggsave(file = "./plots/tree/effect_purpose.pdf", plot = effect_plot_purpose)
ggsave(file = "./plots/tree/effect_amount.pdf", plot = effect_plot_amount)
ggsave(file = "./plots/tree/effect_property.pdf", plot = effect_plot_property)
ggsave(file = "./plots/tree/effect_income.pdf", plot = effect_plot_income)
