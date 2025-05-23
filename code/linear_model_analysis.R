
# Load packages
library(mlr3)
library(mlr3learners)
mlr_learners$get("classif.log_reg")
mlr_measures$get("classif.precision")
mlr_measures$get("classif.recall")
mlr_measures$get("classif.fbeta")


# Read data
data <- readRDS("./data/credit_approval_data.rds")
data$hoepa_status <- NULL
data$loan_to_property_value_ratio <- NULL

# Set seed for reproducibility
set.seed(123)


# Set up task, learner, resampling and measure
task <- as_task_classif(data, target = "action_taken", positve = "Loan approved", id = "credit_approval")
task$set_col_roles("action_taken", add_to = "stratum") # Target variable is kind of imbalanced -> stratify

learner <- lrn("classif.log_reg", predict_type = "prob")

resampling = rsmp("holdout", ratio = 0.8)

resampling$instantiate(task)

measures <- msrs(c("classif.acc", "classif.precision", "classif.recall", "classif.fbeta", "classif.bbrier", "classif.logloss"))


# Train and Test model
learner$train(task, resampling$train_set(1))

prediction <- learner$predict(task, resampling$test_set(1))

# Evaluation of model
confusion_matrix <- prediction$confusion
evaluation <- prediction$score(measures)

confusion_matrix
evaluation


# Feature Importance
