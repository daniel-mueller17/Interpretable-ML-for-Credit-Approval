
# Load packages
library(mlr3)
library(mlr3learners)
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
loss_fi <- msr("classif.logloss")
resampling_fi = rsmp("subsampling", ratio = 0.8, repeats = 50)

# LOCO
res_loco <- loco(task, learner, resampling_fi, loss_fi)
df_loco = data.frame(feature = names(res_loco),
                     importance = res_loco,
                     type = "LOCO")

# LOCI
res_loci = loci(task, learner, resampling_fi, loss_fi)
df_loci = data.frame(feature = names(res_loci),
                     importance = res_loci,
                     type = "LOCI")

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


