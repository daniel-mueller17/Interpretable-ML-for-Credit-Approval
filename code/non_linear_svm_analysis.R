
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

learner <- lrn("classif.svm", predict_type = "prob", type = "C-classification")

resampling = rsmp("subsampling", ratio = 0.75, repeats = 50)

measures <- msrs(c("classif.acc", "classif.precision", "classif.recall", "classif.specificity", "classif.fbeta", "classif.bbrier", "classif.logloss", "classif.auc"))

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
search_space <- ps(cost = p_dbl(lower = 0, upper = 8), # cost (For computational reasons only 0 to 8)
                   gamma = p_dbl(lower = 0, upper = 6, depends = (kernel == "radial")), # gamma (For computational reasons only 0 to 6)
                   degree = p_int(lower = 2, upper = 6, depends = (kernel == "polynomial")), # degree (For computational reasons only 2 to 6)
                   kernel = p_fct(c("radial", "polynomial"))) 

tuner = tnr("random_search", batch_size = 100)

terminator <- trm("evals", n_evals = 500)

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
# cost = 7.03, kernel = "radial", gamma = 2.96

# Save parameters
write.csv(as.data.frame(best_par[6:8]), file = "./data/hyperparameter_models/non_linear_svm.csv")

learner_tuned <- as_learner(factor_pipeline %>>% learner)
learner_tuned$param_set$set_values(classif.svm.cost = best_par$cost)
learner_tuned$param_set$set_values(classif.svm.kernel = best_par$kernel)
learner_tuned$param_set$set_values(classif.svm.gamma = best_par$gamma)

# Evolution of model
rr <- resample(task, learner_tuned, resampling)

future::plan("sequential")

evaluation <- rr$aggregate(measures)

evaluation

write.csv(as.data.frame(evaluation), file = "./data/performance_models/non_linear_svm.csv")
