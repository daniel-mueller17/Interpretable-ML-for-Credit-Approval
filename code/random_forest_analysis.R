
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
search_space <- lts("classif.ranger.default", min.node.size = to_tune(1, 100)) # num.tress, replace, sample.fraction, mtry.ratio, min.node.size

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
# best min.nodes.size = 23, mtry.ratio = 0.885, num.random.splits = 12, num.trees = 750, replace = FALSE, respect.unordered.factors = "order", sample.fraction = 0.371, splitrule = "extratrees"

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
