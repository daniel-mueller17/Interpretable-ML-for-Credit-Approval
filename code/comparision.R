
library(ggplot2)
library(forcats)
library(dplyr)
library(patchwork)

# Performance

# Read data performance
performance_linear <- read.csv("./data/performance_models/linear.csv")
performance_tree <- read.csv("./data/performance_models/tree.csv")
performance_rf <- read.csv("./data/performance_models/rf.csv")
performance_linear_svm <- read.csv("./data/performance_models/linear_svm.csv")
performance_non_linear_svm <- read.csv("./data/performance_models/non_linear_svm.csv")

# Combine data
performance_linear <- performance_linear %>% 
  mutate(
    Model = "Logistic Rgegression"
  ) %>% 
  relocate(Model)
performance_tree <- performance_tree %>% 
  mutate(
    Model = "Classifiaction Tree"
  ) %>% 
  relocate(Model)
performance_rf <- performance_rf %>% 
  mutate(
    Model = "Random Forest"
  ) %>% 
  relocate(Model)
performance_linear_svm <- performance_linear_svm %>% 
  mutate(
    Model = "Linear SVM"
  ) %>% 
  relocate(Model)
performance_non_linear_svm <- performance_non_linear_svm %>% 
  mutate(
    Model = "Non-Linear SVM"
  ) %>% 
  relocate(Model)

performance_all <- rbind(performance_linear, performance_tree, performance_rf,
                         performance_linear_svm, performance_non_linear_svm)

names(performance_all) <- c("Model", "Accuracy", "Precision", "Recall", "Specificity", "F1-score", "log loss")

# Save data
write.csv(performance_all, file = "./data/performance_models/all_models.csv", row.names = FALSE, quote = FALSE)


# Feature importnace

# Read data feature importance
fi_linear_loco <- read.csv("./data/feature_importance/linear_loco.csv")
fi_tree_loco <- read.csv("./data/feature_importance/tree_loco.csv")
fi_rf_loco <- read.csv("./data/feature_importance/rf_loco.csv")
fi_linear_svm_loco <- read.csv("./data/feature_importance/linear_svm_loco.csv")
fi_non_linear_svm_loco <- read.csv("./data/feature_importance/non_linear_svm_loco.csv")

fi_linear_loci <- read.csv("./data/feature_importance/linear_loci.csv")
fi_tree_loci <- read.csv("./data/feature_importance/tree_loci.csv")
fi_rf_loci <- read.csv("./data/feature_importance/rf_loci.csv")
fi_linear_svm_loci <- read.csv("./data/feature_importance/linear_svm_loci.csv")
fi_non_linear_svm_loci <- read.csv("./data/feature_importance/non_linear_svm_loci.csv")

# Filter top 6
fi_linear_loco <- fi_linear_loco %>% 
  slice_max(importance, n = 6)
fi_linear_loci <- fi_linear_loci %>% 
  slice_max(importance, n = 6)

fi_tree_loco <- fi_tree_loco %>% 
  slice_max(importance, n = 6)
fi_tree_loci <- fi_tree_loci %>% 
  arrange(desc(importance)) %>% 
  head(n = 6)

fi_rf_loco <- fi_rf_loco %>% 
  slice_max(importance, n = 6)
fi_rf_loci <- fi_rf_loci %>% 
  slice_max(importance, n = 6)

fi_linear_svm_loco <- fi_linear_svm_loco %>% 
  slice_max(importance, n = 6)
fi_linear_svm_loci <- fi_linear_svm_loci %>% 
  slice_max(importance, n = 6)

fi_non_linear_svm_loco <- fi_non_linear_svm_loco %>% 
  slice_max(importance, n = 6)
fi_non_linear_svm_loci <- fi_non_linear_svm_loci %>% 
  slice_max(importance, n = 6)

# Plot results
theme_set(theme_bw(base_size = 28))

# Linear Model
loco_plot_linear = fi_linear_loco %>% 
  ggplot(aes(x = importance/max(importance), y = fct_reorder(feature, importance))) +
  geom_col(position = "identity", fill = "steelblue") +
  labs(
    y = "Features",
    x = "Importance"
  )
loco_plot_linear

loci_plot_linear = fi_linear_loci %>% 
  ggplot(aes(x = importance/max(importance), y = fct_reorder(feature, importance))) +
  geom_col(position = "identity", fill = "#ff7f0e") +
  labs(
    y = "Features",
    x = "Importance"
  )
loci_plot_linear

# Classification Tree
loco_plot_tree = fi_tree_loco %>% 
  ggplot(aes(x = importance/max(importance), y = fct_reorder(feature, importance))) +
  geom_col(position = "identity", fill = "steelblue") +
  labs(
    x = "Importance",
    y = element_blank()
  )
loco_plot_tree

loci_plot_tree = fi_tree_loci %>% 
  ggplot(aes(x = importance/max(importance), y = fct_reorder(feature, importance))) +
  geom_col(position = "identity", fill = "#ff7f0e") +
  labs(
    x = "Importance",
    y = element_blank()
  )
loci_plot_tree

# Random Forest
loco_plot_rf = fi_rf_loco %>% 
  ggplot(aes(x = importance/max(importance), y = fct_reorder(feature, importance))) +
  geom_col(position = "identity", fill = "steelblue") +
  labs(
    y = "Features",
    x = "Importance"
  )
loco_plot_rf

loci_plot_rf = fi_rf_loci %>% 
  ggplot(aes(x = importance/max(importance), y = fct_reorder(feature, importance))) +
  geom_col(position = "identity", fill = "#ff7f0e") +
  labs(
    y = "Features",
    x = "Importance"
  )
loci_plot_rf

# Linear SVM
loco_plot_linear_svm = fi_linear_svm_loco %>% 
  ggplot(aes(x = importance/max(importance), y = fct_reorder(feature, importance))) +
  geom_col(position = "identity", fill = "steelblue") +
  labs(
    x = "Importance",
    y = element_blank()
  )
loco_plot_linear_svm

loci_plot_linear_svm = fi_linear_svm_loci %>% 
  ggplot(aes(x = importance/max(importance), y = fct_reorder(feature, importance))) +
  geom_col(position = "identity", fill = "#ff7f0e") +
  labs(
    x = "Importance",
    y = element_blank()
  )
loci_plot_linear_svm

# Non-linear SVM
loco_plot_non_linear_svm = fi_non_linear_svm_loco %>% 
  ggplot(aes(x = importance/max(importance), y = fct_reorder(feature, importance))) +
  geom_col(position = "identity", fill = "steelblue") +
  labs(
    y = "Features",
    x = "Importance"
  )
loco_plot_non_linear_svm

loci_plot_non_linear_svm = fi_non_linear_svm_loci %>% 
  ggplot(aes(x = importance/max(importance), y = fct_reorder(feature, importance))) +
  geom_col(position = "identity", fill = "#ff7f0e") +
  labs(
    y = "Features",
    x = "Importance"
  )
loci_plot_non_linear_svm

# Save plots
ggsave(file = "./plots/comparison/linear_fi.pdf", plot = loco_plot_linear / loci_plot_linear + plot_layout(axis_titles = "collect"))
ggsave(file = "./plots/comparison/tree_fi.pdf", plot = loco_plot_tree / loci_plot_tree + plot_layout(axis_titles = "collect"))
ggsave(file = "./plots/comparison/rf_fi.pdf", plot = loco_plot_rf / loci_plot_rf + plot_layout(axis_titles = "collect"))
ggsave(file = "./plots/comparison/linear_svm_fi.pdf", plot = loco_plot_linear_svm / loci_plot_linear_svm + plot_layout(axis_titles = "collect"))
ggsave(file = "./plots/comparison/non_linear_svm_fi.pdf", plot = loco_plot_non_linear_svm / loci_plot_non_linear_svm + plot_layout(axis_titles = "collect"))


# Feature Effects

# Read data feature effect
effect_linear_debt <- read.csv("./data/feature_effects/linear_model/debt.csv")
effect_linear_purpose <- read.csv("./data/feature_effects/linear_model/purpose.csv")
effect_linear_pre <- read.csv("./data/feature_effects/linear_model/pre.csv")
effect_linear_amount <- read.csv("./data/feature_effects/linear_model/amount.csv")
effect_linear_lien <- read.csv("./data/feature_effects/linear_model/lien.csv")
effect_linear_co <- read.csv("./data/feature_effects/linear_model/co.csv")
effect_linear_income <- read.csv("./data/feature_effects/linear_model/income.csv")
effect_linear_race <- read.csv("./data/feature_effects/linear_model/race.csv")
effect_linear_eth <- read.csv("./data/feature_effects/linear_model/eth.csv")
effect_linear_sex <- read.csv("./data/feature_effects/linear_model/sex.csv")
effect_linear_type <- read.csv("./data/feature_effects/linear_model/type.csv")

effect_tree_debt <- read.csv("./data/feature_effects/tree/debt.csv")
effect_tree_purpose <- read.csv("./data/feature_effects/tree/purpose.csv")
effect_tree_pre <- read.csv("./data/feature_effects/tree/pre.csv")
effect_tree_amount <- read.csv("./data/feature_effects/tree/amount.csv")
effect_tree_lien <- read.csv("./data/feature_effects/tree/lien.csv")
effect_tree_co <- read.csv("./data/feature_effects/tree/co.csv")
effect_tree_income <- read.csv("./data/feature_effects/tree/income.csv")
effect_tree_race <- read.csv("./data/feature_effects/tree/race.csv")
effect_tree_eth <- read.csv("./data/feature_effects/tree/eth.csv")
effect_tree_sex <- read.csv("./data/feature_effects/tree/sex.csv")
effect_tree_type <- read.csv("./data/feature_effects/tree/type.csv")

effect_rf_debt <- read.csv("./data/feature_effects/rf/debt.csv")
effect_rf_purpose <- read.csv("./data/feature_effects/rf/purpose.csv")
effect_rf_pre <- read.csv("./data/feature_effects/rf/pre.csv")
effect_rf_amount <- read.csv("./data/feature_effects/rf/amount.csv")
effect_rf_lien <- read.csv("./data/feature_effects/rf/lien.csv")
effect_rf_co <- read.csv("./data/feature_effects/rf/co.csv")
effect_rf_income <- read.csv("./data/feature_effects/rf/income.csv")
effect_rf_race <- read.csv("./data/feature_effects/rf/race.csv")
effect_rf_eth <- read.csv("./data/feature_effects/rf/eth.csv")
effect_rf_sex <- read.csv("./data/feature_effects/rf/sex.csv")
effect_rf_type <- read.csv("./data/feature_effects/rf/type.csv")

effect_linear_svm_debt <- read.csv("./data/feature_effects/linear_svm/debt.csv")
effect_linear_svm_purpose <- read.csv("./data/feature_effects/linear_svm/purpose.csv")
effect_linear_svm_pre <- read.csv("./data/feature_effects/linear_svm/pre.csv")
effect_linear_svm_amount <- read.csv("./data/feature_effects/linear_svm/amount.csv")
effect_linear_svm_lien <- read.csv("./data/feature_effects/linear_svm/lien.csv")
effect_linear_svm_co <- read.csv("./data/feature_effects/linear_svm/co.csv")
effect_linear_svm_income <- read.csv("./data/feature_effects/linear_svm/income.csv")
effect_linear_svm_race <- read.csv("./data/feature_effects/linear_svm/race.csv")
effect_linear_svm_eth <- read.csv("./data/feature_effects/linear_svm/eth.csv")
effect_linear_svm_sex <- read.csv("./data/feature_effects/linear_svm/sex.csv")
effect_linear_svm_type <- read.csv("./data/feature_effects/linear_svm/type.csv")

effect_non_linear_svm_debt <- read.csv("./data/feature_effects/non_linear_svm/debt.csv")
effect_non_linear_svm_purpose <- read.csv("./data/feature_effects/non_linear_svm/purpose.csv")
effect_non_linear_svm_pre <- read.csv("./data/feature_effects/non_linear_svm/pre.csv")
effect_non_linear_svm_amount <- read.csv("./data/feature_effects/non_linear_svm/amount.csv")
effect_non_linear_svm_lien <- read.csv("./data/feature_effects/non_linear_svm/lien.csv")
effect_non_linear_svm_co <- read.csv("./data/feature_effects/non_linear_svm/co.csv")
effect_non_linear_svm_income <- read.csv("./data/feature_effects/non_linear_svm/income.csv")
effect_non_linear_svm_race <- read.csv("./data/feature_effects/non_linear_svm/race.csv")
effect_non_linear_svm_eth <- read.csv("./data/feature_effects/non_linear_svm/eth.csv")
effect_non_linear_svm_sex <- read.csv("./data/feature_effects/non_linear_svm/sex.csv")
effect_non_linear_svm_type <- read.csv("./data/feature_effects/non_linear_svm/type.csv")

# Combine datasets
effect_debt <- rbind(effect_linear_debt, effect_tree_debt, effect_rf_debt, effect_linear_svm_debt, effect_non_linear_svm_debt)
effect_purpose <- rbind(effect_linear_purpose, effect_tree_purpose, effect_rf_purpose, effect_linear_svm_purpose, effect_non_linear_svm_purpose)
effect_pre <- rbind(effect_linear_pre, effect_tree_pre, effect_rf_pre, effect_linear_svm_pre, effect_non_linear_svm_pre)
effect_amount <- rbind(effect_linear_amount, effect_tree_amount, effect_rf_amount, effect_linear_svm_amount, effect_non_linear_svm_amount)
effect_lien <- rbind(effect_linear_lien, effect_tree_lien, effect_rf_lien, effect_linear_svm_lien, effect_non_linear_svm_lien)
effect_co <- rbind(effect_linear_co, effect_tree_co, effect_rf_co, effect_linear_svm_co, effect_non_linear_svm_co)
effect_income <- rbind(effect_linear_income, effect_tree_income, effect_rf_income, effect_linear_svm_income, effect_non_linear_svm_income)
effect_race <- rbind(effect_linear_race, effect_tree_race, effect_rf_race, effect_linear_svm_race, effect_non_linear_svm_race)
effect_eth <- rbind(effect_linear_eth, effect_tree_eth, effect_rf_eth, effect_linear_svm_eth, effect_non_linear_svm_eth)
effect_sex <- rbind(effect_linear_sex, effect_tree_sex, effect_rf_sex, effect_linear_svm_sex, effect_non_linear_svm_sex)
effect_type <- rbind(effect_linear_type, effect_tree_type, effect_rf_type, effect_linear_svm_type, effect_non_linear_svm_type)

# Plot results
theme_set(theme_bw(base_size = 19))
color_vector = c("red", "#0066CC", "#FF9900", "#999999", "#9900FF", "#99CCFF")

# Debt
effect_plot_debt <- effect_debt %>% 
  filter(.class == "Loan approved") %>% 
  mutate(
    debt_income_ratio = factor(debt_income_ratio,
                               levels = c("<20%", "20%-29%", "30%-35%", "36%-42%", "43%-49%", "50%-60%", ">60%", "Unknown")),
    Model = factor(Model, levels = c("Logistic Regression", "Tree", "Random Forest", "Linear SVM", "Non-Linear SVM"))
  ) %>% 
  ggplot(aes(x = debt_income_ratio, y = .value, fill = Model)) +
  geom_col(position = "dodge") +
  labs(
    y = "Predicted probability",
    x = element_blank()
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_fill_manual(values = color_vector) +
  ylim(0, 1)
effect_plot_debt

# Purpose
effect_plot_purpose <- effect_purpose %>% 
  filter(.class == "Loan approved") %>% 
  mutate(
    Model = factor(Model, levels = c("Logistic Regression", "Tree", "Random Forest", "Linear SVM", "Non-Linear SVM"))
  ) %>% 
  ggplot(aes(x = loan_purpose, y = .value, fill = Model)) +
  geom_col(position = "dodge") +
  labs(
    y = element_blank(),
    x = element_blank()
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_fill_manual(values = color_vector) +
  ylim(0, 1)
effect_plot_purpose

# Pre
effect_plot_pre <- effect_pre %>% 
  filter(.class == "Loan approved") %>% 
  mutate(
    Model = factor(Model, levels = c("Logistic Regression", "Tree", "Random Forest", "Linear SVM", "Non-Linear SVM"))
  ) %>% 
  ggplot(aes(x = preapproval, y = .value, fill = Model)) +
  geom_col(position = "dodge") +
  labs(
    y = "Predicted probability",
    x = element_blank()
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_fill_manual(values = color_vector) +
  ylim(0, 1)
effect_plot_pre

# Amount
effect_plot_amount <- effect_amount %>% 
  filter(.class == "Loan approved") %>% 
  mutate(
    Model = factor(Model, levels = c("Logistic Regression", "Tree", "Random Forest", "Linear SVM", "Non-Linear SVM"))
  ) %>% 
  ggplot(aes(x = loan_amount, y = .value, color = Model)) +
  geom_line(linewidth = 1) +
  labs(
    y = "Predicted probability",
    x = element_blank()
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_color_manual(values = color_vector) +
  ylim(0, 1)
effect_plot_amount

# Lien
effect_plot_lien <- effect_lien %>% 
  filter(.class == "Loan approved") %>% 
  mutate(
    Model = factor(Model, levels = c("Logistic Regression", "Tree", "Random Forest", "Linear SVM", "Non-Linear SVM"))
  ) %>% 
  ggplot(aes(x = lien_status, y = .value, fill = Model)) +
  geom_col(position = "dodge") +
  labs(
    y = "Predicted probability",
    x = element_blank()
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_fill_manual(values = color_vector) +
  ylim(0, 1)
effect_plot_lien

# Co
effect_plot_co <- effect_co %>% 
  filter(.class == "Loan approved") %>% 
  mutate(
    Model = factor(Model, levels = c("Logistic Regression", "Tree", "Random Forest", "Linear SVM", "Non-Linear SVM"))
  ) %>% 
  ggplot(aes(x = has_co.applicant, y = .value, fill = Model)) +
  geom_col(position = "dodge") +
  labs(
    y = "Predicted probability",
    x = element_blank()
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_fill_manual(values = color_vector) +
  ylim(0, 1)
effect_plot_co

# Income
effect_plot_income <- effect_income %>% 
  filter(.class == "Loan approved") %>% 
  mutate(
    Model = factor(Model, levels = c("Logistic Regression", "Tree", "Random Forest", "Linear SVM", "Non-Linear SVM"))
  ) %>% 
  ggplot(aes(x = income, y = .value, color = Model)) +
  geom_line(linewidth = 1) +
  labs(
    y = "Predicted probability",
    x = element_blank()
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_color_manual(values = color_vector) +
  ylim(0, 1)
effect_plot_income

# Race
effect_plot_race <- effect_race %>% 
  filter(.class == "Loan approved") %>% 
  mutate(
    Model = factor(Model, levels = c("Logistic Regression", "Tree", "Random Forest", "Linear SVM", "Non-Linear SVM"))
  ) %>% 
  ggplot(aes(x = race, y = .value, fill = Model)) +
  geom_col(position = "dodge") +
  labs(
    y = "Predicted probability",
    x = element_blank()
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_fill_manual(values = color_vector) +
  ylim(0, 1)
effect_plot_race

# Eth
effect_plot_eth <- effect_eth %>% 
  filter(.class == "Loan approved") %>% 
  mutate(
    Model = factor(Model, levels = c("Logistic Regression", "Tree", "Random Forest", "Linear SVM", "Non-Linear SVM"))
  ) %>% 
  ggplot(aes(x = ethnicity, y = .value, fill = Model)) +
  geom_col(position = "dodge") +
  labs(
    y = element_blank(),
    x = element_blank()
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_fill_manual(values = color_vector) +
  ylim(0, 1)
effect_plot_eth

# Sex
effect_plot_sex <- effect_sex %>% 
  filter(.class == "Loan approved") %>% 
  mutate(
    Model = factor(Model, levels = c("Logistic Regression", "Tree", "Random Forest", "Linear SVM", "Non-Linear SVM"))
  ) %>% 
  ggplot(aes(x = sex, y = .value, fill = Model)) +
  geom_col(position = "dodge") +
  labs(
    y = "Predicted probability",
    x = element_blank()
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_fill_manual(values = color_vector) +
  ylim(0, 1)
effect_plot_sex

# Type
effect_plot_type <- effect_type %>% 
  filter(.class == "Loan approved") %>% 
  mutate(
    Model = factor(Model, levels = c("Logistic Regression", "Tree", "Random Forest", "Linear SVM", "Non-Linear SVM"))
  ) %>% 
  ggplot(aes(x = loan_type, y = .value, fill = Model)) +
  geom_col(position = "dodge") +
  labs(
    y = element_blank(),
    x = element_blank()
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_fill_manual(values = color_vector) +
  ylim(0, 1)
effect_plot_type

# Save plots
ggsave(file = "./plots/comparison/debt_pdp.pdf", plot = effect_plot_debt)
ggsave(file = "./plots/comparison/purpose_pdp.pdf", plot = effect_plot_purpose)
ggsave(file = "./plots/comparison/lien_pdp.pdf", plot = effect_plot_lien)
ggsave(file = "./plots/comparison/type_pdp.pdf", plot = effect_plot_type)
ggsave(file = "./plots/comparison/race_pdp.pdf", plot = effect_plot_race)
ggsave(file = "./plots/comparison/eth_pdp.pdf", plot = effect_plot_eth)
