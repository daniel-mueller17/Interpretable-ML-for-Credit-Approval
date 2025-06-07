
library(rpart)
library(rpart.plot)

# Read data
data <- readRDS("./data/credit_approval_data.rds")

# Set seed for reproducibility
set.seed(123)

# Train Tree
tree <- rpart(action_taken ~ ., data = data, method = "class")

# Plot Tree
rpart.plot(tree, main = "Decision Tree", extra = 4, yesno = 2, fallen.leaves = TRUE, split.border.col = 0)