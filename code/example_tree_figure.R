
library(rpart)
library(rpart.plot)

# Read data
data <- readRDS("./data/credit_approval_data.rds")

# Set seed for reproducibility
set.seed(123)

# Train Tree
tree <- rpart(action_taken ~ ., data = data, method = "class")

# Plot Tree
example <- rpart.plot(tree, extra = 4, yesno = 2, fallen.leaves = TRUE, split.border.col = 0)
example