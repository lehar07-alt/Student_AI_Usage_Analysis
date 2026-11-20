
# ============================================================
# OBJECTIVE 4: DECISION TREE CLASSIFICATION
# ============================================================

library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)

students <- readRDS(
  "processed_data/cleaned_students.rds"
)

# 1. Create target variable
students <- students %>%
  mutate(
    impact_category = factor(
      impact_category,
      levels = c("Negative", "Neutral", "Positive")
    )
  )

# 2. Select predictors and target
model_data <- students %>%
  select(
    impact_category,
    daily_usage_hours,
    trust_in_ai_tools,
    awareness_level,
    number_of_ai_tools,
    stream,
    year_of_study,
    preferred_ai_tool,
    do_professors_allow_use,
    willing_to_pay_for_access,
    device_used,
    internet_access
  ) %>%
  drop_na()

# 3. Split into training and testing sets
set.seed(123)

train_index <- createDataPartition(
  model_data$impact_category,
  p = 0.80,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# 4. Train decision tree
tree_model <- rpart(
  impact_category ~ .,
  data = train_data,
  method = "class",
  control = rpart.control(
    cp = 0.01,
    minsplit = 20,
    maxdepth = 5
  )
)

# 5. Display decision tree
rpart.plot(
  tree_model,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Decision Tree for Perceived Grade Impact"
)

# 6. Make predictions
predictions <- predict(
  tree_model,
  test_data,
  type = "class"
)

# 7. Evaluate model
confusion <- confusionMatrix(
  data = factor(
    predictions,
    levels = levels(test_data$impact_category)
  ),
  reference = test_data$impact_category
)

print(confusion)

# 8. Extract performance measures
accuracy <- confusion$overall["Accuracy"]

precision <- confusion$byClass[, "Precision"]
recall <- confusion$byClass[, "Recall"]
f1 <- confusion$byClass[, "F1"]

cat("\nAccuracy:", round(accuracy, 4), "\n")
cat("\nClass-wise Precision:\n")
print(precision)

cat("\nClass-wise Recall:\n")
print(recall)

cat("\nClass-wise F1 Score:\n")
print(f1)

# 9. Save predictions
prediction_results <- data.frame(
  Actual = test_data$impact_category,
  Predicted = predictions
)

write.csv(
  prediction_results,
  "processed_data/decision_tree_predictions.csv",
  row.names = FALSE
)

# 10. Save confusion matrix
write.csv(
  as.data.frame(confusion$table),
  "processed_data/decision_tree_confusion_matrix.csv",
  row.names = FALSE
)

cat("Decision tree classification completed.\n")
