# Install and load necessary libraries if not installed
if (!require(dplyr)) {
  install.packages("dplyr")
}
if (!require(caret)) {
  install.packages("caret")
}
if (!require(randomForest)) {
  install.packages("randomForest")
}

# Load libraries
library(dplyr)
library(caret)
library(randomForest)

# Load the dataset
credit_data <- read.csv("creditcard.csv")

# Handle missing values (if any)
credit_data <- na.omit(credit_data)

# Split the data into training and testing sets
set.seed(123)
train_indices <- createDataPartition(credit_data$Class, p = 0.8, list = FALSE)
train_data <- credit_data[train_indices, ]
test_data <- credit_data[-train_indices, ]

# Train a random forest model
fraud_model <- randomForest(Class ~ ., data = train_data, ntree = 100)

# Make predictions on the test set
predictions <- predict(fraud_model, newdata = test_data)

# Create confusion matrix using table function
conf_matrix_formatted <- matrix(c(conf_matrix[1,1], conf_matrix[2,1], conf_matrix[1,2], conf_matrix[2,2]),
                                nrow = 2, byrow = TRUE,
                                dimnames = list(Actual = c("Non-Fraud", "Fraud"),
                                                Predicted = c("Non-Fraud", "Fraud")))

# Print the formatted confusion matrix
cat("-------------------------\n")
cat("Credit Card Fraud Detection\n")
cat("-------------------------\n\n")
cat("--- Confusion Matrix ---\n")
cat("         Predicted\n")
cat("Actual   Non-Fraud  Fraud\n")
cat("----------------------------\n")
cat("Non-Fraud   ", conf_matrix_formatted[1,1], "     ", conf_matrix_formatted[1,2], "\n")
cat("Fraud       ", conf_matrix_formatted[2,1], "      ", conf_matrix_formatted[2,2], "\n")

# Calculate additional metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print additional metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")
