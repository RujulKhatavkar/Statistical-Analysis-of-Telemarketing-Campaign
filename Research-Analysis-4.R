# Install necessary packages (if not already installed)
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("caret")) install.packages("caret")
if (!require("broom")) install.packages("broom")

# Load libraries
library(tidyverse)
library(caret)
library(broom)

install.packages(c('readr', 'dplyr'))
library(readr)
library(dplyr)

# Load necessary libraries
if (!require("randomForest")) install.packages("randomForest")

library(randomForest)
library(caret)
library(pROC)
library(ggplot2)

# Load the data
data <- read.csv("merged_data.csv", sep = ",", header = TRUE)

# Rename variables
data <- data %>%
  rename(
    marital_status = marital,
    credit_default = default,
    avg_bank_balance = balance,
    housing_loan = housing,
    personal_loan = loan,
    contact_type = contact,
    contact_duration = duration,
    no_of_campaigns = campaign,
    no_of_days_since_last_contact = pdays,
    previous_no_of_contacts = previous,
    previous_outcome = poutcome,
    client_subscribed = y
  )
head(data)

## Convert categorical variables to factors
categorical_vars <- c("job", "marital_status", "education", "credit_default",
                      "housing_loan", "personal_loan", "contact_type",
                      "month", "previous_outcome", "client_subscribed")
data[categorical_vars] <- lapply(data[categorical_vars], as.factor)

# Create features for contact strategy
# data$previous_contacts <- ifelse(data$previous > 0, 1, 0)
# data$campaign_intensity <- cut(data$campaign, breaks = c(0, 1, 5, 10, Inf), labels = c("Low", "Medium", "High", "Very High"))
# Create additional features for contact strategy
data$previous_contacts <- ifelse(data$previous_no_of_contacts > 0, 1, 0)
data$campaign_intensity <- cut(data$no_of_campaigns, breaks = c(0, 1, 5, 10, Inf),
                               labels = c("Low", "Medium", "High", "Very High"))

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(data$client_subscribed, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train a Random Forest model including 'age'
rf_model <- randomForest(client_subscribed ~ age + avg_bank_balance + no_of_campaigns +
                           previous_no_of_contacts + previous_outcome + month + day +
                           contact_type + campaign_intensity + previous_contacts +
                           contact_duration,
                         data = train_data, ntree = 500, importance = TRUE)

# Make predictions on the test set
predictions <- predict(rf_model, test_data, type = "prob")[, 2]

# Evaluate model performance (Confusion Matrix and ROC-AUC)
conf_matrix <- confusionMatrix(predict(rf_model, test_data), test_data$client_subscribed)
roc_curve <- roc(test_data$client_subscribed, predictions)
auc_value <- auc(roc_curve)

head(train_data)
head(test_data)

# Plot Feature Importance
importance_df <- as.data.frame(importance(rf_model))
importance_df$feature <- rownames(importance_df)
ggplot(importance_df, aes(x = reorder(feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Importance", x = "Features", y = "Importance") +
  theme_minimal()

# Plot ROC Curve
plot(roc_curve, col = "#FF4500", lwd = 2, main = "Receiver Operating Characteristic (ROC) Curve")
abline(a=0, b=1, lty=2) # Add diagonal line for random guessing
text(0.5, 0.5, paste("AUC =", round(auc_value, 3)))

# Visualize Confusion Matrix
cm_data <- as.data.frame(conf_matrix$table)
colnames(cm_data) <- c("True_Label", "Predicted_Label", "Frequency")
ggplot(cm_data, aes(x = True_Label, y = Predicted_Label)) +
  geom_tile(aes(fill = Frequency), color = "white") +
  geom_text(aes(label = Frequency), color = "black") +
  scale_fill_gradient(low = "#D6EAF8", high = "#2874A6") +
  labs(title = "Confusion Matrix", x = "True Label", y = "Predicted Label") +
  theme_minimal()

# Partial Dependence Plots for Key Variables
par(mfrow=c(2,3))
partialPlot(rf_model, train_data, "age", "yes")
partialPlot(rf_model, train_data, "avg_bank_balance", "yes")
partialPlot(rf_model, train_data, "no_of_campaigns", "yes")
partialPlot(rf_model, train_data, "contact_duration", "yes")
partialPlot(rf_model, train_data, "month", "yes")
partialPlot(rf_model, train_data, "previous_no_of_contacts", "yes")


# Extract Metrics from Confusion Matrix
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1_score <- 2 * ((precision * recall) / (precision + recall))

# Print Final Scores
cat("Final Model Performance Metrics:\n")
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1-Score:", round(f1_score, 3), "\n")
cat("AUC:", round(auc_value, 3), "\n")