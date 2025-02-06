library(dplyr)
library(reticulate)
library(caret)
library(tidyverse)
library(pROC)
library(ggplot2)
library(car)
library(survival)
library(survminer)
library(cluster)

## DATA PREPROCESSING

# Load the dataset with the correct delimiter
test_dataset <- read.csv("C:/Users/Praveena Acharya/Documents/507/Final Project/test.csv", sep = ";", header = TRUE)
train_dataset <- read.csv("C:/Users/Praveena Acharya/Documents/507/Final Project/train.csv", sep = ";", header = TRUE)
merged_data <- bind_rows(test_dataset, train_dataset)
write.csv(merged_data, "merged_data.csv")
data <- read.csv("merged_data.csv", sep = ",", header = TRUE)

# Check for missing values
colSums(is.na(data))

# Replace empty strings with NA
data <- data.frame(lapply(data, function(x) {
  if (is.character(x) | is.factor(x)) {
    x[x == ""] <- NA
  }
  return(x)
}))

# Rename column names
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

# Create additional features for contact strategy
data$previous_contacts <- ifelse(data$previous_no_of_contacts > 0, 1, 0)
data$campaign_intensity <- cut(data$no_of_campaigns, breaks = c(0, 1, 5, 10, Inf),
                               labels = c("Low", "Medium", "High", "Very High"))

# Convert numeric columns as numeric
numeric_cols <- c("age", "avg_bank_balance", "contact_duration", "no_of_campaigns", "no_of_days_since_last_contact", "previous_no_of_contacts")
data[numeric_cols] <- lapply(data[numeric_cols], as.numeric)
# Convert categorical columns to factors
categorical_cols <- c("job", "marital_status", "education", "credit_default", "housing_loan", "personal_loan", "contact_type", "month", "previous_outcome", "client_subscribed")
data[categorical_cols] <- lapply(data[categorical_cols], as.factor)
# Z-score normalization
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}
data[numeric_cols] <- lapply(data[numeric_cols], standardize)

# Remove duplicate rows
data <- unique(data)

# Convert 'y' to binary (0/1)
data$client_subscribed <- ifelse(data$client_subscribed == "yes", 1, 0)

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(data$client_subscribed, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

################################################################################
# Research Question - 1
## MODEL FITTING

# Logistic regression model
model <- glm(client_subscribed ~ age + job + marital_status + education + credit_default + avg_bank_balance + housing_loan + personal_loan + contact_type + contact_duration + no_of_campaigns + no_of_days_since_last_contact + previous_no_of_contacts + previous_outcome, 
             data = train_data, family = binomial())

# Summary of the model
summary(model)

## ODDS RATIOS

# Extracting odds ratios and confidence intervals
odds_ratios <- exp(coef(model))
conf_int <- exp(confint(model))

# Combine odds ratios and confidence intervals
odds_ratios_df <- data.frame(
  Variable = names(odds_ratios),
  Odds_Ratio = odds_ratios,
  CI_Lower = conf_int[, 1],
  CI_Upper = conf_int[, 2]
)

# Separate job categories
odds_ratios_df$Variable <- gsub("job([a-z])", "job : \\L\\1", 
                                odds_ratios_df$Variable, 
                                perl = TRUE)
# Separate marital status
odds_ratios_df$Variable <- gsub("marital_status([a-z]+)", 
                                "marital status : \\1", 
                                odds_ratios_df$Variable)
# Separate education level
odds_ratios_df$Variable <- gsub("education([a-z]+)", 
                                "education : \\1", 
                                odds_ratios_df$Variable)
# Separate credit default
odds_ratios_df$Variable <- gsub("credit_default([a-z]+)", 
                                "credit default : \\1", 
                                odds_ratios_df$Variable)
# Separate loan types
odds_ratios_df$Variable <- gsub("([a-z]+)_loan([a-z]+)", 
                                "\\1 loan : \\2", 
                                odds_ratios_df$Variable)
# Separate contact duration
odds_ratios_df$Variable <- gsub("contact_type([a-z]+)", 
                                "contact type : \\1", 
                                odds_ratios_df$Variable)
# Separate previous outcome
odds_ratios_df$Variable <- gsub("previous_outcome([a-z]+)", 
                                "previous outcome : \\1", 
                                odds_ratios_df$Variable)
# Display results
print(odds_ratios_df)

## VISUALIZATION

# Visualizing significant predictors
top_factors <- odds_ratios_df %>%
  filter(!Variable %in% c("(Intercept)")) %>%
  arrange(desc(abs(log(Odds_Ratio)))) %>%
  head(10)

ggplot(top_factors, aes(x = reorder(Variable, log(Odds_Ratio)), y = log(Odds_Ratio))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = log(CI_Lower), ymax = log(CI_Upper)), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Significant Predictors for Term Deposit Subscription",
       x = "Predictor Variables", y = "Log Odds Ratio")

## MODEL EVALUATION

# Assess model performance
# Predict on test data
predictions <- predict(model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Evaluate the model
conf_matrix <- confusionMatrix(factor(predicted_classes), factor(test_data$client_subscribed))
print(conf_matrix)

# Check for multicollinearity
vif_values <- vif(model)
print(vif_values)

# Calculate ROC curve and AUC
roc_curve <- roc(test_data$client_subscribed, predictions)
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "steelblue")
text(0.8, 0.2, paste("AUC =", round(auc_value, 3)))