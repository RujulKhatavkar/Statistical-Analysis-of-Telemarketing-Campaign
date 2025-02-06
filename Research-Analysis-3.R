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

#Research Question - 3

# Create survival object
surv_object <- Surv(time = data$no_of_days_since_last_contact, event = data$client_subscribed)

# Kaplan-Meier survival curve
km_fit <- survfit(surv_object ~ 1, data = data)
summary(km_fit)
ggsurvplot(km_fit, data = data, conf.int = TRUE, title = "Kaplan-Meier Survival Curve")

# Stratified Kaplan-Meier curve by 'poutcome'
km_fit_strat <- survfit(surv_object ~ previous_outcome, data = data)
summary(km_fit_strat)
ggsurvplot(km_fit_strat, data = data, title = "Kaplan-Meier by poutcome")

# Cox proportional hazards model
cox_model <- coxph(surv_object ~ no_of_days_since_last_contact + previous_no_of_contacts + previous_outcome, data = data)
summary(cox_model)
ggforest(cox_model,data=data)
