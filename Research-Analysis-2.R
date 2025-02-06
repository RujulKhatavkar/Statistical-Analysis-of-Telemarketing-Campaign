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

#Research Question - 2

data_subset <- data[, c("job", "education", "marital_status", "client_subscribed")]
# Create a contingency table for job and subscription outcome
table_job <- table(data_subset$job, data_subset$client_subscribed)

# Perform Chi-Square test
chi_square_job <- chisq.test(table_job)

# Check the results
chi_square_job

ggplot(as.data.frame(table_job), aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity") +
  labs(x = "Job", y = "Frequency", fill = "Subscription Outcome") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  ggtitle("Relationship Between Job and Subscription Outcome") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# For education and subscription outcome
table_education <- table(data_subset$education, data_subset$client_subscribed)
chi_square_education <- chisq.test(table_education)
chi_square_education

# Create a stacked bar plot
library(ggplot2)
ggplot(as.data.frame(table_education), aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity") +
  labs(x = "Education", y = "Frequency", fill = "Subscription Outcome") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  ggtitle("Relationship Between Education and Subscription Outcome") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# For marital status and subscription outcome
table_marital <- table(data_subset$marital, data_subset$client_subscribed)
chi_square_marital <- chisq.test(table_marital)
chi_square_marital

# Create a contingency table for marital status and subscription outcome
table_marital <- table(data$marital, data$client_subscribed)

# Create a stacked bar plot
library(ggplot2)
ggplot(as.data.frame(table_marital), aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity") +
  labs(x = "Marital Status", y = "Frequency", fill = "Subscription Outcome") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  ggtitle("Relationship Between Marital Status and Subscription Outcome") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))