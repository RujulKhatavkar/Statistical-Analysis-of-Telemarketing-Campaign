# Load required libraries
libraries <- c("dplyr", "caret", "tidyverse", "pROC", "ggplot2", "car", "cluster", "factoextra", "NbClust", "fpc")
lapply(libraries, library, character.only = TRUE)

# Load and merge datasets
merged_data <- bind_rows(
  read.csv("C:/Users/Praveena Acharya/Documents/507/Final Project/test.csv", sep = ";"),
  read.csv("C:/Users/Praveena Acharya/Documents/507/Final Project/train.csv", sep = ";")
)

# Rename columns and handle missing values
merged_data <- merged_data %>%
  rename(
    marital_status = marital, credit_default = default, avg_bank_balance = balance,
    housing_loan = housing, personal_loan = loan, contact_type = contact,
    contact_duration = duration, no_of_campaigns = campaign,
    no_of_days_since_last_contact = pdays, previous_no_of_contacts = previous,
    previous_outcome = poutcome, client_subscribed = y
  ) %>%
  mutate_all(~ifelse(. == "", NA, .)) %>%
  mutate(across(c(age, avg_bank_balance, contact_duration, no_of_campaigns, 
                  no_of_days_since_last_contact, previous_no_of_contacts), as.numeric)) %>%
  mutate(across(c(job, marital_status, education, credit_default, housing_loan, 
                  personal_loan, contact_type, month, previous_outcome, client_subscribed), as.factor)) %>%
  mutate(across(where(is.numeric), ~scale(.) %>% as.vector)) %>%
  distinct() %>%
  mutate(client_subscribed = as.integer(client_subscribed == "yes"))

# Prepare data for clustering
cluster_features <- c("age", "avg_bank_balance", "no_of_campaigns", "previous_no_of_contacts", "contact_duration")
cluster_data <- scale(merged_data[, cluster_features])

# Normalize the data
cluster_data_normalized <- scale(cluster_data)
# Determine optimal number of clusters using elbow method
wss <- sapply(1:10, function(k){kmeans(cluster_data_normalized, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


# K-means clustering
set.seed(123)
kmeans_result <- kmeans(cluster_data, centers = 6, nstart = 25)
merged_data$kmeans_cluster <- kmeans_result$cluster
# Visualize clusters
fviz_cluster(kmeans_result, data = cluster_data_normalized,
             geom = "point", ellipse.type = "convex",
             palette = "jco", ggtheme = theme_minimal())
#k-means end

# Hierarchical clustering
hc_result <- hclust(dist(cluster_data), method = "ward.D2")
merged_data$hc_cluster <- cutree(hc_result, k = 6)
# Plot dendrogram
plot(hc_result, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "")

# Cut the dendrogram to create clusters
hc_clusters <- cutree(hc_result, k = 6)

# Visualize clusters
fviz_cluster(list(data = cluster_data_normalized, cluster = hc_clusters),
             geom = "point", ellipse.type = "convex",
             palette = "jco", ggtheme = theme_minimal())
#hierarchical clustering end

# Subscription rates by cluster
ggplot(merged_data %>% 
         group_by(kmeans_cluster) %>% 
         summarise(subscription_rate = mean(client_subscribed)),
       aes(x = factor(kmeans_cluster), y = subscription_rate, fill = factor(kmeans_cluster))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f%%", subscription_rate * 100)), vjust = -0.5) +
  labs(title = "Subscription Rates by K-means Cluster", x = "Cluster", y = "Subscription Rate") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# Cluster centroids heatmap
ggplot(tidyr::pivot_longer(as.data.frame(kmeans_result$centers) %>% 
                             mutate(Cluster = row_number()), 
                           cols = -Cluster, names_to = "Feature", values_to = "Value"),
       aes(x = Feature, y = factor(Cluster), fill = Value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Cluster Centroids Heatmap", x = "Features", y = "Cluster")

# Feature importance
ggplot(data.frame(
  Feature = colnames(cluster_data),
  Importance = apply(cluster_data, 2, function(x) sum(abs(x)))
) %>% arrange(desc(Importance)),
aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance for K-means Clustering", x = "Features", y = "Importance") +
  theme_minimal()

# Subscription rate heatmap
ggplot(merged_data %>%
         mutate(age_bin = cut(age, breaks = 10),
                balance_bin = cut(avg_bank_balance, breaks = 10)) %>%
         group_by(age_bin, balance_bin) %>%
         summarise(subscription_rate = mean(client_subscribed), .groups = "drop"),
       aes(x = age_bin, y = balance_bin, fill = subscription_rate)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(title = "Subscription Rate by Age and Average Bank Balance", 
       x = "Age", y = "Average Bank Balance", fill = "Subscription Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
