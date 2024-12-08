# Load the necessary libraries (install if not available)
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("caret")) install.packages("caret")
if (!require("Matrix")) install.packages("Matrix")

# Load the data. The data is in the data folder in a zipper format. 
# If you want to run the code, you need to unzip the file first. 
# Keep the unzipped file also in the data folder.
hunters_data <- 
  read.csv("data/ECommerce_consumer behaviour.csv")

# General look at the data
hunters_data |> glimpse()

# Exploratory data analysis ----

# Number of missing values and unique values for order_id
cat("Number of missing values:", sum(is.na(hunters_data$order_id)), "\n")
cat("Number of unique values:", unique(hunters_data$order_id) |> length(), "\n")

# Number of missing values and unique values for user_id
cat("Number of missing values:", sum(is.na(hunters_data$user_id)), "\n")
cat("Number of unique values:", unique(hunters_data$user_id) |> length(), "\n")

# Plot the number of orders by user
hunters_data |> 
  distinct(user_id, order_id) |>
  count(user_id) |>
  count(n) |>
  arrange(n) |> 
  ggplot(aes(x = n, y = nn)) +
  geom_col() +
  labs(title = "Number of Users by Orders", x = "Number of Orders", y = "Number of Users") +
  theme_minimal()

# Number of missing values and unique values for order_number
cat("Number of missing values:", sum(is.na(hunters_data$order_number)), "\n")
cat("Number of unique values:", unique(hunters_data$order_number) |> length(), "\n")

# Look at the data to check if the order_number is sequential for a user
hunters_data |> 
  filter(user_id == 23986) |>
  arrange(order_number) |>
  select(1:4, 6) |>
  head(10) 

# Plot the number of total orders by user
hunters_data |> 
  group_by(user_id) |>
  summarize(max_orders = max(order_number)) |>
  count(max_orders) |> 
  ggplot(aes(x = max_orders, y = n)) +
  geom_col() +
  labs(title = "Number of Total Orders by User", x = "Number of Total Orders", y = "Number of Users") +
  theme_minimal()

# Number of missing values and unique values for order_dow
# List of unique values for order_dow
cat("Number of missing values:", sum(is.na(hunters_data$order_dow)), "\n")
cat("Number of unique values:", unique(hunters_data$order_dow) |> length(), "\n")
unique(hunters_data$order_dow) |> sort()

# Replace the numbers with the corresponding days of the week
hunters_data <- 
  hunters_data |> 
  mutate(order_dow = recode_factor(
    order_dow,
    `0` = "Monday",
    `1` = "Tuesday",
    `2` = "Wednesday",
    `3` = "Thursday",
    `4` = "Friday",
    `5` = "Saturday",
    `6` = "Sunday"
  ))

# Plot the number of items purchased per day
hunters_data |> 
  count(order_dow) |> 
  ggplot(aes(x = order_dow, y = n)) +
  geom_col() +
  labs(title = "Number of Items Purchased per Day", x = "", y = "Number of Items") +
  theme_minimal()

# Plot the number of orders placed per day
hunters_data |> 
  distinct(order_id, order_dow) |> 
  count(order_dow) |>
  ggplot(aes(x = order_dow, y = n)) +
  geom_col() +
  labs(title = "Number of Orders Placed per Day", x = "", y = "Number of Orders") +
  theme_minimal()

# Number of missing values and unique values for order_hour_of_day
# List of unique values for order_hour_of_day
cat("Number of missing values:", sum(is.na(hunters_data$order_hour_of_day)), "\n")
cat("Number of unique values:", unique(hunters_data$order_hour_of_day) |> length(), "\n")
unique(hunters_data$order_hour_of_day) |> sort()

# Plot the number of items purchased per hour
hunters_data |> 
  count(order_hour_of_day) |> 
  ggplot(aes(x = order_hour_of_day, y = n)) +
  geom_col() +
  labs(title = "Number of Items Purchased per Hour", x = "", y = "Number of Items") +
  theme_minimal()

# Plot the number of orders placed per hour
hunters_data |> 
  distinct(order_id, order_hour_of_day) |> 
  count(order_hour_of_day) |>
  ggplot(aes(x = order_hour_of_day, y = n)) +
  geom_col() +
  labs(title = "Number of Orders Placed per Hour", x = "", y = "Number of Orders") +
  theme_minimal()

# Number of missing values and unique values for days_since_prior_order
# List of unique values for days_since_prior_order
cat("Number of missing values:", sum(is.na(hunters_data$days_since_prior_order)), "\n")
cat("Number of unique values:", unique(hunters_data$days_since_prior_order) |> length(), "\n")
unique(hunters_data$days_since_prior_order) |> sort()

# Plot the number of orders placed by days since prior order
hunters_data |> 
  distinct(order_id, days_since_prior_order) |> 
  count(days_since_prior_order) |>
  ggplot(aes(x = days_since_prior_order, y = n)) +
  geom_col() +
  labs(title = "Number of Orders Placed by Days Since Prior Order", x = "", y = "Number of Orders") +
  theme_minimal()

# Number of missing values and unique values for product_id
cat("Number of missing values:", sum(is.na(hunters_data$product_id)), "\n")
cat("Number of unique values:", unique(hunters_data$product_id) |> length(), "\n")

# Plot the number of products in an order
hunters_data |> 
  count(order_id) |>
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Number of Products in an Order", x = "Number of Products", y = "Number of Orders") +
  theme_minimal()

# Number of missing values and unique values for add_to_cart_order
# List of unique values for add_to_cart_order
cat("Number of missing values:", sum(is.na(hunters_data$add_to_cart_order)), "\n")
cat("Number of unique values:", unique(hunters_data$add_to_cart_order) |> length(), "\n")
unique(hunters_data$add_to_cart_order)

# Plot the number of orders placed by cart size
hunters_data |> 
  group_by(order_id) |> 
  summarize(cart_size = max(add_to_cart_order)) |>
  count(cart_size) |>
  ggplot(aes(x = cart_size, y = n)) +
  geom_col() +
  labs(title = "Number of Orders Placed by Cart Size", x = "Cart Size", y = "Number of Items") +
  theme_minimal()

# Number of missing values and unique values for reordered
# List of unique values for reordered
cat("Number of missing values:", sum(is.na(hunters_data$reordered)), "\n")
cat("Number of unique values:", unique(hunters_data$reordered) |> length(), "\n")
unique(hunters_data$reordered)

# Number of missing values and unique values for department_id
# List of unique values for department_id
cat("Number of missing values:", sum(is.na(hunters_data$department_id)), "\n")
cat("Number of unique values:", unique(hunters_data$department_id) |> length(), "\n")
unique(hunters_data$department_id) |> sort()

# Number of missing values and unique values for department
# List of unique values for department
cat("Number of missing values:", sum(is.na(hunters_data$department)), "\n")
cat("Number of unique values:", unique(hunters_data$department) |> length(), "\n")
unique(hunters_data$department) |> sort()

# Plot the number of orders by department
hunters_data |> 
  distinct(order_id, department) |> 
  count(department) |>
  ggplot(aes(x = reorder(department, n), y = n)) + 
  geom_col() +
  labs(title = "Number of Orders by Department", x = "", y = "Number of Orders") +
  theme_minimal() +
  coord_flip() 

# Number of missing values and unique values for product_name
# List of unique values for product_name
cat("Number of missing values:", sum(is.na(hunters_data$product_name)), "\n")
cat("Number of unique values:", unique(hunters_data$product_name) |> length(), "\n")
unique(hunters_data$product_name) |> sort()

# Plot the number of orders by product
hunters_data |> 
  count(product_name) |>
  filter(n > 10000) |> 
  ggplot(aes(x = reorder(product_name, n), y = n)) + 
  geom_col() +
  labs(title = "Number of Products", x = "", y = "Number of Orders") +
  theme_minimal() +
  coord_flip() 

# Plot the products that are ordered less than 2,000 times
hunters_data |> 
  count(product_name) |>
  filter(n < 2000) |> 
  ggplot(aes(x = reorder(product_name, desc(n)), y = n)) + 
  geom_col() +
  labs(title = "Number of Products", x = "", y = "Number of Orders") +
  theme_minimal() +
  coord_flip() 

# Models ----

## Prepare Training and Test Data ----

# Set seed for reproducibility
set.seed(1)
# Create training and test data with 10 percent of the data as test data
test_index <- createDataPartition(y = hunters_data$order_id, p = 0.1, list = FALSE)

train_data0 <- hunters_data[-test_index, ] # 90% of the data
test_data0 <- hunters_data[test_index, ]   # 10% of the data

# Ensure `order_id`s and `user_id`s in test_data are also in train_data
# In this project this is not necessary as we are not using the user_id variable, but in case the model would be improved by using this information in the future, this step would be necessary.
test_data <- test_data0 |>
  semi_join(train_data0, by = "order_id") |>
  semi_join(train_data0, by = "user_id")

# Add back any rows that were removed from test_data0 to `train_data`
removed_rows <- anti_join(test_data0, test_data)
train_data <- bind_rows(train_data0, removed_rows)

# Clean up temporary variables
rm(train_data0, test_data0, removed_rows)

## Model with Departments ----

# Generate a vector of unique product_ids
product_list <- 
  hunters_data |> 
  select(product_name) |> 
  unique() |> 
  pull()

# Generate clusters based on department
clusters <- 
  train_data |> 
  select(product_name, department) |> 
  distinct() |> 
  mutate(cluster_id = as.numeric(factor(department))) # Assign cluster IDs

# Convert clusters to a named vector with cluster ids and product names
clusters <- 
  clusters |> 
  arrange(cluster_id) |> 
  select(product_name, cluster_id) |> 
  deframe()

### Evaluation ----

# Create a order_product_test data frame with distinct values of order_id and product_name
order_product_test <- 
  test_data |> 
  select(order_id, product_name) |> 
  distinct() |> 
  mutate(product_name = factor(product_name, levels = product_list),
         order_id = factor(order_id))

# Create product by order matrix for test data
matrix_test <- sparseMatrix(
  i = as.integer(order_product_test$order_id), 
  j = as.integer(order_product_test$product_name),
  x = 1,
  dims = c(length(unique(order_product_test$order_id)), length(product_list))
)

# Create co-occurrence matrix for the test data
co_occurrence_test <- t(matrix_test) %*% matrix_test
# Select product pairs with co-occurrence
pairs_test <- which(co_occurrence_test > 0, arr.ind = TRUE)
# We drop out pairs of the same product
pairs_test <- pairs_test[pairs_test[, 1] != pairs_test[, 2], ]

# Prepare data to compare co-occurrences in test data with train clusters
# First set the count of co-occurrences aligned with clusters to 0 for a starting point
same_cluster_count <- 0
# Get the total number of pairs
pairs_total <- nrow(pairs_test)

# Rename clusters to train_clusters to be used in the evaluation
train_clusters <- clusters

# Compare co-occurrences in test data with train clusters
for (i in 1:pairs_total) {
  prod1 <- pairs_test[i, 1]
  prod2 <- pairs_test[i, 2]
  if (train_clusters[prod1] == train_clusters[prod2]) {
    same_cluster_count <- same_cluster_count + 1
  }
}

# Get the proportion of test co-occurrences aligned with clusters
alignment_score <- same_cluster_count / pairs_total
cat("Proportion of test co-occurrences aligned with clusters:", alignment_score)

## Model with Simple Clustering ----

# Create a order_product_train data frame with distinct values of order_id and product_name
order_product_train <- 
  train_data |> 
  select(order_id, product_name) |> 
  distinct() |> 
  mutate(product_name = factor(product_name, levels = product_list),
         order_id = factor(order_id))
# Create product by order matrix for training data
matrix_train <- 
  sparseMatrix(
    i = as.integer(order_product_train$order_id), 
    j = as.integer(order_product_train$product_name),
    x = 1,
    dims = c(length(unique(order_product_train$order_id)), length(product_list))
)
# Create co-occurrence matrix for the training data
co_occurrence_train <- t(matrix_train) %*% matrix_train

# Calculate similarity scores with Jaccard index
freq_train <- diag(co_occurrence_train)

# Compute Jaccard similarity for each pair
# co_occurrence[i,j] / (freq[i] + freq[j] - co_occurrence[i,j])
jaccard_train <- matrix(0, nrow=length(product_list), ncol=length(product_list))
for (i in seq_along(product_list)) {
  for (j in seq_along(product_list)) {
    numerator <- co_occurrence_train[i,j]
    denominator <- freq_train[i] + freq_train[j] - numerator
    if (denominator > 0) {
      jaccard_train[i,j] <- numerator / denominator
    } else {
      jaccard_train[i,j] <- 0
    }
  }
}

# Do the clustering
distance_matrix_train <- as.dist(1 - jaccard_train)

hc_train <- hclust(distance_matrix_train, method = "ward.D2")

# Create 21 clusters
k <- 21
train_clusters <- cutree(hc_train, k = k) 

# Plot the cluster sizes for the obtained clusters
cluster_summary <- table(train_clusters)
data.frame(cluster_summary) |> 
  ggplot(aes(x = factor(1:k), y = Freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Cluster Sizes", x = "Clusters", y = "Number of Products") +
  theme_minimal()

### Evaluation ----

# Prepare data to compare co-occurrences in test data with train clusters
# First set the count of co-occurrences aligned with clusters to 0 for a starting point
same_cluster_count <- 0
# Get the total number of pairs
pairs_total <- nrow(pairs_test)

for (i in 1:pairs_total) {
  prod1 <- pairs_test[i, 1]
  prod2 <- pairs_test[i, 2]
  if (train_clusters[prod1] == train_clusters[prod2]) {
    same_cluster_count <- same_cluster_count + 1
  }
}

# Get the proportion of test co-occurrences aligned with clusters
alignment_score <- same_cluster_count / pairs_total
cat("Proportion of test co-occurrences aligned with clusters:", alignment_score)

## Model with Principal Component Analysis ----

# Prepare the data for PCA
co_occurrence_scaled <- scale(co_occurrence_train)

# Perform PCA
pca_result <- prcomp(co_occurrence_scaled, scale. = FALSE)

# Create explained variance
explained_variance <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)

# Restrict the number of components to those that explain 90 percent of the variance
num_components <- which(explained_variance >= 0.9)[1]  
pca_coordinates <- pca_result$x[, 1:num_components]

# Cluster the products using the coordinates resulting from PCA
set.seed(1)
train_pca_clusters <- kmeans(pca_coordinates, centers = 21, nstart = 10)$cluster

### Evaluation ----

# Prepare data to compare co-occurrences in test data with train clusters
# First set the count of co-occurrences aligned with clusters to 0 for a starting point
same_cluster_count <- 0
# Get the total number of pairs
pairs_total <- nrow(pairs_test)

for (i in 1:pairs_total) {
  prod1 <- pairs_test[i, 1]
  prod2 <- pairs_test[i, 2]
  if (train_pca_clusters[prod1] == train_pca_clusters[prod2]) {
    same_cluster_count <- same_cluster_count + 1
  }
}

# Get the proportion of test co-occurrences aligned with clusters
alignment_score_pca <- same_cluster_count / pairs_total
cat("Proportion of test co-occurrences aligned with PCA clusters:", alignment_score_pca, "\n")

## Model with 10 Clusters ----

# We already have the distances, so only change the number of clusters
set.seed(1)
train_pca_clusters <- kmeans(pca_coordinates, centers = 10, nstart = 10)$cluster

### Evaluation ----

# Prepare data to compare co-occurrences in test data with train clusters
# First set the count of co-occurrences aligned with clusters to 0 for a starting point
same_cluster_count <- 0
# Get the total number of pairs
pairs_total <- nrow(pairs_test)

for (i in 1:pairs_total) {
  prod1 <- pairs_test[i, 1]
  prod2 <- pairs_test[i, 2]
  if (train_pca_clusters[prod1] == train_pca_clusters[prod2]) {
    same_cluster_count <- same_cluster_count + 1
  }
}

# Get the proportion of test co-occurrences aligned with clusters
alignment_score_pca <- same_cluster_count / pairs_total
cat("Proportion of test co-occurrences aligned with PCA clusters:", alignment_score_pca, "\n")

