
library(tidyverse)
library(caret)
library(leaflet)
library(dplyr)
library(psych)
library(corrplot)
library(tidygeocoder)
library(ggplot2)
library(stats)
library(shiny)

################### 1. Importing Data ####################

recession_indicator <- read.csv("./Data/CanadaRecessionIndicator.csv")
rates <-read.csv("./Data/clean_rates.csv")
home_prices <- read.csv("./Data/GTA_HomePrice_History.csv")
primary_keys <- read.csv("./prim_key.csv")



###################### Exploratory Data Analysis and Data Processing ############################

# Missing Values
# Duplicates
# Number of rows and columns


data_q_report <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # 1. View Columns
  cat("Column Names:\n")
  print(colnames(data))

  # 2. Check for Nulls
  null_counts <- sapply(data, function(x) sum(is.na(x)))
  cat("\nNull counts per column:\n")
  print(null_counts)

  # Identify columns with any null values
  columns_with_nulls <- colnames(data)[apply(data, 2, anyNA)]
  cat("\nColumns with null values:\n")
  print(columns_with_nulls)

  # 3. Check for Duplicates
  duplicates <- data[duplicated(data), ]
  cat("\nDuplicate rows:\n")
  print(duplicates)

  # 4. Summary Report
  cat("\nSummary Report:\n")
  cat("Total Columns:", ncol(data), "\n")
  cat("Total Rows:", nrow(data), "\n")
  cat("Columns with Nulls:", paste(columns_with_nulls, collapse = ", "), "\n")
  cat("Number of Duplicate Rows:", nrow(duplicates), "\n")
}



data_list <- list(recession_data=recession_indicator, homePrices = home_prices, primaryKeys = primary_keys, interestR=rates)

for (data in names(data_list)) {
  cat("\nData Quality report for", data, ":\n")
  data_q_report(data_list[[data]])
}




# Merging the datasets:
prices_recession <- inner_join(home_prices, recession_indicator, by = "Year_Quarter_Key")
merged_data <- inner_join(prices_recession, rates, by = "Year_Quarter_Key")

# sanity check:
head(merged_data)

# data quality report for the merged data:
data_q_report(merged_data)


# Selecting the columns:

selected_data <- merged_data %>%
  select(-1,-3,-4,-5,-7,-12,-14,-16,-17,-18)

# sanity check
head(selected_data)

# renaming all the rows with values starting with Toronto as Toronto:
final_data <- selected_data %>%
  mutate(Municipality = ifelse(grepl("^Toronto", Municipality), "Toronto", Municipality))

# checking cardinality in this column:
unique(final_data$Municipality)



######### 2. Exploratory Data Analysis ###################

# summary
# Univaritae analysis: Distribution of individual features
# Bivariate Analysis: Relationships between variables
# Geospatial Analysis : ggmap & sf
# Trends over time.

print(colnames(final_data))
describe(final_data)


# Data preprocessing
municipalities <- table(final_data$Municipality)

# Grouping by year and area, and calculating the average price
avgPrice_area <- final_data %>%
  group_by(X_Year.x, Municipality) %>%
  summarize(avg_price = mean(Average_Price, na.rm = TRUE), .groups = "drop")  # Fixing the column name here

head(avgPrice_area)

# Bar chart of average price by area
ggplot(avgPrice_area, aes(x = Municipality, y = avg_price)) +
  geom_bar(stat = "identity", color = "purple") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate x-axis labels by 45 degrees
  )


value_counts <- table(final_data$Municipality)
value_counts_df <- as.data.frame(value_counts)
# Renaming COlumns:
colnames(value_counts_df) <- c("Municipality", "Frequency")



ggplot(value_counts_df, aes(x =  Municipality, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "white") +
  labs(title = "Municipality Counts", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



############## 3. Feature Engineering ######################
# Categorical Conversion of qualitative factors: factor()
# Undersampling Toronto:


toronto_undersample_count <- 10000

# Create a subset of Toronto
toronto_data <- final_data[final_data$Municipality == "Toronto", ]
toronto_data

# Create a subset of other municipalities
other_data <- final_data[final_data$Municipality != "Toronto", ]

set.seed(123)  # For reproducibility
toronto_undersampled <- toronto_data[sample(1:nrow(toronto_data), toronto_undersample_count), ]
final_data <- rbind(toronto_undersampled, other_data)



value_counts <- table(final_data$Municipality)
value_counts_df <- as.data.frame(value_counts)
# Renaming COlumns:
colnames(value_counts_df) <- c("Municipality", "Frequency")



ggplot(value_counts_df, aes(x =  Municipality, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "white") +
  labs(title = "Municipality Counts", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


############## 4. Statistical Analysis ##########################
# Correlation Analysis
# Hypothesis Testing: If required
# Variance Inflation Facotr: Checking for multi-collinearity.

#plotting the number of observation per municipality in the dataset:
                 



#separating numerical columns:
final_numeric <- select(final_data, where(is.numeric))
head(final_numeric)

# One Hot encoding of categorical columns:
dummy_vars <- dummyVars(~Municipality + Building_Type, data = final_data)
encoded_categorical <- as.data.frame(predict(dummy_vars, newdata = final_data))
final_prepped <- cbind(final_numeric, encoded_categorical)

#sanity check:
head(final_prepped)

#summary:
final_prepped %>% select(Sales, Dollar_Volume, New_Listings, CANRECDM, avg_five_year_rates) %>%
  summary()
#correlation:
cor_matrix <- cor(final_data %>% select_if(is.numeric))
corrplot(cor_matrix, 
         method = "square",                   # Square shaped cells
         type = "upper",                      # Display only the upper triangle
         col = colorRampPalette(c("blue", "white", "red"))(200),  # Blue to white to red color palette
         tl.col = "black",                    # Title labels in black
         tl.cex = 0.8,                        # Adjust title label size
         number.cex = 0.7,                    # Adjust number label size
         addCoef.col = "black",               # Add coefficients in black color
         diag = FALSE,                        # Remove the diagonal (correlations with themselves)
         cl.pos = "n",                        # Hide color legend for a cleaner plot
         border = "white",                    # Border color for the squares
         title = "Correlation Matrix"         # Title for the plot
)


#for year 2022
ggplot(final_data %>% filter(New_Listings > 0), aes(x = Municipality, y = New_Listings, color = as.factor(X_Year.x))) +
  geom_boxplot(fill = "lightblue", color = "black") +  # Box plot with specified colors
  theme_minimal() +  # Minimal theme for a cleaner look
  labs(
    title = "Box Plot of New Listings by Municipality and Year",
    x = "Municipality",
    y = "New Listings",
    color = "Year"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Title alignment and size
    axis.title = element_text(size = 12),  # Axis title size
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    legend.position = "top"  # Position the legend at the top
  )


yearly_listings <- final_data %>%
  group_by(X_Year.x) %>%
  summarise(total_listings = sum(New_Listings, na.rm = TRUE))

# Plotting the total listings by year as a bar chart
ggplot(yearly_listings, aes(x = as.factor(X_Year.x), y = total_listings)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +  # Bar chart with specified colors
  theme_minimal() +  # Minimal theme for a cleaner look
  labs(
    title = "Total New Listings by Year",
    x = "Year",
    y = "Total New Listings"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Title alignment and size
    axis.title = element_text(size = 12),  # Axis title size
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )


summary_stats <-  selected_data %>%
  select(Sales, Dollar_Volume, Average_Price, New_Listings) %>%
  summary()

print(summary_stats)


ggplot(merged_data, aes(x = X_Year.x, y = Average_Price)) +
  geom_col() +
  labs(title = "Average Price over Time", x = "Year Quarter", y = "Average Price") +
  theme_minimal()

################# 5. Regression Modelling ##########


## Data Peocessing:



colnames(final_prepped) <- make.names(colnames(final_prepped))
independent_columns <- colnames(final_prepped)[!colnames(final_prepped) %in% c("Average_Price")]
formula <- as.formula(paste("Average_Price ~", paste(independent_columns, collapse = " + ")))
lm_model <- lm(formula, data = final_prepped)

# Summary of the model
summary(lm_model)

coefficients <- coef(lm_model)

# Remove the intercept from the coefficients (assuming the intercept is named "(Intercept)")
coefficients <- coefficients[!names(coefficients) %in% "(Intercept)"]

# Convert the remaining coefficients into a data frame for easier plotting
coeff_df <- data.frame(
  Variable = names(coefficients),
  Coefficient = coefficients
)

# Plot the coefficients using ggplot2

ggplot(coeff_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(
    title = "Coefficients from Linear Model (Excluding Intercept)",
    x = "Variable",
    y = "Coefficient"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Title alignment and size
    axis.title = element_text(size = 12),  # Axis title size
    axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate x-axis labels for readability
  )

# splitting data
# model selection
# triaining or fitting the model
# evaluating the model
# model improvements
# Results Interpretation

colnames(final_prepped)

# Group by Municipality and calculate the mean of Average_Price
model <- lm(Average_Price ~ ., data = final_prepped)

# Display the summary of the model
summary(model)

# Check model diagnostics
par(mfrow = c(2, 2))
plot(model)
anova_model <- aov(Average_Price ~ ., data = final_prepped)
summary(anova_model)

# Scatter plot to check relationship between Average_Price and another variable (e.g., Dollar_Volume)
ggplot(final_prepped, aes(x = Dollar_Volume, y = Average_Price)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Relationship between Dollar Volume and Average Price", x = "Dollar Volume", y = "Average Price")




####################################3 Machine Learning #################################################
# splitting the data:

# Stratify data based on Municipality and split into train and test sets
set.seed(123)

# Split the data into 70% for training and 30% for testing
train_index <- sample(1:nrow(final_prepped), 0.7 * nrow(final_prepped))
train_data <- final_prepped[train_index, ]
test_data <- final_prepped[-train_index, ]

# Now you can proceed with model training and evaluation

# Linear Regression:
lm_model <- lm(Average_Price ~ ., data = train_data)
lm_predictions <- predict(lm_model, newdata = test_data)
lm_rmse <- sqrt(mean((lm_predictions - test_data$Average_Price)^2))
cat("Linear Regression RMSE:", lm_rmse, "\n")

# XGBoost:
library(xgboost)
train_matrix <- as.matrix(train_data[, setdiff(names(train_data), "Average_Price")])
test_matrix <- as.matrix(test_data[, setdiff(names(test_data), "Average_Price")])
train_label <- train_data$Average_Price
test_label <- test_data$Average_Price

xg_model <- xgboost(data = train_matrix, label = train_label, 
                    objective = "reg:squarederror", nrounds = 100)
xg_predictions <- predict(xg_model, newdata = test_matrix)
xg_rmse <- sqrt(mean((xg_predictions - test_label)^2))
cat("XGBoost RMSE:", xg_rmse, "\n")

# Calculate errors for XGBoost
errors <- test_label - xg_predictions
# Scatter Plot of Actual vs Predicted Values for XGBoost
library(ggplot2)
ggplot(data = data.frame(Actual = test_label, Predicted = xg_predictions), aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Reference line for perfect prediction
  labs(title = "Scatter Plot of Actual vs Predicted for XGBoost", x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

# Plot the distribution of errors for XGBoost
ggplot(data.frame(Errors = errors), aes(x = Errors)) +
  geom_histogram(bins = 100, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Errors for XGBoost", x = "Error (Actual - Predicted)", y = "Frequency") +
  theme_minimal()


# Ridge Regression:
library(glmnet)
ridge_model <- cv.glmnet(train_matrix, train_label, alpha = 0)
ridge_predictions <- predict(ridge_model, newx = test_matrix)
ridge_rmse <- sqrt(mean((ridge_predictions - test_label)^2))
cat("Ridge Regression RMSE:", ridge_rmse, "\n")

# Lasso Regression:
lasso_model <- cv.glmnet(train_matrix, train_label, alpha = 1)
lasso_predictions <- predict(lasso_model, newx = test_matrix)
lasso_rmse <- sqrt(mean((lasso_predictions - test_label)^2))
cat("Lasso Regression RMSE:", lasso_rmse, "\n")

# Decision Tree:
library(rpart)
dt_model <- rpart(Average_Price ~ ., data = train_data)
dt_predictions <- predict(dt_model, newdata = test_data)
dt_rmse <- sqrt(mean((dt_predictions - test_data$Average_Price)^2))
cat("Decision Tree RMSE:", dt_rmse, "\n")

# Random Forest:
library(randomForest)

# Train a Random Forest model
rf_model <- randomForest(Average_Price ~ ., data = train_data, mtry = 3, ntree = 50)

# Make predictions on the test set
rf_predictions <- predict(rf_model, newdata = test_data)

# Calculate RMSE for Random Forest
rf_rmse <- sqrt(mean((rf_predictions - test_data$Average_Price)^2))
cat("Random Forest RMSE:", rf_rmse, "\n")

# Combine all RMSE values in a data frame for comparison
rmse_comparison <- data.frame(
  Model = c("Linear Regression", "XGBoost", "Ridge Regression", "Lasso Regression", "Decision Tree", "Random Forest"),
  RMSE = c(lm_rmse, xg_rmse, ridge_rmse, lasso_rmse, dt_rmse, rf_rmse)
)

# Plot the RMSE values
ggplot(rmse_comparison, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(RMSE, 2)), vjust = -0.3) +  # Add the RMSE values on top of bars
  theme_minimal() +
  labs(title = "RMSE Comparison of Models", x = "Model", y = "RMSE") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




############### Optimized models #########################################################################################

# Load necessary libraries
library(caret)
library(xgboost)
library(glmnet)
library(rpart)
library(randomForest)
library(ggplot2)

# Normalize data
train_matrix <- as.matrix(train_data[, setdiff(names(train_data), "Average_Price")])
test_matrix <- as.matrix(test_data[, setdiff(names(test_data), "Average_Price")])

preProcess_obj <- preProcess(train_matrix, method = "scale")
train_normalized <- predict(preProcess_obj, train_data)
test_normalized <- predict(preProcess_obj, test_data)

train_label <- train_normalized$Average_Price
test_label <- test_normalized$Average_Price

# Initialize the models list with Random Forest parameters
models <- list(
  list(mtry = 2, ntree = 30), 
  list(mtry = 4, ntree = 60)
)

# Prepare k-fold cross-validation for Random Forest
train_accuracy <- numeric(length(models))
test_accuracy <- numeric(length(models))


train_data <- na.omit(train_data)

set.seed(1)
cv_folds <- createFolds(train_data$Average_Price, k = 5)

# Initialize vectors to store accuracy
train_accuracy <- numeric(length(models))
test_accuracy <- numeric(length(models))

# Loop over models and perform k-fold cross-validation
for (model_index in 1:length(models)) {
  mtry_value <- models[[model_index]]$mtry
  ntree_value <- models[[model_index]]$ntree
  
  # Set the seed for reproducibility
  set.seed(1)
  
  fold_train_acc <- numeric(5)
  fold_test_acc <- numeric(5)
  
  # k-Fold cross-validation
  for (fold in 1:5) {
    # Define train and test sets based on the current fold
    train_indices <- cv_folds[[fold]]  # indices for the current fold
    test_indices <- unlist(cv_folds[-fold])  # indices for the other folds (excluding the current fold)
    
    train_data_fold <- train_data[train_indices, ]
    test_data_fold <- train_data[test_indices, ]
    
    # Train Random Forest Model
    rf_model <- randomForest(Average_Price ~ ., data = train_data_fold, mtry = mtry_value, ntree = ntree_value)
    
    # Make predictions
    train_preds <- predict(rf_model, train_data_fold)
    test_preds <- predict(rf_model, test_data_fold)
    
    # Calculate RMSE for training and testing sets
    fold_train_acc[fold] <- sqrt(mean((train_preds - train_data_fold$Average_Price)^2))  # RMSE for train
    fold_test_acc[fold] <- sqrt(mean((test_preds - test_data_fold$Average_Price)^2))  # RMSE for test
  }
  
  # Average RMSE over the folds
  train_accuracy[model_index] <- mean(fold_train_acc)
  test_accuracy[model_index] <- mean(fold_test_acc)
}

# Now calculate the RMSE for other models
# 1. Linear Regression Model
lm_model <- lm(Average_Price ~ ., data = train_normalized)
lm_predictions <- predict(lm_model, newdata = test_normalized)
lm_rmse <- sqrt(mean((lm_predictions - test_label)^2))

# 2. XGBoost Model
xgboost_grid <- expand.grid(
  nrounds = c(20, 40),
  max_depth = c(3, 6),
  eta = c(0.01, 0.1),
  gamma = c(0, 1),
  colsample_bytree = c(0.5, 1),
  min_child_weight = c(1, 5),
  subsample = c(0.5, 1)
)

train_control <- trainControl(method = "cv", number = 5)
xg_model <- train(
  x = train_matrix,
  y = train_label,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = xgboost_grid
)
xg_predictions <- predict(xg_model, newdata = test_matrix)
xg_rmse <- sqrt(mean((xg_predictions - test_label)^2))

# 3. Ridge Regression Model
ridge_model <- cv.glmnet(train_matrix, train_label, alpha = 0)
ridge_predictions <- predict(ridge_model, newx = test_matrix)
ridge_rmse <- sqrt(mean((ridge_predictions - test_label)^2))

# 4. Lasso Regression Model
lasso_model <- cv.glmnet(train_matrix, train_label, alpha = 1)
lasso_predictions <- predict(lasso_model, newx = test_matrix)
lasso_rmse <- sqrt(mean((lasso_predictions - test_label)^2))

# 5. Decision Tree Model
dt_model <- rpart(Average_Price ~ ., data = train_normalized)
dt_predictions <- predict(dt_model, newdata = test_normalized)
dt_rmse <- sqrt(mean((dt_predictions - test_label)^2))

# Combine RMSE and k-fold results into a single data frame
rmse_results <- data.frame(
  Model = c("Linear Regression", "XGBoost", "Ridge Regression", "Lasso Regression", "Decision Tree", "Random Forest"),
  RMSE = c(lm_rmse, xg_rmse, ridge_rmse, lasso_rmse, dt_rmse, mean(test_accuracy))
)

# Plot RMSE comparison as a bar plot
ggplot(rmse_results, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", color = "black") + 
  geom_text(aes(label = round(RMSE, 2)), vjust = -0.5, color = "black", size = 5) +  # Add text labels on top
  theme_minimal() +
  labs(
    title = "RMSE Comparison of Different Regression Models",
    x = "Model",
    y = "RMSE"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(values = c("skyblue", "lightgreen", "orange", "pink", "lightcoral", "lightyellow"))



###################################Without Normalizing Data###############################################################
# Min-Max Scaling Function
min_max_scale <- function(data, min_val, max_val) {
  scaled_data <- (data - min_val) / (max_val - min_val)
  return(scaled_data)
}

# Initialize accuracy results
model_names <- c("Random Forest", "Linear Regression", "XGBoost", "Ridge Regression", "Lasso Regression", "Decision Tree")
test_rmse <- numeric(length(model_names))

# k-Fold Cross-Validation with Min-Max Scaling
set.seed(1)
fold_rmse <- matrix(0, nrow = length(model_names), ncol = 5)
for (fold in 1:5) {
  # Define train and test sets for the current fold
  train_indices <- unlist(cv_folds[-fold])
  test_indices <- cv_folds[[fold]]
  
  train_data_fold <- train_data[train_indices, ]
  test_data_fold <- train_data[test_indices, ]
  
  # Extract features and labels
  train_features <- train_data_fold[, setdiff(names(train_data), "Average_Price")]
  test_features <- test_data_fold[, setdiff(names(train_data), "Average_Price")]
  train_labels <- train_data_fold$Average_Price
  test_labels <- test_data_fold$Average_Price
  
  # Calculate Min and Max from training data
  min_val <- apply(train_features, 2, min)
  max_val <- apply(train_features, 2, max)
  
  # Apply Min-Max Scaling
  train_features_scaled <- as.data.frame(apply(train_features, 2, min_max_scale, min_val = min_val, max_val = max_val))
  test_features_scaled <- as.data.frame(apply(test_features, 2, min_max_scale, min_val = min_val, max_val = max_val))
  
  # Combine scaled features with labels
  train_data_scaled <- cbind(train_features_scaled, Average_Price = train_labels)
  test_data_scaled <- cbind(test_features_scaled, Average_Price = test_labels)
  
  # 1. Random Forest Model
  rf_model <- randomForest(Average_Price ~ ., data = train_data_scaled, mtry = 2, ntree = 30)
  rf_preds <- predict(rf_model, test_data_scaled)
  fold_rmse[1, fold] <- sqrt(mean((rf_preds - test_labels)^2))
  
  # 2. Linear Regression Model
  lm_model <- lm(Average_Price ~ ., data = train_data_scaled)
  lm_preds <- predict(lm_model, newdata = test_data_scaled)
  fold_rmse[2, fold] <- sqrt(mean((lm_preds - test_labels)^2))
  
  # 3. XGBoost Model
  xgboost_grid <- expand.grid(
    nrounds = c(20, 40),
    max_depth = c(3, 6),
    eta = c(0.01, 0.1),
    gamma = c(0, 1),
    colsample_bytree = c(0.5, 1),
    min_child_weight = c(1, 5),
    subsample = c(0.5, 1)
  )
  train_control <- trainControl(method = "cv", number = 5)
  xg_model <- train(
    x = as.matrix(train_features_scaled),
    y = train_labels,
    method = "xgbTree",
    trControl = train_control,
    tuneGrid = xgboost_grid
  )
  xg_preds <- predict(xg_model, newdata = as.matrix(test_features_scaled))
  fold_rmse[3, fold] <- sqrt(mean((xg_preds - test_labels)^2))
  
  # 4. Ridge Regression Model
  ridge_model <- cv.glmnet(as.matrix(train_features_scaled), train_labels, alpha = 0)
  ridge_preds <- predict(ridge_model, newx = as.matrix(test_features_scaled), s = "lambda.min")
  fold_rmse[4, fold] <- sqrt(mean((ridge_preds - test_labels)^2))
  
  # 5. Lasso Regression Model
  lasso_model <- cv.glmnet(as.matrix(train_features_scaled), train_labels, alpha = 1)
  lasso_preds <- predict(lasso_model, newx = as.matrix(test_features_scaled), s = "lambda.min")
  fold_rmse[5, fold] <- sqrt(mean((lasso_preds - test_labels)^2))
  
  # 6. Decision Tree Model
  dt_model <- rpart(Average_Price ~ ., data = train_data_scaled)
  dt_preds <- predict(dt_model, newdata = test_data_scaled)
  fold_rmse[6, fold] <- sqrt(mean((dt_preds - test_labels)^2))
}

# Calculate average RMSE for each model

fold_rmse

test_rmse <- rowMeans(fold_rmse)

# Create a data frame for RMSE results
model_names
test_rmse

rmse_results <- data.frame(
  Model = model_names,
  RMSE = test_rmse
)

# Plot RMSE comparison as a bar plot
library(ggplot2)
ggplot(rmse_results, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(RMSE, 2)), vjust = -0.5, color = "black", size = 5) +
  theme_minimal() +
  labs(
    title = "RMSE Comparison of Different Models with Min-Max Scaling",
    x = "Model",
    y = "RMSE"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(values = c("skyblue", "lightgreen", "orange", "pink", "lightcoral", "lightyellow"))



best_params <- xg_model$bestTune

# Print best parameters
print(best_params)

# Train the final XGBoost model using the best parameters
final_xg_model <- xgboost(
  data = as.matrix(train_features_scaled),
  label = train_labels,
  nrounds = best_params$nrounds,
  max_depth = best_params$max_depth,
  eta = best_params$eta,
  gamma = best_params$gamma,
  colsample_bytree = best_params$colsample_bytree,
  min_child_weight = best_params$min_child_weight,
  subsample = best_params$subsample,
  objective = "reg:squarederror",
  verbose = 0
)

# Make predictions on the test dataset
final_xg_preds <- predict(final_xg_model, as.matrix(test_features_scaled))

# Calculate RMSE for the final model on the test data
final_xg_rmse <- sqrt(mean((final_xg_preds - test_labels)^2))
print(paste("Final XGBoost Test RMSE:", round(final_xg_rmse, 4)))



