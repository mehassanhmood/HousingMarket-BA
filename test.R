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
rates <- read.csv("./Data/clean_rates.csv")
home_prices <- read.csv("./Data/GTA_HomePrice_History.csv")
primary_keys <- read.csv("./prim_key.csv")

###################### Exploratory Data Analysis and Data Processing ############################

# Function to generate data quality report
data_q_report <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  cat("Column Names:\n", colnames(data), "\n")
  null_counts <- sapply(data, function(x) sum(is.na(x)))
  cat("\nNull counts per column:\n", null_counts, "\n")
  columns_with_nulls <- colnames(data)[apply(data, 2, anyNA)]
  cat("\nColumns with null values:\n", columns_with_nulls, "\n")
  duplicates <- data[duplicated(data), ]
  if (length(duplicates) > 0) {
    cat("\nDuplicate rows:\n", paste(duplicates, collapse = "\n"), "\n")
  } else {
    cat("\nNo duplicate rows found.\n")
  }
  cat("\nSummary Report:\n")
  cat("Total Columns:", ncol(data), "\n")
  cat("Total Rows:", nrow(data), "\n")
  cat("Columns with Nulls:", paste(columns_with_nulls, collapse = ", "), "\n")
  cat("Number of Duplicate Rows:", nrow(duplicates), "\n")
}

# Data Quality reports
data_list <- list(recession_data = recession_indicator, homePrices = home_prices, primaryKeys = primary_keys, interestR = rates)
for (data in names(data_list)) {
  cat("\nData Quality report for", data, ":\n")
  data_q_report(data_list[[data]])
}

# Merging datasets
prices_recession <- inner_join(home_prices, recession_indicator, by = "Year_Quarter_Key")
merged_data <- inner_join(prices_recession, rates, by = "Year_Quarter_Key")
head(merged_data)

# Data Quality report for merged data
data_q_report(merged_data)

# Selecting relevant columns
selected_data <- merged_data %>%
  select(-1, -3, -4, -5, -7, -12, -14, -16, -17, -18)
head(selected_data)

# Renaming columns
final_data <- selected_data %>%
  mutate(Municipality = ifelse(grepl("^Toronto", Municipality), "Toronto", Municipality))
unique(final_data$Municipality)

######################## 2. Exploratory Data Analysis ############################

# Summary and descriptive statistics
describe(final_data)

# Grouping by year and municipality to calculate average price
avgPrice_area <- final_data %>%
  group_by(X_Year.x, Municipality) %>%
  summarize(avg_price = mean(Average_Price, na.rm = TRUE), .groups = "drop")
head(avgPrice_area)

# Bar chart of average price by municipality
ggplot(avgPrice_area, aes(x = Municipality, y = avg_price)) +
  geom_bar(stat = "identity", color = "purple") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Frequency of municipalities
value_counts_df <- as.data.frame(table(final_data$Municipality))
colnames(value_counts_df) <- c("Municipality", "Frequency")
ggplot(value_counts_df, aes(x = Municipality, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "white") +
  labs(title = "Municipality Counts", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

######################## 3. Feature Engineering ##########################

# Undersampling Toronto
toronto_undersample_count <- 10000
toronto_data <- final_data[final_data$Municipality == "Toronto", ]
other_data <- final_data[final_data$Municipality != "Toronto", ]
set.seed(123)
toronto_undersampled <- toronto_data[sample(1:nrow(toronto_data), toronto_undersample_count), ]
final_data <- rbind(toronto_undersampled, other_data)

# Municipality counts after undersampling
value_counts_df <- as.data.frame(table(final_data$Municipality))
colnames(value_counts_df) <- c("Municipality", "Frequency")
ggplot(value_counts_df, aes(x = Municipality, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "white") +
  labs(title = "Municipality Counts", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

######################## 4. Statistical Analysis ##########################

# Correlation Analysis
final_numeric <- select(final_data, where(is.numeric))
head(final_numeric)

# One-hot encoding categorical variables
dummy_vars <- dummyVars(~Municipality + Building_Type, data = final_data)
encoded_categorical <- as.data.frame(predict(dummy_vars, newdata = final_data))
final_prepped <- cbind(final_numeric, encoded_categorical)

# Correlation matrix
cor_matrix <- cor(final_prepped %>% select_if(is.numeric))
corrplot(cor_matrix, method = "square", type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.col = "black", tl.cex = 0.8, number.cex = 0.7, addCoef.col = "black", 
         diag = FALSE, cl.pos = "n", border = "white", title = "Correlation Matrix")

######################## 5. Regression Modelling ##########################

# Prepare data for regression
colnames(final_prepped) <- make.names(colnames(final_prepped))
independent_columns <- colnames(final_prepped)[!colnames(final_prepped) %in% c("Average_Price")]
formula <- as.formula(paste("Average_Price ~", paste(independent_columns, collapse = " + ")))

# Linear model
lm_model <- lm(formula, data = final_prepped)
summary(lm_model)

# Plot the coefficients
coefficients <- coef(lm_model)[!names(coef(lm_model)) %in% "(Intercept)"]
coeff_df <- data.frame(Variable = names(coefficients), Coefficient = coefficients)
ggplot(coeff_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Coefficients from Linear Model (Excluding Intercept)", 
       x = "Variable", y = "Coefficient") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Model diagnostics and ANOVA
par(mfrow = c(2, 2))
plot(lm_model)
anova_model <- aov(Average_Price ~ ., data = final_prepped)
summary(anova_model)

# Scatter plot to check relationship between Average_Price and Dollar_Volume
ggplot(final_prepped, aes(x = Dollar_Volume, y = Average_Price)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Relationship between Dollar Volume and Average Price", 
       x = "Dollar Volume", y = "Average Price")


# Load necessary libraries
library(caret)
library(xgboost)
library(glmnet)
library(rpart)
library(randomForest)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Split the data into 70% for training and 30% for testing
train_index <- sample(1:nrow(final_prepped), 0.7 * nrow(final_prepped))
train_data <- final_prepped[train_index, ]
test_data <- final_prepped[-train_index, ]

# Prepare the data matrices for model training
train_matrix <- as.matrix(train_data[, setdiff(names(train_data), "Average_Price")])
test_matrix <- as.matrix(test_data[, setdiff(names(test_data), "Average_Price")])
train_label <- train_data$Average_Price
test_label <- test_data$Average_Price

# Normalize the data
preProcess_obj <- preProcess(train_matrix, method = "scale")
train_normalized <- predict(preProcess_obj, train_data)
test_normalized <- predict(preProcess_obj, test_data)

train_label <- train_normalized$Average_Price
test_label <- test_normalized$Average_Price

# Define models for comparison
models <- list(
  lm = lm(Average_Price ~ ., data = train_normalized),
  xgboost = train(x = train_matrix, y = train_label, method = "xgbTree", 
                  trControl = trainControl(method = "cv", number = 5)),
  ridge = cv.glmnet(train_matrix, train_label, alpha = 0),
  lasso = cv.glmnet(train_matrix, train_label, alpha = 1),
  dt = rpart(Average_Price ~ ., data = train_normalized),
  rf = randomForest(Average_Price ~ ., data = train_data, mtry = 3, ntree = 50)
)

# Function to calculate RMSE
calc_rmse <- function(predictions, actual) {
  sqrt(mean((predictions - actual)^2))
}

# Initialize a data frame to store RMSE results
rmse_results <- data.frame(Model = character(0), RMSE = numeric(0))

# Evaluate each model and calculate RMSE
for (model_name in names(models)) {
  model <- models[[model_name]]
  
  # Get predictions based on model type
  if (model_name == "xgboost") {
    predictions <- predict(model, newdata = test_matrix)
  } else if (model_name == "rf") {
    predictions <- predict(model, newdata = test_data)
  } else {
    predictions <- predict(model, newdata = test_normalized)
  }
  
  # Calculate RMSE for the model
  model_rmse <- calc_rmse(predictions, test_label)
  
  # Add the results to the rmse_results data frame
  rmse_results <- rbind(rmse_results, data.frame(Model = model_name, RMSE = model_rmse))
}

# Plot RMSE comparison
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
