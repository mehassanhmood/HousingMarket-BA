
library(tidyverse)
library(dplyr)
library(tidygeocoder)
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


data_q_report(recession_indicator)


data_list <- list(recession_data=recession_indicator, homePrices = home_prices, primaryKeys = primary_keys, interestR=rates)

for (data in names(data_list)) {
  cat("\nData Quality report for", data, ":\n")
  data_q_report(data_list[[data]])
}




head(home_prices)

prices_recession <- inner_join(home_prices, recession_indicator, by = "Year_Quarter_Key")

merged_data <- inner_join(prices_recession, rates, by = "Year_Quarter_Key")

head(merged_data)

data_q_report(merged_data)


# Selecting the specific columns:
library(tmaptools)  # For OpenStreetMap geocoding
selected_data <- merged_data %>%
  select(-1,-4,-12,-13,-14,-15,-16,-17,-18)


# safe_geocode <- function(area_name) {
#   tryCatch({
#     # Geocode using OpenStreetMap
#     geocode_result <- geocode_OSM(area_name)
#     return(geocode_result)
#   }, error = function(e) {
#     # In case of an error, return NAs
#     return(data.frame(lat = NA, lon = NA))
#   })
# }
# 
# # Apply geocoding to each row with error handling
# final_data <- selected_data %>%
#   rowwise() %>%
#   mutate(
#     geocode_result = list(safe_geocode(Area)),
#     latitude = geocode_result[[1]]$lat,
#     longitude = geocode_result[[1]]$lon
#   ) %>%
#   ungroup() %>%
#   select(-geocode_result)

final_data <- selected_data
head(final_data)


######### 2. Exploratory Data Analysis ###################
library(psych)

# summary
# Univaritae analysis: Distribution of individual features
# Bivariate Analysis: Relationships between variables
# Geospatial Analysis : ggmap & sf
# Trends over time.

print(colnames(final_data))
describe(final_data)

## qualitative features:
#


# Data preprocessing
municipalities <- table(final_data$Municipality)
areas <- table(final_data$Area)

head(final_data)

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



############## 3. Feature Engineering ######################
# Categorical Conversion of qualitative factors: factor()
# Normalizing or Scaling the features

library(caret)
library(leaflet)

############## 4. Statistical Analysis ##########################
# Correlation Analysis
# Hypothesis Testing: If required
# Variance Inflation Facotr: Checking for multi-collinearity.
head(final_data)

value_counts <- table(final_data$Municipality)
value_counts

final_numeric <- select(final_data, where(is.numeric))
head(final_numeric)
dummy_vars <- dummyVars(~ Area + Municipality, data = final_data)
encoded_categorical <- as.data.frame(predict(dummy_vars, newdata = final_data))
final_prepped <- cbind(final_numeric, encoded_categorical)
head(final_prepped)

final_prepped %>% select(Sales, Dollar_Volume, New_Listings, CANRECDM, avg_five_year_rates) %>%
  summary()

cor(final_prepped %>% select(Sales, Dollar_Volume, New_Listings, CANRECDM, avg_five_year_rates, AreaDurham))

library(corrplot)
cor_matrix <- cor(final_prepped %>% select(Sales, Dollar_Volume, New_Listings, CANRECDM, avg_five_year_rates, AreaDurham))
corrplot(cor_matrix, method = "square")

#for year 2001
ggplot(final_data %>% filter(X_Year.x == 2001 ), aes(x = Area, y = New_Listings)) +
  geom_boxplot(fill = "lightblue", color = "black") +  # Box plot with specified colors
  theme_minimal() +  # Minimal theme for a cleaner look
  labs(
    title = "Box Plot of New Listings by Area",
    x = "Area",
    y = "New Listings"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Title alignment and size
    axis.title = element_text(size = 12),  # Axis title size
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

#for year 2022
ggplot(final_data %>% filter(X_Year.x == 2021 & New_Listings > 0), aes(x = Area, y = New_Listings)) +
  geom_boxplot(fill = "lightblue", color = "black") +  # Box plot with specified colors
  theme_minimal() +  # Minimal theme for a cleaner look
  labs(
    title = "Box Plot of New Listings by Area",
    x = "Area",
    y = "New Listings"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Title alignment and size
    axis.title = element_text(size = 12),  # Axis title size
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
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


library(ggplot2)
#install.packages('corrplot')
library(corrplot)
library(stats)


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



colnames(final_prepped)


colnames(final_prepped) <- make.names(colnames(final_prepped))
independent_columns <- colnames(final_prepped)[!colnames(final_prepped) %in% c("Average_Price")]
formula <- as.formula(paste("Average_Price ~", paste(independent_columns, collapse = " + ")))
lm_model <- lm(formula, data = final_prepped)

# Summary of the model
summary(lm_model)

# splitting data
# model selection
# triaining or fitting the model
# evaluating the model
# model improvements
# Results Interpretation

colnames(final_prepped)

# Group by Municipality and calculate the mean of Average_Price
model <- lm(Average_Price ~ Sales + Dollar_Volume + New_Listings + CANRECDM + 
              X_Indicator + avg_five_year_rates + AreaDurham, data = final_prepped)

# Display the summary of the model
summary(model)

# Check model diagnostics
par(mfrow = c(2, 2))
plot(model)
anova_model <- aov(Average_Price ~ AreaDurham, data = final_prepped)
summary(anova_model)

# Scatter plot to check relationship between Average_Price and another variable (e.g., Dollar_Volume)
ggplot(final_prepped, aes(x = Dollar_Volume, y = Average_Price)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Relationship between Dollar Volume and Average Price", x = "Dollar Volume", y = "Average Price")

# Boxplot for categorical variables (e.g., AreaDurham)
ggplot(final_prepped, aes(x = as.factor(AreaDurham), y = Average_Price)) +
  geom_boxplot() +
  labs(title = "Average Price by AreaDurham", x = "AreaDurham", y = "Average Price")



####################################3 Machine Learning #################################################
# splitting the data:

set.seed(123)

# Split the data into 70% for training and 30% for testing
train_index <- sample(1:nrow(final_prepped), 0.7 * nrow(final_prepped))
train_data <- final_prepped[train_index, ]
test_data <- final_prepped[-train_index, ]


#Linear Regression:

# Train a linear regression model
lm_model <- lm(Average_Price ~ ., data = train_data)

# View the summary of the model
summary(lm_model)

# Make predictions on the test set
lm_predictions <- predict(lm_model, newdata = test_data)

# Calculate RMSE
lm_rmse <- sqrt(mean((lm_predictions - test_data$Average_Price)^2))
cat("Linear Regression RMSE:", lm_rmse, "\n")


# library(randomForest)
# 
# #Randdom Forest:
# rf_model <- randomForest(Average_Price ~., data = train_data)
# 
# # View the summary of the model
# print(rf_model)
# 
# # Make predictions on the test set
# rf_predictions <- predict(rf_model, newdata = test_data)
# 
# # Calculate RMSE
# rf_rmse <- sqrt(mean((rf_predictions - test_data$Average_Price)^2))
# cat("Random Forest RMSE:", rf_rmse, "\n")

library(xgboost)

train_matrix <- as.matrix(train_data[, setdiff(names(train_data), "Average_Price")])
test_matrix <- as.matrix(test_data[, setdiff(names(test_data), "Average_Price")])

train_label <- train_data$Average_Price
test_label <- test_data$Average_Price

# Train an XGBoost model
xg_model <- xgboost(data = train_matrix, label = train_label, 
                    objective = "reg:squarederror", nrounds = 100)

# Make predictions on the test set
xg_predictions <- predict(xg_model, newdata = test_matrix)

# Calculate RMSE
xg_rmse <- sqrt(mean((xg_predictions - test_label)^2))
cat("XGBoost RMSE:", xg_rmse, "\n")




