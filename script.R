library(dplyr)
library(tidygeocoder)


################### 1. Importing Data ####################

recession_indicator <- read.csv("./CanadaRecessionIndicator.csv")
rates <-read.csv("./clean_rates.csv")
home_prices <- read.csv("./GTA_HomePrice_History.csv")
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
selected_data <- merged_data %>%
  select(-1,-4,-12,-13,-14,-15,-16,-17,-18)

head(selected_data)

final_data <- selected_data %>%
  geocode(Municipality, method="osm", lat = latitude, long=longitude)

head(final_data)


######### 2. Exploratory Data Analysis ###################
# summary
# Univaritae analysis: Distribution of individual features
# Bivariate Analysis: Relationships between variables
# Geospatial Analysis : ggmap & sf
# Trends over time.



############## 3. Feature Engineering ######################
# Categorical Conversion of qualitative factors: factor()
# Normalizing or Scaling the features



############## 4. Statistical Analysis ##########################
# Correlation Analysis
# Hypothesis Testing: If required
# Variance Inflation Facotr: Checking for multi-collinearity.





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
# splitting data
# model selection
# triaining or fitting the model
# evaluating the model
# model improvements
# Results Interpretation

