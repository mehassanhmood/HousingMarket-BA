library(dplyr)


recession_indicator <- read.csv("./CanadaRecessionIndicator.csv")
rates <-read.csv("./clean_rates.csv")
home_prices <- read.csv("./GTA_HomePrice_History.csv")
primary_keys <- read.csv("./prim_key.csv")


# head(primary_keys)
# 
# print(colnames(home_prices))
# 
# null_counts
# 
# 
# count_nulls <- function (data) {
#   if (!is.data.frame(data)) {
#     stop("Input must be a data frame")
#   }
#   null_counts <- sapply(data, function(x) sum(is.na(x)))
#   
#   return(null_counts)
# }
# 
# 
# nulls_recession <-count_nulls((recession_indicator))
# nulls_recession
# 
# nulls_home_prices <- count_nulls(home_prices)
# nulls_home_prices
# 
# nulls_rates <- count_nulls(rates)
# nulls_rates
# 
# nulls_primary <- count_nulls(primary_keys)
# nulls_primary
# 
# 
# 
# 
# duplicates_recession <- recession_indicator[duplicated(recession_indicator), ]
# duplicates_recession
# 
# home_prices[duplicated(home_prices), ]




# writing a function for the above mentioned code for reusability:



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

prices_recession <- merge(home_prices, recession_indicator, by = "Year_Quarter_Key")

merged_data <- merge(prices_recession, rates, by = "Year_Quarter_Key")

head(merged_data)

data_q_report(merged_data)










