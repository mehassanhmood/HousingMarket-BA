
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
selected_data <- merged_data %>%
  select(-1,-4,-12,-13,-14,-15,-16,-17,-18)

head(selected_data)

final_data <- selected_data %>%
  geocode(Municipality, method="osm", lat = latitude, long=longitude)

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
  group_by(X_Year.x, Area) %>%
  summarize(avg_price = mean(Average_Price, na.rm = TRUE), .groups = "drop")  # Fixing the column name here

head(avgPrice_area)

# Bar chart of average price by area
ggplot(avgPrice_area, aes(x=Area, y=avg_price)) +
  geom_bar(stat="identity", color="purple") +
  theme_minimal()

# UI
ui <- fluidPage(
  titlePanel("Average Price Trend by Area and Numeric Column Analysis"),

  sidebarLayout(
    sidebarPanel(
      # Dropdown to select Area
      selectInput("selected_area", "Select Area:", choices = unique(avgPrice_area$Area)),

      # Dropdown to select Numeric Column (dynamically based on numeric columns in data)
      selectInput("selected_column", "Select Numeric Column:", choices = colnames(avgPrice_area)[sapply(avgPrice_area, is.numeric)])
    ),

    mainPanel(
      plotOutput("area_plot"),
      plotOutput("box_plot"),
      plotOutput("histogram_plot")
    )
  )
)


# Server
server <- function(input, output) {

  # Plot for average price trend based on selected area
  output$area_plot <- renderPlot({
    # Filter the data based on selected area
    filtered_data <- avgPrice_area %>%
      filter(Area == input$selected_area) %>%
      group_by(X_Year.x) %>%
      summarise(avg_price = mean(avg_price, na.rm = TRUE), .groups = "drop")  # Fixing to use avg_price

    ggplot(filtered_data, aes(x = X_Year.x, y = avg_price)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 2) +
      labs(
        title = paste("Average Price Trend for", input$selected_area),
        x = "Year",
        y = "Average Price"
      ) +
      theme_minimal()
  })

  # Box plot for selected numeric column
  output$box_plot <- renderPlot({
    ggplot(avgPrice_area, aes_string(x = "Area", y = input$selected_column)) +
      geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
      theme_minimal() +
      labs(
        title = paste("Box Plot of", input$selected_column, "by Area"),
        x = "Area",
        y = input$selected_column
      )
  })

  # Histogram for selected numeric column
  output$histogram_plot <- renderPlot({
    ggplot(avgPrice_area, aes_string(x = input$selected_column)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
      theme_minimal() +
      labs(
        title = paste("Histogram of", input$selected_column),
        x = input$selected_column,
        y = "Frequency"
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)







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

