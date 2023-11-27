# Read your CSV file
cricket_data <- read.csv("ball_by_ball_it20.csv")
# Check for missing values
# missing_values <- colSums(is.na(cricket_data))
# print("Missing Values:")
# print(missing_values)

# # Check data types
# data_types <- sapply(cricket_data, class)
# print("Data Types:")
# print(data_types)

# # Check for duplicated rows
# duplicated_rows <- cricket_data[duplicated(cricket_data), ]
# print("Duplicated Rows:")
# print(duplicated_rows)

# # Check summary statistics
# summary_stats <- summary(cricket_data)
# print("Summary Statistics:")
# print(summary_stats)

# Check for consistency in categorical variables 
# unique_categories <- unique(cricket_data$Venue)
# print("Unique Categories:")
# print(unique_categories)

# Visualize the distribution of a numeric column
hist(cricket_data$Batter.Runs, main = "Histogram of Numeric_Column", xlab = "Value")

# Visualize the relationship between two variables (replace "X_Column" and "Y_Column" with actual column names)
# plot(cricket_data$X_Column, cricket_data$Y_Column, main = "Scatter Plot", xlab = "X_Column", ylab = "Y_Column")

# Use other   as needed

# Example: Correlation matrix
correlation_matrix <- cor(cricket_data, use = "complete.obs")
print("Correlation Matrix:")
print(correlation_matrix)
