
# install.packages("dplyr")
# install.packages("ggplot2")
# library(plotly)

# Importing dataset
ipl_df <- read.csv('ball_by_ball_it20.csv')
cat(paste("Dataset successfully Imported of Shape : ", dim(ipl_df), "\n"))

# First 5 Columns Data
# head(ipl_df)

# Describing the ipl_dfset
# summary(ipl_df)

# Information about Each Column
# str(ipl_df)

# Number of Unique Values in each column
unique_counts <- sapply(ipl_df, function(x) length(unique(x)))
#print(unique_counts)

# Wickets Distribution
# hist(ipl_df$Wicket, breaks = 10, main = "Wickets Distribution", xlab = "Wickets", ylab = "Frequency", col = "blue")

# Runs Distribution
# hist(ipl_df$Batter.Runs, breaks = 10, main = "Runs Distribution", xlab = "Runs", ylab = "Frequency", col = "green")

# Names of all columns
print(names(ipl_df))

# Calculate 'overs' column
ipl_df$overs <- as.numeric(paste(ipl_df$Over, ".", ipl_df$Ball, sep = "")) - 1

# Calculate 'runs_last_5_overs' and 'wickets_last_5_overs' based on conditions
for (index in 1:nrow(ipl_df)) {
  if (ipl_df$`Balls.Remaining`[index] < 89) {
    # Calculate runs and wickets for the last 5 overs
    ipl_df$runs_last_5_overs[index] <- ipl_df$`Innings Runs`[index] - ipl_df$`Innings Runs`[index - 30]
    ipl_df$wickets_last_5_overs[index] <- ipl_df$`Innings Wickets`[index] - ipl_df$`Innings Wickets`[index - 30]
  } else {
    # Assign current innings runs and wickets when not in the last 5 overs
    ipl_df$runs_last_5_overs[index] <- ipl_df$`Innings Runs`[index]
    ipl_df$wickets_last_5_overs[index] <- ipl_df$`Innings Wickets`[index]
  }
}

# Display the modified dataset
ipl_df

# Define irrelevant columns to be removed
irrelevant <- c('Unnamed: 0', 'Match.ID', 'Date', 'Venue', 'Innings', 'Batter', 'Non.Striker', 'Bowler', 'Over', 'Ball',
                 'Batter.Runs', 'Extra.Runs', 'Runs.From.Ball', 'Ball.Rebowled',
                 'Extra.Type', 'Wicket', 'Method', 'Player.Out', 'Runs.to.Get', 'Balls.Remaining',
                 'Winner', 'Chased.Successfully', 'Total.Batter.Runs',
                 'Total.Non.Striker.Runs', 'Batter.Balls.Faced',
                 'Non.Striker.Balls.Faced', 'Player.Out.Runs', 'Player.Out.Balls.Faced',
                 'Bowler.Runs.Conceded', 'Valid.Ball')

# Print the number of columns before and after removing irrelevant columns
print(paste("Before Removing Irrelevant Columns:", ncol(ipl_df)))
ipl_df <- ipl_df[, !(names(ipl_df) %in% irrelevant)]  # Drop Irrelevant Columns
print(paste("After Removing Irrelevant Columns:", ncol(ipl_df)))

# Display the first 124 rows of the modified dataset
head(ipl_df, 124)

# Define Consistent Teams
const_teams <- c('Afghanistan', 'Australia', 'Bangladesh',
                 'England', 'India', 'Ireland',
                 'New.Zealand', 'Pakistan', 'South.Africa', 'Sri.Lanka', 'West.Indies', 'Zimbabwe')

# Print Before Removing Inconsistent Teams
cat(paste("Before Removing Inconsistent Teams: ", nrow(ipl_df), " rows\n"))

# Remove Inconsistent Teams
ipl_df <- ipl_df[ipl_df$Bat.First %in% const_teams & ipl_df$Bat.Second %in% const_teams, ]

# Print After Removing Inconsistent Teams
cat(paste("After Removing Inconsistent Teams: ", nrow(ipl_df), " rows\n"))

# Print Consistent Teams
cat("Consistent Teams:\n", unique(ipl_df$Bat.First), "\n")

# Display the head of the data frame
head(ipl_df)


# Print Before Removing Overs
cat(paste("Before Removing Overs: ", nrow(ipl_df), " rows\n"))

# Remove Overs less than 5.0
ipl_df <- ipl_df[ipl_df$overs >= 5.0, ]

# Print After Removing Overs
cat(paste("After Removing Overs: ", nrow(ipl_df), " rows\n"))

# Display the head of the data frame
head(ipl_df)

# Correlation heatmap
numerical_ipl_df <- ipl_df %>%
  select_if(is.numeric)

# Install and load the corrplot library if not already installed
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}
library(corrplot)

# Select numerical columns
numerical_ipl_df <- ipl_df[, sapply(ipl_df, is.numeric)]

# Calculate correlation matrix
cor_matrix <- cor(numerical_ipl_df)

# Plot heatmap
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 45)

##################################################################################################
##################################################################################################

# Load the necessary libraries
library(caret)
library(Matrix)

# Specify the columns for label encoding
columns_to_encode <- c('Bat.First', 'Bat.Second')

# Label encoding using the 'factor' function
ipl_df[columns_to_encode] <- lapply(ipl_df[columns_to_encode], as.factor)

# Load the necessary library
library(caret)

# Specify the columns to be one-hot encoded
columns_to_encode <- c('Bat.First', 'Bat.Second')

# One-hot encoding using 'dummyVars'
dummy_var <- dummyVars(" ~ .", data = ipl_df[, columns_to_encode], fullRank = TRUE)

# Applying the encoding to the data
encoded_data <- predict(dummy_var, newdata = ipl_df[, columns_to_encode])

# Combining the encoded data with the original dataset
ipl_df <- cbind(ipl_df, encoded_data)

# Removing the original columns that were encoded
ipl_df <- ipl_df[, -which(names(ipl_df) %in% columns_to_encode)]

# Convert to sparse matrix
sparse_matrix <- sparse.model.matrix(~ ., data = ipl_df)

# Convert the sparse matrix to a regular matrix
ipl_matrix <- as.matrix(sparse_matrix)

# Create the list of column names for the DataFrame
feature_names <- colnames(ipl_matrix)

# Define the columns you want in the DataFrame
cols <- c('Innings.Runs', 'Innings.Wickets', 'Target.Score', 'overs',
          'Bat.First.Australia', 'Bat.First.Bangladesh', 'Bat.First.England',
          'Bat.First.India', 'Bat.First.Ireland', 'Bat.First.Pakistan', 'Bat.First.Zimbabwe',
          'Bat.Second.Australia', 'Bat.Second.Bangladesh', 'Bat.Second.England',
          'Bat.Second.India', 'Bat.Second.Ireland', 'Bat.Second.Pakistan', 'Bat.Second.Zimbabwe')


# Check if specified columns exist in the ipl_matrix
df <- data.frame(ipl_matrix[, cols])
  
# Display the head of the DataFrame
head(df)

# Specify the target variable
target_column <- 'Target.Score'

# Extract features and labels
features <- df[, !(names(df) %in% c(target_column))]
labels <- df[[target_column]]

# Set seed for reproducibility
set.seed(123)

# Split the dataset into training and testing sets
split_index <- createDataPartition(labels, p = 0.8, list = FALSE)
train_features <- features[split_index, ]
test_features <- features[-split_index, ]
train_labels <- labels[split_index]
test_labels <- labels[-split_index]

# Print the dimensions of the sets
cat(paste("Training Set: ", dim(train_features), "\n"))
cat(paste("Testing Set: ", dim(test_features), "\n"))

##################################################################################################
##################################################################################################

# Load necessary libraries
# install.packages("tree")
# install.packages("Metrics")

library(Metrics)
library(tree)


# Decision Tree Regressor
tree_model <- tree(Target.Score ~ ., data = df)
# Train Model
tree_pred_train <- predict(tree_model, newdata = train_features)
tree_pred_test <- predict(tree_model, newdata = test_features)

# Evaluate Model
train_score_tree <- R2(tree_pred_train, train_labels)
test_score_tree <- R2(tree_pred_test, test_labels)
cat(paste("Train Score: ", round(train_score_tree * 100, 2), "%\n"))
cat(paste("Test Score: ", round(test_score_tree * 100, 2), "%\n"))

# Predictions on the test set
tree_pred_test <- predict(tree, newdata = test_features)
# Calculate Mean Squared Error
mse_tree <- mean((tree_pred_test - test_labels)^2)

# Model Evaluation
cat("---- Decision Tree Regressor - Model Evaluation ----\n")
cat("Mean Absolute Error (MAE): ", MAE(tree_pred_test, test_labels), "\n")
cat("Mean Squared Error (MSE): ", MSE(tree_pred_test, test_labels), "\n")
cat("Root Mean Squared Error (RMSE): ", sqrt(MSE(tree_pred_test, test_labels)), "\n")

# Linear Regression
linreg_model <- lm(Target.Score ~ ., data = df)
# Train Model
linreg_pred_train <- predict(linreg_model, newdata = train_features)
linreg_pred_test <- predict(linreg_model, newdata = test_features)

# Evaluate Model
train_score_linreg <- R2(linreg_pred_train, train_labels)
test_score_linreg <- R2(linreg_pred_test, test_labels)
cat(paste("Train Score: ", round(train_score_linreg * 100, 2), "%\n"))
cat(paste("Test Score: ", round(test_score_linreg * 100, 2), "%\n"))

# Model Evaluation
cat("---- Linear Regression - Model Evaluation ----\n")
cat("Mean Absolute Error (MAE): ", MAE(linreg_pred_test, test_labels), "\n")
cat("Mean Squared Error (MSE): ", MSE(linreg_pred_test, test_labels), "\n")
cat("Root Mean Squared Error (RMSE): ", sqrt(MSE(linreg_pred_test, test_labels)), "\n")

