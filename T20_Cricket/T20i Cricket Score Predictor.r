# Install and load necessary libraries
install.packages(c("dplyr", "ggplot2", "Metrics","caret"))
library(dplyr)
library(ggplot2)
library(Metrics)
library(caret)

# Importing dataset
ipl_df <- read.csv('ball_by_ball_it20.csv')
cat(paste("Dataset successfully Imported of Shape : ", dim(ipl_df), "\n"))

# Select relevant columns
selected_data <- ipl_df %>%
  select(Bat.First,Bat.Second,Target.Score, Innings.Runs, Innings.Wickets, Balls.Remaining, Bowler.Runs.Conceded)

# Remove rows with missing values
selected_data <- na.omit(selected_data)

# Split the dataset into training and testing sets
set.seed(123)
split_index <- createDataPartition(selected_data$Target.Score, p = 0.8, list = FALSE)
train_data <- selected_data[split_index, ]
test_data <- selected_data[-split_index, ]

# Linear Regression
linreg_model <- lm(Target.Score ~ ., data = train_data)
linreg_pred_train <- predict(linreg_model, newdata = train_data)
linreg_pred_test <- predict(linreg_model, newdata = test_data)

# Evaluate Linear Regression with Mean Absolute Error (MAE)
train_mae_linreg <- MAE(linreg_pred_train, train_data$Target.Score)
test_mae_linreg <- MAE(linreg_pred_test, test_data$Target.Score)
cat(paste("Linear Regression - Train MAE: ", round(train_mae_linreg, 2), "\n"))
cat(paste("Linear Regression - Test MAE: ", round(test_mae_linreg, 2), "\n"))

# Plotting actual vs. predicted scores
ggplot() +
  geom_point(data = train_data, aes(x = Target.Score, y = linreg_pred_train), color = "blue", alpha = 0.5, size = 2, show.legend = TRUE) +
  geom_point(data = test_data, aes(x = Target.Score, y = linreg_pred_test), color = "red", alpha = 0.5, size = 2, show.legend = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Actual vs. Predicted Scores",
       x = "Actual Scores",
       y = "Predicted Scores") +
  theme_minimal()
