# Load necessary libraries
library(dplyr)
library(Metrics)
library(caret)
library(plotly)

t20i_df <- read.csv("ball_by_ball_it20.csv")

# Select additional relevant columns
selected_data <- t20i_df %>%
  select(Bat.First, Bat.Second, Target.Score, Innings.Runs, Innings.Wickets, Balls.Remaining, Bowler.Runs.Conceded)

# Remove rows with missing values
selected_data <- na.omit(selected_data)

# Drop Irrelevant Columns
irrelevant <- c('Match.ID', 'Date', 'Venue', 'Innings', 'Batter','Non.Striker','Bowler', 
                'Over', 'Ball','Batter.Runs', 'Extra.Runs', 'Runs.From.Ball',
                'Ball.Rebowled', 'Extra.Type', 'Wicket', 'Method', 'Player.Out',
                'Runs.to.Get', 'Balls.Remaining', 'Winner', 'Chased.Successfully', 'Total.Batter.Runs',
                 'Total.Non.Striker.Runs','Batter.Balls.Faced', 'Non.Striker.Balls.Faced',
                 'Player.Out.Runs', 'Player.Out.Balls.Faced', 'Bowler.Runs.Conceded', 'Valid.Ball')

print(paste("Before Removing Irrelevant Columns:", ncol(t20i_df)))
t20i_df <- t20i_df %>%
  select(-one_of(irrelevant))
print(paste("After Removing Irrelevant Columns:", ncol(t20i_df)))

# Define Consistent Teams
const_teams <- c('Afghanistan', 'Australia', 'Bangladesh', 'England', 'India', 'Ireland', 'New Zealand', 'Pakistan', 'South Africa', 'Sri Lanka', 'West Indies', 'Zimbabwe')

print(paste("Before Removing Inconsistent Teams:", nrow(t20i_df)))
# Remove the Non-Consistent Teams
t20i_df <- t20i_df %>%
  filter(`Bat.First` %in% const_teams & `Bat.Second` %in% const_teams)
print(paste("After Removing Inconsistent Teams:", nrow(t20i_df)))

cat("Consistent Teams:\n")
print(unique(t20i_df$`Bat.First`))


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

# Plotting actual vs. predicted scores using Plotly
plot_ly() %>%
  add_trace(data = train_data, type = 'scatter', mode = 'markers', x = ~Target.Score, y = ~linreg_pred_train, name = 'Train', marker = list(color = 'blue', opacity = 0.5)) %>%
  add_trace(data = test_data, type = 'scatter', mode = 'markers', x = ~Target.Score, y = ~linreg_pred_test, name = 'Test', marker = list(color = 'red', opacity = 0.5)) %>%
  add_trace(type = 'scatter', mode = 'line', x = c(0, max(train_data$Target.Score)), y = c(0, max(train_data$Target.Score)), name = 'Identity Line', line = list(dash = 'dash', color = 'black')) %>%
  layout(title = "Actual vs. Predicted Scores",
         xaxis = list(title = "Actual Scores"),
         yaxis = list(title = "Predicted Scores"))
