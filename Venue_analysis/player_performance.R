# Install and load required packages
library(tidyverse)
library(plotly)

# Read your CSV file
cricket_data <- read.csv("ball_by_ball_it20.csv")
# print(head(cricket_data))

# print(colnames(cricket_data))
# # Assuming 'Player_Name' is the player you want to track
# player_name <- "V Kohli"

# # Filter data for the specific player
# player_data <- cricket_data %>%
#   filter(Batter == player_name)

# # Convert 'Date' to a date format
# player_data$Date <- as.Date(player_data$Date)

# # Create an interactive line plot
# a <- plot_ly(
#   data = player_data,
#   x = ~Date,
#   y = ~Total.Batter.Runs,
#   type = "scatter",
#   name = player_name,
#   text = ~paste("Date: ", Date, "<br>Total Batter Runs: ", Total.Batter.Runs),
#   hoverinfo = "text"
# ) %>%
#   layout(
#     title = paste("Player's Performance Over the Years -", player_name),
#     xaxis = list(title = "Date"),
#     yaxis = list(title = "Total Batter Runs")
#   )

# print(a)



bowler_data <- cricket_data %>%
  group_by(Bowler) %>%
  summarise(Total_Runs_Conceded = sum(Bowler.Runs.Conceded))

# Create an interactive bar chart
a <- plot_ly(
  data = bowler_data,
  x = ~Bowler,
  y = ~Total_Runs_Conceded,
  type = "bar",
  text = ~paste("Bowler: ", Bowler, "<br>Total Runs Conceded: ", Total_Runs_Conceded),
  hoverinfo = "text"
) %>%
  layout(
    title = "Total Runs Conceded by Bowler",
    xaxis = list(title = "Bowler"),
    yaxis = list(title = "Total Runs Conceded")
  )
print(a)