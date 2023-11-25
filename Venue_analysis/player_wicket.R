# Assuming your_data is the data frame with the provided columns

# Install and load required packages
library(tidyverse)
library(plotly)
cricket_data <- read.csv("ball_by_ball_it20.csv")
target_batter <- "V Kohli"
target_country <- "South Africa"

# Example: Create a bar chart for the number of times the batter got out by each bowler from a specific country
dismissals_data <- cricket_data %>%
  filter(Batter == target_batter, Bowler.Country == target_country, !is.na(Player_Out)) %>%
  group_by(Player_Out) %>%
  summarise(Dismissals = n())

# Create an interactive bar chart
plot_ly(
  data = dismissals_data,
  x = ~Player_Out,
  y = ~Dismissals,
  type = "bar",
  text = ~paste("Bowler: ", Player_Out, "<br>Dismissals: ", Dismissals),
  hoverinfo = "text"
) %>%
  layout(
    title = paste("Number of Dismissals for", target_batter, "against Bowlers from", target_country),
    xaxis = list(title = "Bowler"),
    yaxis = list(title = "Number of Dismissals")
  )
