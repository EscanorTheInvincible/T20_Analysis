# Install and load required packages
library(plotly)

# Sample data (replace with your actual dataset)
cricket_data <- read.csv("ball_by_ball_it20.csv")

# Filter data for batsmen from a specific country (e.g., India)
<<<<<<< HEAD
country_of_interest <- "Australia"
=======
country_of_interest <- readline("Target Country : ")
>>>>>>> 7fa300a8df7152f67a02449fcadc1ee5573836ac
batsmen_data <- cricket_data %>%
  filter((Bat.First == country_of_interest & Innings == 1)| (Bat.Second == country_of_interest & Innings == 2)) %>%
  select(Batter, Batter.Runs)

# Summarize total runs for each batsman
total_runs_by_batsman <- batsmen_data %>%
  group_by(Batter) %>%
  summarise(Total_Runs = sum(Batter.Runs, na.rm = TRUE))

# Create an interactive bar plot using Plotly
a <- plot_ly(total_runs_by_batsman, x = ~reorder(Batter, -Total_Runs), y = ~Total_Runs, type = "bar") %>%
  layout(title = paste("Total Runs by Batsmen from", country_of_interest),
         xaxis = list(title = "Batsman"),
         yaxis = list(title = "Total Runs"))
print(a)
