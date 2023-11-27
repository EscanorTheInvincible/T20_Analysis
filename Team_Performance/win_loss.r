# Install and load required packages
library(dplyr)
library(ggplot2)

# Loading Data
cricket_data <- read.csv("ball_by_ball_it20.csv")

# Create a new column 'Result' to identify the result of each match
cricket_data <- cricket_data %>%
  mutate(Result = ifelse(Winner == Bat.First, "Win", "Loss"))

# Calculate win/loss records for each team
team_records <- cricket_data %>%
  group_by(Team = ifelse(Result == "Win", Bat.First, Bat.Second)) %>%
  summarise(Wins = sum(Result == "Win"),
            Losses = sum(Result == "Loss"))

# Calculate total matches played by each team
team_records <- team_records %>%
  mutate(TotalMatches = Wins + Losses,
         WinPercentage = (Wins / TotalMatches) * 100)

# Print or visualize the results
print(team_records)

# Create a bar plot to visualize win/loss records
a <- ggplot(team_records, aes(x = reorder(Team, -WinPercentage), y = WinPercentage)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  labs(title = "Win/Loss Records of Teams",
       x = "Team",
       y = "Win Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(a)