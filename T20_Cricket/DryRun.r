# # Load necessary libraries
library(tidyverse)
library(lubridate)

# Set your directory path
directory_path <- "ball_by_ball_it20.csv"

# List files in the directory
files <- list.files(directory_path, full.names = TRUE)
files

# Read the CSV file into a data frame
t20i <- read.csv('ball_by_ball_it20.csv')

# Display a sample of the data
head(t20i, 5)

# Display column names
names(t20i)

# Get the full name of Virat Kohli
full_name <- ''
for (name in unique(t20i$Batter)) {
  if ('kohli' %in% tolower(name)) {
    full_name <- name
    break
  }
}

full_name

# Filter data for Virat Kohli
kohli_t20i <- t20i[t20i$Batter == full_name, ]

recommended_columns <- c(
  'X', 'Match.ID', 'Date', 'Venue', 'Bat.First', 'Bat.Second', 'Innings', 'Over', 'Ball',
  'Batter', 'Non.Striker', 'Bowler', 'Batter.Runs', 'Extra.Runs', 'Runs.From.Ball', 'Ball.Rebowled',
  'Extra.Type', 'Wicket', 'Method', 'Player.Out', 'Innings.Runs', 'Innings.Wickets', 'Target.Score',
  'Runs.to.Get', 'Balls.Remaining', 'Winner', 'Chased.Successfully', 'Total.Batter.Runs',
  'Total.Non.Striker.Runs', 'Batter.Balls.Faced', 'Non.Striker.Balls.Faced', 'Player.Out.Runs',
  'Player.Out.Balls.Faced', 'Bowler.Runs.Conceded', 'Valid.Ball'
)

# Select only the recommended columns
kohli_t20i <- kohli_t20i[, recommended_columns]

# Display a sample of Kohli's T20 data
head(kohli_t20i, 5)

# Create an empty match ID mapping
match_id_mapping <- c()
match_id_counter <- 1

# Loop through unique match IDs and create a mapping
for (match_id in unique(kohli_t20i$Match.ID)) {
  match_id_mapping[match_id] <- paste("Match", match_id_counter)
  match_id_counter <- match_id_counter + 1
}

# Display the number of missing values in each column
missing_values <- colSums(is.na(kohli_t20i))
print(missing_values)

# Map the new match IDs to the data frame
kohli_t20i$Match.ID <- match_id_mapping[kohli_t20i$Match.ID]

get_info <- function(dataframe) {
  info <- data.frame(
    Columns = names(dataframe),
    Data_Type = sapply(dataframe, class),
    Missing_Values = colSums(is.na(dataframe)),
    Percentage_Missing = colSums(is.na(dataframe)) / nrow(dataframe)
  )
  return(info)
}

info <- get_info(kohli_t20i)
 
# Total T20I matches played
matches_played <- length(unique(kohli_t20i$Match.ID))
cat("Matches Played:", matches_played, "\n")

