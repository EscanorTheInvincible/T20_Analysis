
# Install and load necessary libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}

# Load necessary libraries
library(tidyverse)
library(lubridate)


# Read the T20 International cricket data
t20i <- read.csv('ball_by_ball_it20.csv')

# Display a sample of the data
sample_n(t20i, 5)

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
kohli_t20i <- filter(t20i, Batter == full_name)

# Display a sample of Kohli's data
sample_n(kohli_t20i, 6)

# Define recommended columns
recommended_columns <- c(
  'Match ID', 'Date', 'Venue', 'Bat First', 'Bat Second', 'Innings', 'Over', 'Ball',
  'Bowler', 'Batter Runs', 'Runs From Ball', 'Batter Balls Faced', 'Wicket', 'Method',
  'Player Out', 'Player Out Runs', 'Player Out Balls Faced', 'Winner', 'Chased Successfully'
)

# Select only the relevant columns
kohli_t20i <- kohli_t20i[, recommended_columns, with = FALSE]

# Display a sample of Kohli's filtered data
sample_n(kohli_t20i, 5)

# Replace Match ID with Match Number
match_id_mapping <- setNames(seq_along(unique(kohli_t20i$`Match ID`)), unique(kohli_t20i$`Match ID`))
kohli_t20i$`Match ID` <- match_id_mapping[kohli_t20i$`Match ID`]

# Function to get information about the data frame
get_info <- function(dataframe) {
  info <- data.frame(
    Columns = names(dataframe),
    Data_Type = sapply(dataframe, class),
    Missing_Values = colSums(is.na(dataframe)),
    Percentage_Missing = colSums(is.na(dataframe)) / nrow(dataframe)
  )
  return(info)
}

# Display information about Kohli's data
get_info(kohli_t20i)

# Kohli's debut match date
debut_date <- min(kohli_t20i$Date)
date_datetime <- as.Date(debut_date)
formatted_date <- format(date_datetime, '%d %B %Y')

formatted_date

# Total T20I matches played
matches_played <- nunique(kohli_t20i$`Match ID`)
cat("Matches Played:", matches_played, "\n")

# Total runs scored
total_runs_scored <- sum(kohli_t20i$`Batter Runs`, na.rm = TRUE)
cat("Runs Scored:", total_runs_scored, "\n")

# Matches played over the years
kohli_t20i$Date <- as.Date(kohli_t20i$Date)
kohli_t20i$year <- year(kohli_t20i$Date)

matches_played_by_year <- kohli_t20i %>%
  group_by(year) %>%
  summarise(matches_played = n_distinct(`Match ID`))

# Display matches played by year
matches_played_by_year

# Plot matches played by year
plt_matches_played_by_year <- ggplot(matches_played_by_year, aes(x = year, y = matches_played)) +
  geom_line() +
  labs(title = "Matches Played by Year", x = "Year", y = "Matches Played") +
  theme_minimal()

print(plt_matches_played_by_year)

# Runs scored by year
runs_by_year <- kohli_t20i %>%
  group_by(year) %>%
  summarise(`Batter Runs` = sum(`Batter Runs`, na.rm = TRUE)) %>%
  mutate(`Percentage Change` = round(100 * c(NA, diff(`Batter Runs`)) / lag(`Batter Runs`), 2))

# Display runs scored by year
runs_by_year

# Plot runs scored by year
plt_runs_by_year <- ggplot(runs_by_year, aes(x = year, y = `Batter Runs`)) +
  geom_line() +
  labs(title = "Runs Scored by Year", x = "Year", y = "Runs Scored") +
  theme_minimal()

print(plt_runs_by_year)

# Venue statistics
venue_stats <- kohli_t20i %>%
  group_by(Venue) %>%
  summarise(`Batter Runs` = sum(`Batter Runs`, na.rm = TRUE),
            `Batter Balls Faced` = n()) %>%
  mutate(`Strike Rate` = round(100 * `Batter Runs` / `Batter Balls Faced`, 2)) %>%
  arrange(desc(`Batter Runs`))

# Display venue statistics
venue_stats

# Plot top 5 venues by runs scored
plt_top_venues <- ggplot(head(venue_stats, 5), aes(x = reorder(Venue, -`Batter Runs`), y = `Batter Runs`)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 5 Venues by Runs Scored", x = "Venue", y = "Runs Scored") +
  theme_minimal()

print(plt_top_venues)

# Runs by innings
runs_by_innings <- kohli_t20i %>%
  group_by(Innings) %>%
  summarise(`Batter Runs` = sum(`Batter Runs`, na.rm = TRUE),
            `Batter Balls Faced` = n()) %>%
  mutate(`Strike Rate` = round(100 * `Batter Runs` / `Batter Balls Faced`, 2))

# Display runs by innings
runs_by_innings

# Plot runs by innings
plt_runs_by_innings <- ggplot(runs_by_innings, aes(x = factor(Innings), y = `Batter Runs`)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Runs by Innings", x = "Innings", y = "Runs Scored") +
  theme_minimal()

print(plt_runs_by_innings)

# Runs by over
over_runs <- kohli_t20i %>%
  group_by(Over) %>%
  summarise(`Batter Runs` = sum(`Batter Runs`, na.rm = TRUE)) %>%
  arrange(desc(`Batter Runs`))

# Display runs by over
over_runs

# Continue plotting top 5 overs by runs scored
plt_top_overs <- ggplot(head(over_runs, 5), aes(x = reorder(Over, -`Batter Runs`), y = `Batter Runs`)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 5 Overs by Runs Scored", x = "Over", y = "Runs Scored") +
  theme_minimal()

print(plt_top_overs)

# Runs by bowlers
runs_bowlers <- kohli_t20i %>%
  group_by(Bowler) %>%
  summarise(`Batter Runs` = sum(`Batter Runs`, na.rm = TRUE),
            `Match ID` = n()) %>%
  mutate(`Strike Rate` = round(100 * `Batter Runs` / `Match ID`, 2)) %>%
  arrange(desc(`Batter Runs`, `Strike Rate`))

# Display runs by bowlers
runs_bowlers

# Plot top 5 bowlers by runs scored
plt_top_bowlers <- ggplot(head(runs_bowlers, 5), aes(x = reorder(Bowler, -`Batter Runs`), y = `Batter Runs`)) +
  geom_bar(stat = "identity", fill = "viridis") +
  labs(title = "Top 5 Bowlers by Runs Scored", x = "Bowler", y = "Runs Scored") +
  theme_minimal()

print(plt_top_bowlers)

# Total fours
total_fours <- sum(kohli_t20i$`Batter Runs` == 4, na.rm = TRUE)
cat("Total Fours:", total_fours, "\n")

# Percentage of runs by fours
percentage_runs_by_fours <- round(100 * total_fours * 4 / total_runs_scored, 2)
cat("Percentage Runs By Fours:", percentage_runs_by_fours, "%\n")

# Runs distribution
runs_dist <- data.frame(
  Runs = table(kohli_t20i$`Batter Runs`),
  Percentage_Contribution = round(100 * table(kohli_t20i$`Batter Runs`) * as.numeric(names(table(kohli_t20i$`Batter Runs`))) / total_runs_scored, 2)
)

# Display runs distribution
runs_dist

# Plot runs distribution
plt_runs_distribution <- pie(runs_dist$Runs, labels = c('Dots', 'Singles', 'Doubles', 'Triples', 'Fours', 'Sixes'),
                             main = 'Runs Distribution', col = rainbow(length(runs_dist$Runs)))

# Total sixes
total_sixes <- sum(kohli_t20i$`Batter Runs` == 6, na.rm = TRUE)
cat("Total Sixes:", total_sixes, "\n")

# Percentage of runs by sixes
percentage_runs_by_sixes <- round(100 * total_sixes * 6 / total_runs_scored, 2)
cat("Percentage Runs By Sixes:", percentage_runs_by_sixes, "%\n")

# Dismissal type
dismissal_type <- as.data.frame(table(kohli_t20i$Method))

# Display dismissal type
dismissal_type

# Plot dismissal type
plt_dismissal_type <- ggplot(dismissal_type, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Dismissal Type", x = "Dismissal Method", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plt_dismissal_type)

# Create an empty data frame to store results
results_df <- data.frame(Match_ID = character(), Runs_Scored = numeric(), India_Won = logical())

# Create an empty list to store match-wise data
match_data <- list()

# Iterate through rows
for (index in 1:nrow(kohli_t20i)) {
  match_id <- kohli_t20i$`Match ID`[index]
  runs <- kohli_t20i$`Batter Runs`[index]
  is_wicket <- kohli_t20i$Wicket[index]
  winner <- kohli_t20i$Winner[index]
  
  # Check if it's a new match
  if (!(match_id %in% names(match_data))) {
    match_data[[match_id]] <- list(total_runs = 0, india_won = NA)
  }
  
  # Update runs scored by Kohli in the current match
  if (!is.na(runs)) {
    match_data[[match_id]]$total_runs <- match_data[[match_id]]$total_runs + runs
  }
  
  # Check if India won the match
  if (!is.na(winner) && grepl("India", winner)) {
    match_data[[match_id]]$india_won <- TRUE
  } else {
    match_data[[match_id]]$india_won <- FALSE
  }
}

# Populate the results data frame
for (match_id in names(match_data)) {
  total_runs <- match_data[[match_id]]$total_runs
  india_won <- match_data[[match_id]]$india_won
  results_df <- rbind(results_df, data.frame(Match_ID = match_id, Runs_Scored = total_runs, India_Won = india_won))
}

# Display results
results_df

# Plot runs scored vs India won
plt_runs_vs_india_won <- ggplot(results_df, aes(x = India_Won, y = Runs_Scored)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Runs Scored vs India Won", x = "India Won", y = "Runs Scored") +
  theme_minimal()

print(plt_runs_vs_india_won)

# Matches won with fifties
matches_won_with_fifties <- subset(results_df, Runs_Scored >= 50)
percentage_matches_won_with_fifties <- 100 * nrow(matches_won_with_fifties) / matches_played
cat("Percentage of Matches Won with Fifties:", percentage_matches_won_with_fifties, "%\n")

# Plot runs scored vs India won for matches with fifties
plt_runs_vs_india_won_with_fifties <- ggplot(matches_won_with_fifties, aes(x = India_Won, y = Runs_Scored)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Runs Scored vs India Won (Matches with Fifties)", x = "India Won", y = "Runs Scored") +
  theme_minimal()

print(plt_runs_vs_india_won_with_fifties)

# Chased matches
chased_matches <- subset(kohli_t20i, `Chased Successfully` == 1)
chased_matches <- chased_matches %>%
  group_by(`Match ID`) %>%
  summarise(`Batter Runs` = sum(`Batter Runs`, na.rm = TRUE))

# Display runs scored by Kohli in chased matches
head(chased_matches)

# Percentage of matches won while chasing with fifties
percentage_chased_matches_won_with_fifties <- 100 * sum(chased_matches$`Batter Runs` >= 50) / matches_played
cat("Percentage of Matches Won with Fifties while Chasing:", percentage_chased_matches_won_with_fifties, "%\n")

# Runs against all opponents
runs_by_opponent <- kohli_t20i %>%
  filter(`Bat Second` != 'India') %>%
  group_by(`Bat Second`) %>%
  summarise(`Batter Runs` = sum(`Batter Runs`, na.rm = TRUE)) %>%
  arrange(desc(`Batter Runs`))

# Display runs scored against opponents
runs_by_opponent

# Plot runs scored against opponents
plt_runs_against_opponents <- ggplot(head(runs_by_opponent, 10), aes(x = reorder(`Bat Second`, -`Batter Runs`), y = `Batter Runs`)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Runs Scored Against Opponents", x = "Opponent", y = "Runs Scored") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plt_runs_against_opponents)


