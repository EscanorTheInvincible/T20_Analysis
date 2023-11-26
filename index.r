# Function to perform analysis based on user input
perform_analysis <- function(choice) {
  if (choice == "1") {
    cat("Batsman Analysis \n")
    source("Venue_analysis\\batsman_analysis.r")

  } else if (choice == "2") {
    cat("Bowler Analysis \n")
    source("Venue_analysis\\bowler_analysis.r")

  } else if (choice == "3") {
    cat("Player Performance \n")
    source("Venue_analysis\\player_performance.r")

  } else if (choice == "4"){
    cat("Venue Analysis \n")
    source("Venue_analysis\\venue_analysis.r")

  }
  else {
    cat("Invalid choice. Please enter a valid option.\n")
  }
}

# Main script
cat("Welcome to the Analysis Menu\n")
cat("1. Batsman Analysis\n")
cat("2. Bowler Analysis\n")
cat("3. Player Performance\n")
cat("4. Venue Analysis\n")

# Get user input
user_choice <- readline("Enter the number corresponding to the analysis you want to perform: ")

# Perform analysis based on user input
perform_analysis(user_choice)
