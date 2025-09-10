get_matches <- function(player, matches, players) {
  # Creates dataframe with given player name
  player <- data.frame(name = player)
  
  # Splits player name into first and last name
  player <- extract(player, name, c("name_first", "name_last"), "([^ ]+) (.*)")
  
  # Gets remaining player information from player dataset
  player <- merge(players, player, by=c("name_first", "name_last"))
  
  # Creates empty dataframe for matches they have played in
  played_matches <- data.frame(matches[FALSE,])
  
  # Loops through all matches played and checks if the player id matches either winner or loser id
  for (i in 1:nrow(matches)) {
    if (matches$winner_id[i] == player$player_id | matches$loser_id[i] == player$player_id) {
      # If it matches then adds to match dataframe
      played_matches <- rbind(played_matches, matches[i,])
    }
  }
  # Returns dataframe of matches
  return(played_matches)
}

# By running the function it will be in the environment and can be easily called throughout your code, no need to copy and paste.

# Example of running the code with Zverev. Can be run with women or men, just need to specify the player dataset to use
test_results <- get_matches("Alexander Zverev", results, men)
