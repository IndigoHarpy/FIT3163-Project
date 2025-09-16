predict_result <- function (player_1, player_2, surface, tourney_level, round, match_dur, matches, players) {
  library(tidyverse)
  library(ranger)
  
  # Creates dataframe with given player name
  player_1 <- data.frame(name = player_1)
  
  # Splits player name into first and last name
  player_1 <- extract(player_1, name, c("name_first", "name_last"), "([^ ]+) (.*)")
  
  # Gets remaining player information from player dataset
  player_1 <- merge(players, player_1, by=c("name_first", "name_last"))
  
  player_1$dob <- as.Date(as.character(player_1$dob), format = "%Y%m%d")
  
  # Creates dataframe with given player name
  player_2 <- data.frame(name = player_2)
  
  # Splits player name into first and last name
  player_2 <- extract(player_2, name, c("name_first", "name_last"), "([^ ]+) (.*)")
  
  # Gets remaining player information from player dataset
  player_2 <- merge(players, player_2, by=c("name_first", "name_last"))
  
  player_2$dob <- as.Date(as.character(player_2$dob), format = "%Y%m%d")
  
  model_1 <- forest_model(player_1, matches, players)
  rf_1 <- model_1[1][[1]]
  rf_1_acc <- model_1[2][[1]]
  
  model_2 <- forest_model(player_2, matches, players)
  rf_2 <- model_2[1][[1]]
  rf_2_acc <- model_2[2][[1]]
  
  match_data_1 <- data.frame("surface" = surface, "tourney_level" = tourney_level, "opp_hand" = player_2$hand, "round" = round, "minutes" = match_dur, "ht_diff" = player_1$height - player_2$height, "age_diff" = as.numeric(player_1$dob - player_2$dob)/365, "rank_diff" = player_1$rank - player_2$rank)
  
  match_data_2 <- data.frame("surface" = surface, "tourney_level" = tourney_level, "opp_hand" = player_1$hand, "round" = round, "minutes" = match_dur, "ht_diff" = player_2$height - player_1$height, "age_diff" = as.numeric(player_2$dob - player_1$dob)/365, "rank_diff" = player_2$rank - player_1$rank)
  
  player_1_outcome <- predict(rf_1, match_data_1)$predictions
  player_2_outcome <- predict(rf_2, match_data_2)$predictions
  
  if (player_1_outcome != player_2_outcome) {
    return(as.character(player_1_outcome))
  } else {
    return("contradiction")
  }
}

forest_model <- function (player, matches, players) {
  matches_played <- get_matches(player, matches, players)
  matches_played$result <- "NA"
  matches_played$opp_hand <- "U"
  
  for (i in 1:nrow(matches_played)) {
    if (matches_played$winner_hand[i] == "U") {
      matches_played$winner_hand[i] <- NULL
    }
    if (matches_played$loser_hand[i] == "U") {
      matches_played$loser_hand[i] <- NULL
    }
    if (matches_played$winner_id[i] == player$player_id) {
      matches_played$result[i] <- "W"
      matches_played$opp_hand <- matches_played$loser_hand
    } else if (matches_played$loser_id[i] == player$player_id) {
      matches_played$result[i] <- "L"
      matches_played$ht_diff[i] <- -matches_played$ht_diff[i]
      matches_played$age_diff[i] <- -matches_played$age_diff[i]
      matches_played$rank_diff[i] <- -matches_played$rank_diff[i]
      matches_played$opp_hand <- matches_played$winner_hand
    }
  }
  
  keep_cols <- c("surface", "tourney_level", "opp_hand", "round", "minutes", "ht_diff", "age_diff", "rank_diff", "result")
  
  matches_played <- matches_played[, keep_cols]
  
  matches_played$result <- as.factor(matches_played$result)
  
  selecter <- as.logical(rbinom(n = nrow(matches_played), size = 1, prob = 0.7))
  match.train <- matches_played[selecter, ]
  match.test <- matches_played[!selecter, ]
  
  rf_model <- ranger(result ~ ., data = match.train)
  
  rf_pred <- predict(rf_model, data = match.test)
  
  conf_matrix = table(Predicted_Class = rf_pred$predictions, Actual_Class = match.test$result)
  
  forest.accuracy <- sum(conf_matrix[2, 2], conf_matrix[1, 1])/sum(conf_matrix)
  
  return(list(rf_model, forest.accuracy))
}

get_matches <- function(player, matches, players) {
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
