library(PlayerRatings)
library(tidyverse)

results$result <- 1
w_results$result <- 1

results[, c(1, 2, 5, 6, 8:33)] <- list(NULL)
w_results[, c(1, 2, 5, 6, 8:33)] <- list(NULL)

for (i in 1:nrow(results)) {
  if (results$winner_id[i] >= 200000) {
    results$winner_id[i] <- results$winner_id[i] + 900000
  } 
  if (results$loser_id[i] >= 200000) {
    results$loser_id[i] <- results$loser_id[i] + 900000
  }
}

full_results <- rbind(results, w_results)

full_results <- full_results %>% arrange(tourney_date)

full_elo <- glicko2(full_results, init = c(1500, 350, 0.06), tau = 0.5, history = TRUE)

setwd("..")
current_men <- read.csv("current_men.csv", row.names = 1)
current_women <- read.csv("current_women.csv", row.names = 1)
setwd("Predictive Model")

current_elo <- full_elo$ratings[, 1:4]

current_players <- rbind(current_men, current_women)

players_with_elo <- merge(x = current_players, y = current_elo, by.x = c("player_id"), by.y = c("Player"))

row.names(players_with_elo) <- players_with_elo$player_id

current_men$Rating <- 1500
current_men$Deviation <- 350
current_men$Volatility <- 0.06
current_women$Rating <- 1500
current_women$Deviation <- 350
current_women$Volatility <- 0.06

for (i in current_players$player_id) {
  if (i >= 200000 & i <= 300000) {
    current_women[as.character(i),] <- players_with_elo[as.character(i),]
  } else {
    current_men[as.character(i),] <- players_with_elo[as.character(i),]
  }
}

write.csv(current_men, "current_men.csv", row.names = FALSE)
write.csv(current_women, "current_women.csv", row.names = FALSE)

elo_hist <- full_elo$history[, , 1]

colnames(elo_hist) <- unique(full_results$tourney_date)

elo_hist <- data.frame(elo_hist)

full_results$winner_rank <- 1500
full_results$loser_rank <- 1500

for (i in 1:nrow(full_results)) {
  full_results$winner_rank[i] <- elo_hist[as.character(full_results$winner_id[i]), paste("X", as.character(full_results$tourney_date[i]), sep = "")]
  full_results$loser_rank[i] <- elo_hist[as.character(full_results$loser_id[i]), paste("X", as.character(full_results$tourney_date[i]), sep = "")]
}

keep_rows <- c()

for (i in 1:nrow(full_results)) {
  if (full_results$winner_id[i] %in% current_players$player_id | full_results$loser_id[i] %in% current_players$player_id) {
    keep_rows <- c(keep_rows, i)
  }
}

full_results <- full_results[keep_rows , ]

results <- results %>% arrange(tourney_date)
w_results <- w_results %>% arrange(tourney_date)

results$winner_rank <- 1500
results$loser_rank <- 1500
w_results$winner_rank <- 1500
w_results$loser_rank <- 1500

j <- 1
k <- 1

for (i in 1:nrow(full_results)) {
  if ((full_results$winner_id[i] >= 200000 & full_results$winner_id[i] <= 300000) | (full_results$loser_id[i] >= 200000 & full_results$loser_id[i] <= 300000)) {
    w_results$winner_rank[j] <- full_results$winner_rank[i]
    w_results$loser_rank[j] <- full_results$loser_rank[i]
    j <- j + 1
  } else {
    results$winner_rank[k] <- full_results$winner_rank[i]
    results$loser_rank[k] <- full_results$loser_rank[i]
    k <- k + 1
  }
}



write.csv(results, "mens_singles_elo.csv")
write.csv(w_results, "womens_singles_elo.csv")
