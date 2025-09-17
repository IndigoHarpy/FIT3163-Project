library(tidyverse)

results <- read.csv("mens_singles.csv")
w_results <- read.csv("womens_singles.csv")
men <- read.csv("current_men.csv", row.names = 1)
women <- read.csv("current_women.csv", row.names = 1)

results$tourney_date <- as.Date(as.character(results$tourney_date), format = "%Y%m%d")
w_results$tourney_date <- as.Date(as.character(w_results$tourney_date), format = "%Y%m%d")

results$year <- format(results$tourney_date, "%Y")
w_results$year <- format(w_results$tourney_date, "%Y")

# Win Loss Summary
yearly_wins <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(matches_won = n())

yearly_losses <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(matches_lost = n())

win_loss_summary <- merge(x = yearly_wins, y = yearly_losses, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

win_loss_summary[is.na(win_loss_summary)] <- 0

win_loss_summary$total_matches <- win_loss_summary$matches_won + win_loss_summary$matches_lost

win_loss_summary$matches_lost <- NULL

colnames(win_loss_summary)[2] <- "player_id"

win_loss_summary <- merge(win_loss_summary, men)[1:4]

yearly_wins <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(matches_won = n())

yearly_losses <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(matches_lost = n())

w_win_loss_summary <- merge(x = yearly_wins, y = yearly_losses, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

w_win_loss_summary[is.na(w_win_loss_summary)] <- 0

w_win_loss_summary$total_matches <- w_win_loss_summary$matches_won + w_win_loss_summary$matches_lost

w_win_loss_summary$matches_lost <- NULL

colnames(w_win_loss_summary)[2] <- "player_id"

w_win_loss_summary <- merge(w_win_loss_summary, women)[1:4]

win_loss_summary <- rbind(win_loss_summary, w_win_loss_summary)

write.csv(win_loss_summary, "win_loss.csv")

# Double faults
df_serves_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(double_fault = sum(w_df),
            service_points = sum(w_svpt))

df_serves_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(double_fault = sum(l_df),
            service_points = sum(l_svpt))

df_serves <- merge(x = df_serves_w, y = df_serves_l, by.x = c("year", "winner_id", "double_fault", "service_points"), by.y = c("year", "loser_id", "double_fault", "service_points"), all = TRUE)

colnames(df_serves)[2] <- "player_id"

df_serves <- merge(df_serves, men)[1:4]

df_serves_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(double_fault = sum(w_df),
            service_points = sum(w_svpt))

df_serves_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(double_fault = sum(l_df),
            service_points = sum(l_svpt))

w_df_serves <- merge(x = df_serves_w, y = df_serves_l, by.x = c("year", "winner_id", "double_fault", "service_points"), by.y = c("year", "loser_id", "double_fault", "service_points"), all = TRUE)

colnames(w_df_serves)[2] <- "player_id"

w_df_serves <- merge(w_df_serves, women)[1:4]

df_serves <- rbind(df_serves, w_df_serves)

na_presence <- aggregate(double_fault ~ player_id, data=df_serves, function(x) {sum(is.na(x))}, na.action = NULL)

data_presence <- df_serves %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n())

na_in_data <- merge(na_presence, data_presence)

absent_players <- c()

for (i in 1:nrow(na_in_data)) {
  if (na_in_data$total_matches[i]/2 >= na_in_data$double_fault[i]) {
    absent_players <- c(absent_players, i)
  }
}

df_serves <- df_serves[-absent_players, ]

write.csv(df_serves, "df_serves.csv")

# 1st Serves Won
first_serves_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_serve = sum(w_1stIn),
            first_won = sum(w_1stWon))

first_serves_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_serve = sum(l_1stIn),
            first_won = sum(l_1stWon))

first_serves <- merge(x = first_serves_w, y = first_serves_l, by.x = c("year", "winner_id", "first_serve", "first_won"), by.y = c("year", "loser_id", "first_serve", "first_won"), all = TRUE)

colnames(first_serves)[2] <- "player_id"

first_serves <- merge(first_serves, men)[1:4]

first_serves_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_serve = sum(w_1stIn),
            first_won = sum(w_1stWon))

first_serves_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_serve = sum(l_1stIn),
            first_won = sum(l_1stWon))

w_first_serves <- merge(x = first_serves_w, y = first_serves_l, by.x = c("year", "winner_id", "first_serve", "first_won"), by.y = c("year", "loser_id", "first_serve", "first_won"), all = TRUE)

colnames(w_first_serves)[2] <- "player_id"

w_first_serves <- merge(w_first_serves, women)[1:4]

first_serves <- rbind(first_serves, w_first_serves)

na_presence <- aggregate(first_serve ~ player_id, data = first_serves, function(x) {sum(is.na(x))}, na.action = NULL)

data_presence <- first_serves %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n())

na_in_data <- merge(na_presence, data_presence)

absent_players <- c()

for (i in 1:nrow(na_in_data)) {
  if (na_in_data$total_matches[i]/2 >= na_in_data$first_serve[i]) {
    absent_players <- c(absent_players, i)
  }
}

first_serves <- first_serves[-absent_players, ]

write.csv(first_serves, "first_serves.csv")

# Second Serves Won
second_serves_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_serve = sum(w_1stIn),
            second_won = sum(w_2ndWon),
            serve_point = sum(w_svpt),
            double_fault = sum(w_df))

second_serves_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_serve = sum(l_1stIn),
            second_won = sum(l_2ndWon),
            serve_point = sum(l_svpt),
            double_fault = sum(l_df))

second_serves <- merge(x = second_serves_w, y = second_serves_l, by.x = c("year", "winner_id", "first_serve", "second_won", "serve_point", "double_fault"), by.y = c("year", "loser_id", "first_serve", "second_won", "serve_point", "double_fault"), all = TRUE)

colnames(second_serves)[2] <- "player_id"

second_serves$second_serve <- second_serves$serve_point - second_serves$first_serve - second_serves$double_fault

second_serves$serve_point <- NULL
second_serves$double_fault <- NULL
second_serves$first_serve <- NULL

second_serves <- merge(second_serves, men)[1:4]

second_serves_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_serve = sum(w_1stIn),
            second_won = sum(w_2ndWon),
            serve_point = sum(w_svpt),
            double_fault = sum(w_df))

second_serves_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_serve = sum(l_1stIn),
            second_won = sum(l_2ndWon),
            serve_point = sum(l_svpt),
            double_fault = sum(l_df))

w_second_serves <- merge(x = second_serves_w, y = second_serves_l, by.x = c("year", "winner_id", "first_serve", "second_won", "serve_point", "double_fault"), by.y = c("year", "loser_id", "first_serve", "second_won", "serve_point", "double_fault"), all = TRUE)

colnames(w_second_serves)[2] <- "player_id"

w_second_serves$second_serve <- w_second_serves$serve_point - w_second_serves$first_serve - w_second_serves$double_fault

w_second_serves$serve_point <- NULL
w_second_serves$double_fault <- NULL
w_second_serves$first_serve <- NULL

w_second_serves <- merge(w_second_serves, women)[1:4]

second_serves <- rbind(second_serves, w_second_serves)

na_presence <- aggregate(second_serve ~ player_id, data = second_serves, function(x) {sum(is.na(x))}, na.action = NULL)

data_presence <- second_serves %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n())

na_in_data <- merge(na_presence, data_presence)

absent_players <- c()

for (i in 1:nrow(na_in_data)) {
  if (na_in_data$total_matches[i]/2 >= na_in_data$second_serve[i]) {
    absent_players <- c(absent_players, i)
  }
}

second_serves <- second_serves[-absent_players, ]

write.csv(second_serves, "second_serves.csv")

# Break Points
bp_save_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(bp_faced = sum(w_bpFaced),
            bp_saved = sum(w_bpSaved))

bp_save_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(bp_faced = sum(l_bpFaced),
            bp_saved = sum(l_bpSaved))

bp_save <- merge(x = bp_save_w, y = bp_save_l, by.x = c("year", "winner_id", "bp_faced", "bp_saved"), by.y = c("year", "loser_id", "bp_faced", "bp_saved"), all = TRUE)

colnames(bp_save)[2] <- "player_id"

bp_save <- merge(bp_save, men)[1:4]

bp_save_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(bp_faced = sum(w_bpFaced),
            bp_saved = sum(w_bpSaved))

bp_save_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(bp_faced = sum(l_bpFaced),
            bp_saved = sum(l_bpSaved))

w_bp_save <- merge(x = bp_save_w, y = bp_save_l, by.x = c("year", "winner_id", "bp_faced", "bp_saved"), by.y = c("year", "loser_id", "bp_faced", "bp_saved"), all = TRUE)

colnames(w_bp_save)[2] <- "player_id"

w_bp_save <- merge(w_bp_save, women)[1:4]

bp_save <- rbind(bp_save, w_bp_save)

na_presence <- aggregate(bp_faced ~ player_id, data=bp_save, function(x) {sum(is.na(x))}, na.action = NULL)

data_presence <- bp_save %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n())

na_in_data <- merge(na_presence, data_presence)

absent_players <- c()

for (i in 1:nrow(na_in_data)) {
  if (na_in_data$total_matches[i]/2 >= na_in_data$bp_faced[i]) {
    absent_players <- c(absent_players, i)
  }
}

bp_save <- bp_save[-absent_players, ]

write.csv(bp_save, "bp_save.csv")

# Aces
aces_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(aces = sum(w_ace),
            serves = sum(w_svpt))

aces_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(aces = sum(l_ace),
            serves = sum(l_svpt))

aces <- merge(x = aces_w, y = aces_l, by.x = c("year", "winner_id", "aces", "serves"), by.y = c("year", "loser_id", "aces", "serves"), all = TRUE)

colnames(aces)[2] <- "player_id"

aces <- merge(aces, men)[1:4]

aces_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(aces = sum(w_ace),
            serves = sum(w_svpt))

aces_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(aces = sum(l_ace),
            serves = sum(l_svpt))

w_aces <- merge(x = aces_w, y = aces_l, by.x = c("year", "winner_id", "aces", "serves"), by.y = c("year", "loser_id", "aces", "serves"), all = TRUE)

colnames(w_aces)[2] <- "player_id"

w_aces <- merge(w_aces, women)[1:4]

aces <- rbind(aces, w_aces)

na_presence <- aggregate(aces ~ player_id, data=aces, function(x) {sum(is.na(x))}, na.action = NULL)

data_presence <- aces %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n())

na_in_data <- merge(na_presence, data_presence)

absent_players <- c()

for (i in 1:nrow(na_in_data)) {
  if (na_in_data$total_matches[i]/2 >= na_in_data$aces[i]) {
    absent_players <- c(absent_players, i)
  }
}

aces <- aces[-absent_players, ]

write.csv(aces, "aces.csv")

# Service Points Won
serve_point_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_serves_won = sum(w_1stWon),
            second_serves_won = sum(w_2ndWon),
            serves = sum(w_svpt))

serve_point_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_serves_won = sum(l_1stWon),
            second_serves_won = sum(l_2ndWon),
            serves = sum(l_svpt))

serve_point <- merge(x = serve_point_w, y = serve_point_l, by.x = c("year", "winner_id", "first_serves_won", "second_serves_won", "serves"), by.y = c("year", "loser_id", "first_serves_won", "second_serves_won", "serves"), all = TRUE)

colnames(serve_point)[2] <- "player_id"

serve_point$serves_won <- serve_point$first_serves_won + serve_point$second_serves_won

serve_point$first_serves_won <- NULL
serve_point$second_serves_won <- NULL

serve_point <- merge(serve_point, men)[1:4]

serve_point_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_serves_won = sum(w_1stWon),
            second_serves_won = sum(w_2ndWon),
            serves = sum(w_svpt))

serve_point_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_serves_won = sum(l_1stWon),
            second_serves_won = sum(l_2ndWon),
            serves = sum(l_svpt))

w_serve_point <- merge(x = serve_point_w, y = serve_point_l, by.x = c("year", "winner_id", "first_serves_won", "second_serves_won", "serves"), by.y = c("year", "loser_id", "first_serves_won", "second_serves_won", "serves"), all = TRUE)

colnames(w_serve_point)[2] <- "player_id"

w_serve_point$serves_won <- w_serve_point$first_serves_won + w_serve_point$second_serves_won

w_serve_point$first_serves_won <- NULL
w_serve_point$second_serves_won <- NULL

w_serve_point <- merge(w_serve_point, women)[1:4]

serve_point <- rbind(serve_point, w_serve_point)

na_presence <- aggregate(serves ~ player_id, data = serve_point, function(x) {sum(is.na(x))}, na.action = NULL)

data_presence <- serve_point %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n())

na_in_data <- merge(na_presence, data_presence)

absent_players <- c()

for (i in 1:nrow(na_in_data)) {
  if (na_in_data$total_matches[i]/2 >= na_in_data$serves[i]) {
    absent_players <- c(absent_players, i)
  }
}

serve_point <- serve_point[-absent_players, ]

write.csv(serve_point, "serves_won.csv")

# First Serve Returns
first_returns_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_return = sum(l_1stIn),
            first_lost = sum(l_1stWon))

first_returns_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_return = sum(w_1stIn),
            first_lost = sum(w_1stWon))

first_returns <- merge(x = first_returns_w, y = first_returns_l, by.x = c("year", "winner_id", "first_return", "first_lost"), by.y = c("year", "loser_id", "first_return", "first_lost"), all = TRUE)

colnames(first_returns)[2] <- "player_id"

first_returns$first_won <- first_returns$first_return - first_returns$first_lost

first_returns$first_lost <- NULL

first_returns <- merge(first_returns, men)[1:4]

first_returns_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_return = sum(l_1stIn),
            first_lost = sum(l_1stWon))

first_returns_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_return = sum(w_1stIn),
            first_lost = sum(w_1stWon))

w_first_returns <- merge(x = first_returns_w, y = first_returns_l, by.x = c("year", "winner_id", "first_return", "first_lost"), by.y = c("year", "loser_id", "first_return", "first_lost"), all = TRUE)

colnames(w_first_returns)[2] <- "player_id"

w_first_returns$first_won <- w_first_returns$first_return - w_first_returns$first_lost

w_first_returns$first_lost <- NULL

w_first_returns <- merge(w_first_returns, women)[1:4]

first_returns <- rbind(first_returns, w_first_returns)

na_presence <- aggregate(first_return ~ player_id, data = first_returns, function(x) {sum(is.na(x))}, na.action = NULL)

data_presence <- first_returns %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n())

na_in_data <- merge(na_presence, data_presence)

absent_players <- c()

for (i in 1:nrow(na_in_data)) {
  if (na_in_data$total_matches[i]/2 >= na_in_data$first_return[i]) {
    absent_players <- c(absent_players, i)
  }
}

first_returns <- first_returns[-absent_players, ]

write.csv(first_returns, "first_returns.csv")

# Second Serve Returns
second_returns_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_return = sum(l_1stIn),
            second_lost = sum(l_2ndWon),
            return_point = sum(l_svpt),
            double_fault = sum(l_df))

second_returns_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_return = sum(w_1stIn),
            second_lost = sum(w_2ndWon),
            return_point = sum(w_svpt),
            double_fault = sum(w_df))

second_returns <- merge(x = second_returns_w, y = second_returns_l, by.x = c("year", "winner_id", "first_return", "second_lost", "return_point", "double_fault"), by.y = c("year", "loser_id", "first_return", "second_lost", "return_point", "double_fault"), all = TRUE)

colnames(second_returns)[2] <- "player_id"

second_returns$second_return <- second_returns$return_point - second_returns$first_return - second_returns$double_fault

second_returns$second_won <- second_returns$second_return - second_returns$second_lost

second_returns$return_point <- NULL
second_returns$double_fault <- NULL
second_returns$first_return <- NULL
second_returns$second_lost <- NULL

second_returns <- merge(second_returns, men)[1:4]

second_returns_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(first_return = sum(l_1stIn),
            second_lost = sum(l_2ndWon),
            return_point = sum(l_svpt),
            double_fault = sum(l_df))

second_returns_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(first_return = sum(w_1stIn),
            second_lost = sum(w_2ndWon),
            return_point = sum(w_svpt),
            double_fault = sum(w_df))

w_second_returns <- merge(x = second_returns_w, y = second_returns_l, by.x = c("year", "winner_id", "first_return", "second_lost", "return_point", "double_fault"), by.y = c("year", "loser_id", "first_return", "second_lost", "return_point", "double_fault"), all = TRUE)

colnames(w_second_returns)[2] <- "player_id"

w_second_returns$second_return <- w_second_returns$return_point - w_second_returns$first_return - w_second_returns$double_fault

w_second_returns$second_won <- w_second_returns$second_return - w_second_returns$second_lost

w_second_returns$return_point <- NULL
w_second_returns$double_fault <- NULL
w_second_returns$first_return <- NULL
w_second_returns$second_lost <- NULL

w_second_returns <- merge(w_second_returns, women)[1:4]

second_returns <- rbind(second_returns, w_second_returns)

na_presence <- aggregate(second_return ~ player_id, data = second_returns, function(x) {sum(is.na(x))}, na.action = NULL)

data_presence <- second_returns %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n())

na_in_data <- merge(na_presence, data_presence)

absent_players <- c()

for (i in 1:nrow(na_in_data)) {
  if (na_in_data$total_matches[i]/2 >= na_in_data$second_return[i]) {
    absent_players <- c(absent_players, i)
  }
}

second_returns <- second_returns[-absent_players, ]

write.csv(second_returns, "second_returns.csv")

# BP Conversion
bp_convert_w <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(bp = sum(l_bpFaced),
            bp_saved = sum(l_bpSaved))

bp_convert_l <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(bp = sum(w_bpFaced),
            bp_saved = sum(w_bpSaved))

bp_convert <- merge(x = bp_convert_w, y = bp_convert_l, by.x = c("year", "winner_id", "bp", "bp_saved"), by.y = c("year", "loser_id", "bp", "bp_saved"), all = TRUE)

colnames(bp_convert)[2] <- "player_id"

bp_convert$bp_converted <- bp_convert$bp - bp_convert$bp_saved

bp_convert$bp_saved <- NULL

bp_convert <- merge(bp_convert, men)[1:4]

bp_convert_w <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(bp = sum(l_bpFaced),
            bp_saved = sum(l_bpSaved))

bp_convert_l <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(bp = sum(w_bpFaced),
            bp_saved = sum(w_bpSaved))

w_bp_convert <- merge(x = bp_convert_w, y = bp_convert_l, by.x = c("year", "winner_id", "bp", "bp_saved"), by.y = c("year", "loser_id", "bp", "bp_saved"), all = TRUE)

colnames(w_bp_convert)[2] <- "player_id"

w_bp_convert$bp_converted <- w_bp_convert$bp - w_bp_convert$bp_saved

w_bp_convert$bp_saved <- NULL

w_bp_convert <- merge(w_bp_convert, women)[1:4]

bp_convert <- rbind(bp_convert, w_bp_convert)

na_presence <- aggregate(bp ~ player_id, data = bp_convert, function(x) {sum(is.na(x))}, na.action = NULL)

data_presence <- bp_convert %>% 
  group_by(player_id) %>% 
  summarise(total_matches = n())

na_in_data <- merge(na_presence, data_presence)

absent_players <- c()

for (i in 1:nrow(na_in_data)) {
  if (na_in_data$total_matches[i]/2 >= na_in_data$bp[i]) {
    absent_players <- c(absent_players, i)
  }
}

bp_convert <- bp_convert[-absent_players, ]

write.csv(bp_convert, "bp_convert.csv")

