library(tidyr)
# Win Loss Summary
results$tourney_date <- as.Date(as.character(results$tourney_date), format = "%Y%m%d")
w_results$tourney_date <- as.Date(as.character(w_results$tourney_date), format = "%Y%m%d")

results$year <- format(results$tourney_date, "%Y")

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

win_loss_summary <- merge(win_loss_summary, men)[1:4]

w_results$year <- format(w_results$tourney_date, "%Y")

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

w_win_loss_summary <- merge(w_win_loss_summary, women)[1:4]

win_loss_summary <- rbind(win_loss_summary, w_win_loss_summary)

write.csv(win_loss_summary, "win_loss.csv")

# Double faults
avg_df <- results %>% 
  group_by(year, winner_id) %>% 
  summarise(double_fault <- mean(w_df))

avg_df_loss <- results %>% 
  group_by(year, loser_id) %>% 
  summarise(double_fault <- mean(w_df))

win_loss_summary <- merge(x = yearly_wins, y = yearly_losses, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

win_loss_summary[is.na(win_loss_summary)] <- 0

win_loss_summary$total_matches <- win_loss_summary$count.x + win_loss_summary$count.y

win_loss_summary$count.y <- NULL

colnames(win_loss_summary) <- c("year", "player_id", "matches_won", "total_matches")

win_loss_summary <- merge(win_loss_summary, men)[1:4]

w_results$year <- format(w_results$tourney_date, "%Y")

yearly_wins <- w_results %>% 
  group_by(year, winner_id) %>% 
  summarise(count = n())

yearly_losses <- w_results %>% 
  group_by(year, loser_id) %>% 
  summarise(count = n())

w_win_loss_summary <- merge(x = yearly_wins, y = yearly_losses, by.x = c("year", "winner_id"), by.y = c("year", "loser_id"), all = TRUE)

w_win_loss_summary[is.na(w_win_loss_summary)] <- 0

w_win_loss_summary$total_matches <- w_win_loss_summary$count.x + w_win_loss_summary$count.y

w_win_loss_summary$count.y <- NULL

colnames(w_win_loss_summary) <- c("year", "player_id", "matches_won", "total_matches")

w_win_loss_summary <- merge(w_win_loss_summary, women)[1:4]

win_loss_summary <- rbind(win_loss_summary, w_win_loss_summary)

write.csv(win_loss_summary, "win_loss.csv")