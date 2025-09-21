results <- read.csv("mens_singles.csv")
men <- read.csv("current_men.csv")
w_results <- read.csv("womens_singles.csv")
women <- read.csv("current_women.csv")

player_1 <- "Alexander Zverev"
player_2 <- "Alex De Minaur"

predict_result(player_1, player_2, "Hard", "M", "R16", 90, results, men)

