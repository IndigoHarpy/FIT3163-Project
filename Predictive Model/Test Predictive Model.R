results <- read.csv("mens_singles.csv")
men <- read.csv("current_men.csv", row.names = 1)
w_results <- read.csv("womens_singles.csv")
women <- read.csv("current_women.csv", row.names = 1)

men$ELO <- seq(1000, 2000, length.out = 2510)
men$rank <- seq(2510, 1, length.out = 2510)

women$ELO <- seq(1000, 2000, length.out = 2045)
women$rank <- seq(2045, 1, length.out = 2045)

player_1 <- "Alexander Zverev"
player_2 <- "Alex De Minaur"

predict_result(player_1, player_2, "Hard", "M", "R16", 90, results, men)
