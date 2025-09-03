#################################
# Monty Hall Problem Simulation
#################################

set.seed(6325)  # for reproducibility

monty_hall_trial <- function(strategy = c("stay", "switch")) {
  strategy <- match.arg(strategy)
  doors <- 1:3
  car   <- base::sample(doors, 1)
  pick1 <- base::sample(doors, 1)
  
  # Host logic that can NEVER reveal the car
  if (pick1 == car) {
    # You picked the car; host chooses randomly among the two goats
    host_reveal <- base::sample(base::setdiff(doors, car), 1)
  } else {
    # You picked a goat; only one eligible goat door remains
    host_reveal <- base::setdiff(doors, c(pick1, car))
  }
  stopifnot(host_reveal %in% base::setdiff(doors, car))  # sanity check
  
  remaining <- base::setdiff(doors, c(pick1, host_reveal))
  final_pick <- if (strategy == "stay") pick1 else remaining
  as.integer(final_pick == car)
}

simulate_monty <- function(n = 100000, strategy = c("stay", "switch")) {
  strategy <- match.arg(strategy)
  wins <- sum(unlist(replicate(n, monty_hall_trial(strategy), simplify = FALSE), use.names = FALSE))
  data.frame(
    strategy = strategy, n = n, wins = wins,
    losses = n - wins, win_rate = wins / n
  )
}

# ---- Run and compare stay vs switch ----
N <- 100000
strategies <- c("stay", "switch")
results <- do.call(rbind, lapply(strategies, function(s) simulate_monty(N, s)))
print(results, row.names = FALSE)


# Visualize using a bar chart
barplot(height = results$win_rate,
        names.arg = results$strategy,
        ylim = c(0, 1),
        main = paste0("Monty Hall Simulation (N = ", N, ")"),
        ylab = "Win rate")
abline(h = c(1/3, 2/3), lty = c(2, 2))
text(x = 1:length(results$strategy),
     y = results$win_rate,
     labels = sprintf("%.3f", results$win_rate),
     pos = 3)
