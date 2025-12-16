# Number of strata
L <- 3

# Population size of each stratum
Nh <- c(500, 300, 200)
N <- sum(Nh)

# Stratum weights
Wh <- Nh / N

# Standard deviation in each stratum
Sh <- c(10, 20, 15)

# Precision parameters
z <- 1.96     # 95% confidence
d <- 2        # allowable error


n_prop <- (z^2 * sum(Wh * Sh^2)) / d^2
n_prop <- ceiling(n_prop)
n_prop



nh_prop <- n_prop * Wh
nh_prop




n_neyman <- (z^2 * (sum(Wh * Sh))^2) / d^2
n_neyman <- ceiling(n_neyman)
n_neyman




nh_neyman <- n_neyman * (Wh * Sh) / sum(Wh * Sh)
nh_neyman




# Cost per unit in each stratum
ch <- c(5, 10, 8)

# Total cost available
C <- 5000



n_opt_cost <- (z^2 * (sum(Wh * Sh * sqrt(ch)))^2) / (d^2 * C)
n_opt_cost <- ceiling(n_opt_cost)
n_opt_cost



nh_opt_cost <- n_opt_cost * (Wh * Sh / sqrt(ch)) /
  sum(Wh * Sh / sqrt(ch))
nh_opt_cost



# Time per unit in each stratum
th <- c(2, 3, 4)

# Total time available
T <- 2000




n_opt_time <- (z^2 * (sum(Wh * Sh * sqrt(th)))^2) / (d^2 * T)
n_opt_time <- ceiling(n_opt_time)
n_opt_time


nh_opt_time <- n_opt_time * (Wh * Sh / sqrt(th)) /
  sum(Wh * Sh / sqrt(th))
nh_opt_time



allocation <- data.frame(
  Stratum = 1:L,
  Proportional = round(nh_prop, 2),
  Neyman = round(nh_neyman, 2),
  Optimized_Cost = round(nh_opt_cost, 2),
  Optimized_Time = round(nh_opt_time, 2)
)

allocation




cat("Optimized allocation gives minimum variance by considering both stratum variability and survey cost/time.")


