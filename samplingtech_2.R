# Number of subgroups
G <- 4

# Population size of each subgroup
Ng <- c(400, 300, 200, 100)
N <- sum(Ng)

# Subgroup weights
Wg <- Ng / N

# Standard deviation in each subgroup
Sg <- c(12, 18, 10, 20)

# Precision (sampling bias)
z <- 1.96   # 95% confidence
d <- 2      # allowable error


# Cost per observation
cg <- c(5, 8, 6, 10)

# Time per observation
tg <- c(2, 3, 1.5, 4)

# Total cost and time constraints
C <- 6000
T <- 2500


n_cost <- (z^2 * (sum(Wg * Sg * sqrt(cg)))^2) / (d^2 * C)
n_cost <- ceiling(n_cost)
n_cost



ng_cost <- n_cost * (Wg * Sg / sqrt(cg)) /
  sum(Wg * Sg / sqrt(cg))
ng_cost




n_time <- (z^2 * (sum(Wg * Sg * sqrt(tg)))^2) / (d^2 * T)
n_time <- ceiling(n_time)
n_time



ng_time <- n_time * (Wg * Sg / sqrt(tg)) /
  sum(Wg * Sg / sqrt(tg))
ng_time




design <- data.frame(
  Subgroup = paste("G", 1:G, sep = ""),
  Population = Ng,
  SD = Sg,
  Cost = cg,
  Time = tg,
  Sample_Cost_Based = round(ng_cost, 2),
  Sample_Time_Based = round(ng_time, 2)
)

design



barplot(
  rbind(ng_cost, ng_time),
  beside = TRUE,
  names.arg = paste("G", 1:G, sep = ""),
  legend.text = c("Cost Optimized", "Time Optimized"),
  main = "Experimental Design: Sample Allocation Across Subgroups",
  xlab = "Subgroups",
  ylab = "Sample Size"
)




pie(
  ng_cost,
  labels = paste("G", 1:G, sep = ""),
  main = "Cost Optimized Sample Distribution"
)


