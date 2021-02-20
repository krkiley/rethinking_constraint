



sim.mat <- matrix(runif(2546*19, 0, 1), nrow = 2546, ncol = 19)

n <- nrow(sim.mat)
sim.dist <- matrix(NA, n, n)
for (i in 1:n) {
  for (j in 1:n) {
    if (i < j) {
      sim.dist[i,j] <- sim.dist[j,i] <- as.numeric(ab_diff(sim.mat[i,], sim.mat[j,]))
    }
  }
}


sims <- sim.dist %>% as.data.frame() %>%
  gather() %>% mutate(class = "sim")

obs <- dist %>% as.data.frame() %>%
  gather() %>% mutate(class = "obs")

bind_rows(sims, obs) %>%
  ggplot(aes(x = value, fill = class)) + 
  geom_histogram(position = "identity", alpha = .3)
