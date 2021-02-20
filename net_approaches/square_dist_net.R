

b1 <- belief.mat[1:10,]
n <- nrow(b1)
bdist <- matrix(NA, n, n)
for (i in 1:n) {
  for (j in 1:n) {
    if (i < j) {
      bdist[i,j] <- bdist[j,i] <- ab_diff(belief.mat[i,], belief.mat[j,])
    }
  }
}

bg <- graph_from_adjacency_matrix(bdist^2, mode = "undirected", weighted = TRUE, diag = FALSE)
cbg <- cluster_fast_greedy(bg, weights = E(bg)$weight)
plot(bg, layout = layout_with_fr(bg, weights = E(bg)$weight),
     vertex.color=cbg$membership)
bdist


# Average squared difference between people on each issue
v1.dist <- matrix(NA, nrow = nrow(belief.mat), ncol = nrow(belief.mat))
mean.dist <- numeric(length = ncol(belief.mat))
sd.dist <- numeric(length = ncol(belief.mat))

for (k in 1:ncol(belief.mat)) {
  for (i in 1:length(belief.mat[,k])) {
    for (j in 1:length(belief.mat[,k])) {
      if (i < j) {
        v1.dist[i,j] <- as.numeric((belief.mat[i,k] - belief.mat[j,k])^2)
      }
    }
  }
  mean.dist[k] <- mean(v1.dist, na.rm = TRUE)
  sd.dist[k] <- sd(v1.dist, na.rm = TRUE)
}
ij

ab_diff <- function(vec1, vec2) {
  val <- sum((vec1 - vec2)^2)/length(vec1)
  return(val)
}

std.diff <- function(vec1, vec2, means, sd) {
  return(sum(((vec1 - vec2)^2 - mean)/sd))
}

belief.mat <- as.matrix(b2.1[,2:20])
n <- nrow(belief.mat)
dist <- matrix(NA, n, n)
for (i in 1:n) {
  for (j in 1:n) {
    if (i < j) {
      dist[i,j] <- dist[j,i] <- std.diff(belief.mat[i,], belief.mat[j,], mean.dist, sd.dist)
    }
  }
}


d <- (-1 * dist)
d <- ifelse(d < 0, 0, d)
d <- d/max(d, na.rm = TRUE)

g.d <- graph_from_adjacency_matrix(d, mode = "undirected", diag = FALSE)

cluster_fast_greedy(g.d)

