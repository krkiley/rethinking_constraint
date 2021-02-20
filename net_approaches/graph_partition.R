

#Seven beliefs



#Two groups


#Group 1 constrains all but one of them


#Group 2 constrains 4 of them

#More constrained belief system
xn <- 300
x <- matrix(NA, nrow=xn, ncol = 7)
x[,1] <- sample(c(1, 2, 3), xn, prob = c(1, 0, 0), replace = TRUE)
x[,2] <- sample(c(1, 2, 3), xn, prob = c(1, 0, 0), replace = TRUE)
x[,3] <- sample(c(1, 2, 3), xn, prob = c(1, 0, 0), replace = TRUE)
x[,4] <- sample(c(1, 2, 3), xn, prob = c(8, 1, 1), replace = TRUE)
x[,5] <- sample(c(1, 2, 3), xn, prob = c(8, 1, 1), replace = TRUE)
x[,6] <- sample(c(1, 2, 3), xn, prob = c(8, 1, 1), replace = TRUE)
x[,7] <- sample(c(1, 2, 3), xn, prob = c(1, 1, 1), replace = TRUE)

#Fewer things constrained
yn <- 400
y <- matrix(NA, nrow=yn, ncol = 7)
y[,1] <- sample(c(1, 2, 3), yn, prob = c(8, 1, 1), replace = TRUE)
y[,2] <- sample(c(1, 2, 3), yn, prob = c(8, 1, 1), replace = TRUE)
y[,3] <- sample(c(1, 2, 3), yn, prob = c(8, 1, 1), replace = TRUE)
y[,4] <- sample(c(1, 2, 3), yn, prob = c(1, 1, 1), replace = TRUE)
y[,5] <- sample(c(1, 2, 3), yn, prob = c(1, 1, 1), replace = TRUE)
y[,6] <- sample(c(1, 2, 3), yn, prob = c(1, 1, 1), replace = TRUE)
y[,7] <- sample(c(1, 2, 3), yn, prob = c(1, 1, 1), replace = TRUE)

#Different constraints
zn <- 100
z <- matrix(NA, nrow=zn, ncol = 7)
z[,1] <- sample(c(1, 2, 3), zn, prob = c(1,1,8), replace = TRUE)
z[,2] <- sample(c(1, 2, 3), zn, prob = c(1,1,8), replace = TRUE)
z[,3] <- sample(c(1, 2, 3), zn, prob = c(1,1,8), replace = TRUE)
z[,4] <- sample(c(1, 2, 3), zn, prob = c(1, 1, 1), replace = TRUE)
z[,5] <- sample(c(1, 2, 3), zn, prob = c(1, 1, 1), replace = TRUE)
z[,6] <- sample(c(1, 2, 3), zn, prob = c(1, 1, 1), replace = TRUE)
z[,7] <- sample(c(1, 2, 3), zn, prob = c(1,1,8), replace = TRUE)

test.t1 <- data.frame(rbind(x,y,z))

#test.t2 <- rbind(x,y,z)

n <- nrow(test.t1)
dist <- matrix(NA, n, n)
for (i in 1:n) {
  for (j in 1:n) {
    if (i < j) {
      dist[i,j] <- dist[j,i] <- as.numeric(ab_diff(test.t1[i,1], test.t1[j,1]))
    }
  }
}

mat <- ifelse(dist < 0.75, 0, dist^4)
g <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE,
                                 diag = FALSE)
c1 <- cluster_fast_greedy(g, weights = E(g)$weight)

plot(g, vertex.size = 2, vertex.label=NA, 
     layout = layout_with_fr(g, weight = E(g)$weight), 
     vertex.color=membership(c1),
     #vertex.label.color = membership(c1),
     edge.width = E(g)$weight/3)


g2 <- graph_from_adjacency_matrix(mat2, mode = "undirected", weighted = TRUE,
                                  diag = FALSE)

#5 = 9521.794
#3 = 9461.123
#4 = 9538.275
poLCA(cbind(X1, X2, X3, X4, X5, X6, X7)~1, data = test.t1,
      nclass = 3)
