pop.growth <- function(a12, a21) {
  num_gen <- 30
  generation <- 1:num_gen
  N1 <- rep(0, num_gen)
  N2 <- rep(0, num_gen)
  growth.rate <- 1.2 
  K1 <- 100
  K2 <- 120
  N1[1] <- 10
  N2[1] <- 20
  
  for (t in 1:(num_gen-1)) {
    N1[t+1] = N1[1] + (growth.rate*N1[t]*((K1-N1[t]-a12*N2[t])/K1))
    N2[t+1] = N2[1] + (growth.rate*N2[t]*((K2-N2[t]-a21*N1[t])/K2))
  }
  
  if (N1[1]>2) {
    plot(N1~generation, typ = "b", ylim = c(0, max(c(K1,K2))), ylab = "N")
  }  else {
    plot(N1~generation, typ = "b", ylim = c(0, max(c(K1,K2))), ylab = "N")
  }
  
  print(N2[1])
  if (N2[1]>0){
    lines(N2~generation,typ="b",col=2)}
}

par(mar = c(4, 4, 2, 1), mfrow = c(3, 1), las = 1)

pop.growth(1,0)
text(4,110,"Species 1 alone")

pop.growth(0,1)
text(4,110,"Species 2 alone")

pop.growth(1,2)
text(6,110,"Both Species competing")












pop.growth <- function(a12, a21) {
  num_gen <- 30
  generation <- 1:num_gen
  N1 <- rep(0, num_gen)
  N2 <- rep(0, num_gen)
  growth.rate <- 1.2 
  K1 <- 100
  K2 <- 120
  N1[1] <- 2
  N2[1] <- 6
  
  for (t in 1:(num_gen - 1)) {
    N1[t + 1] <- N1[t] + (growth.rate * N1[t] * ((K1 - N1[t] - a12 * N2[t]) / K1))
    N2[t + 1] <- N2[t] + (growth.rate * N2[t] * ((K2 - N2[t] - a21 * N1[t]) / K2))
  }
  
  # Plot N1
  plot(N1 ~ generation, type = "b", ylim = c(0, max(K1, K2)), ylab = "N", main = "Population Growth")
  
  # Plot N2 on the same graph
  if (all(N2 > 0)) {
    lines(N2 ~ generation, type = "b", col = 2)
  }
  
  print(N2[1])  # Optional debug output
}

# Plot setup
par(mar = c(4, 4, 2, 1), mfrow = c(3, 1), las = 1)

pop.growth(1, 0)
text(4, 110, "Species 1 alone")

pop.growth(0, 1)
text(4, 110, "Species 2 alone")

pop.growth(1, 2)
text(6, 110, "Both Species competing")






