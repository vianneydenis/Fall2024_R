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
    N1[t+1] = N1[1] + (growth.rate * N1[t] * ((K1 - N1[t] - a12 * N2[t]) / K1))
    N2[t+1] = N2[1] + (growth.rate * N2[t] * ((K2 - N2[t] - a21 * N1[t]) / K2))
  }
  
  if (N1[1]>2) {
    plot(N1~generation, typ = "b", ylim = c(0, max(c(K1,K2))), ylab = "N")
  }  else {
    plot(N2~generation, typ = "b", ylim = c(0, max(c(K1,K2))), ylab = "N")
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








pop.growth <- function(a12, a21) {
  num_gen <- 30
  generation <- 1:num_gen
  N1 <- rep(0, num_gen)
  N2 <- rep(0, num_gen)
  growth.rate <- 1.2 
  K1 <- 100
  K2 <- 120
  N1[1] <- 5
  N2[1] <- 5
  
  for (t in 1:(num_gen-1)) {
    N1[t+1] = N1[1] + (growth.rate*N1[t]*((K1-N1[t]-a12*N2[t])/K1))
    N2[t+1] = N2[1] + (growth.rate*N2[t]*((K2-N2[t]-a21*N1[t])/K2))
  }
  plot(N~generation, type = 'b', main = paste("Rate =", growth.rate))
}
pop.growth(1, 0)







# Function to model population growth based on competition coefficients
pop.growth <- function(a12, a21) {
  num_gen <- 30
  generation <- 1:num_gen
  N1 <- rep(0, num_gen)
  N2 <- rep(0, num_gen)
  growth.rate <- 1.2  # Common growth rate for both species
  K1 <- 100  # Carrying capacity for species 1
  K2 <- 120  # Carrying capacity for species 2
  
  # Initial population sizes
  N1[1] <- 10  # Starting value for species 1
  N2[1] <- 20  # Starting value for species 2
  
  # Loop to update population sizes over generations
  for (t in 1:(num_gen - 1)) {
    N1[t+1] <- N1[t] + (growth.rate * N1[t] * ((K1 - N1[t] - a12 * N2[t]) / K1))
    N2[t+1] <- N2[t] + (growth.rate * N2[t] * ((K2 - N2[t] - a21 * N1[t]) / K2))
    
    # Ensure populations do not fall below 0
    N1[t+1] <- max(0, N1[t+1])
    N2[t+1] <- max(0, N2[t+1])
  }
  
  # Plot the results
  plot(N1 ~ generation, type = "b", ylim = c(0, max(K1, K2)), ylab = "N", xlab = "Generation", col = "black", main = "")
  lines(N2 ~ generation, type = "b", col = "red")
}

# Set up the plotting space for three graphs in one column
par(mar = c(4, 4, 2, 1), mfrow = c(3, 1), las = 1)

# Run the function for different scenarios
pop.growth(1, 0)
text(4, 110, "Species 1 alone")

pop.growth(0, 1)
text(4, 110, "Species 2 alone")

pop.growth(1, 2)
text(6, 110, "Both Species competing")





# Function to model population growth based on competition coefficients
pop.growth <- function(a12, a21) {
  num_gen <- 30
  generation <- 1:num_gen
  N1 <- rep(0, num_gen)
  N2 <- rep(0, num_gen)
  growth.rate <- 1.2  # Common growth rate for both species
  K1 <- 100  # Carrying capacity for species 1
  K2 <- 120  # Carrying capacity for species 2
  
  # Initial population sizes
  N1[1] <- 10  # Starting value for species 1
  N2[1] <- 20  # Starting value for species 2
  
  # Loop to update population sizes over generations
  for (t in 1:(num_gen - 1)) {
    N1[t+1] <- N1[t] + (growth.rate * N1[t] * ((K1 - N1[t] - a12 * N2[t]) / K1))
    N2[t+1] <- N2[t] + (growth.rate * N2[t] * ((K2 - N2[t] - a21 * N1[t]) / K2))
    
    # Ensure populations do not fall below 0
    N1[t+1] <- max(0, N1[t+1])
    N2[t+1] <- max(0, N2[t+1])
  }
  
  # Plot the results
  plot(N1 ~ generation, type = "b", ylim = c(0, max(K1, K2)), ylab = "N", xlab = "Generation", col = "black", main = "")
  lines(N2 ~ generation, type = "b", col = "red")
}

# Set up the plotting space for three graphs in one column
par(mar = c(4, 4, 2, 1), mfrow = c(3, 1), las = 1)

# Run the function for different scenarios
pop.growth(1, 0)
text(4, 110, "Species 1 alone")

pop.growth(0, 1)
text(4, 110, "Species 2 alone")

pop.growth(1, 2)
text(6, 110, "Both Species competing")







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
  plot(N1~generation, typ = "b", ylim = c(0, max(c(K1,K2))), ylab = "N")
  lines(N2~generation, typ = "b", col = 2)
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
  N1[1] <- 5
  N2[1] <- 5
  
  for (t in 1:(num_gen-1)) {
    N1[t+1] = N1[t] + (growth.rate * N1[t] * ((K1 - N1[t] - a12 * N2[t]) / K1))
    N2[t+1] = N2[t] + (growth.rate * N2[t] * ((K2 - N2[t] - a21 * N1[t]) / K2))
  }
  
  # Plot for Species 1 alone
  plot(generation, N1, type = "o", col = "blue", ylim = c(0, max(N1, K1)), 
       xlab = "Generation", ylab = "Population Size", main = "Population Growth of Species 1 Alone")
  
  # Plot for Species 2 alone
  plot(generation, N2, type = "o", col = "red", ylim = c(0, max(N2, K2)), 
       xlab = "Generation", ylab = "Population Size", main = "Population Growth of Species 2 Alone")
  
  # Plot for both species in competition
  plot(generation, N1, type = "o", col = "blue", ylim = c(0, max(N1, N2, K1, K2)), 
       xlab = "Generation", ylab = "Population Size", main = "Competition Between Species 1 and 2")
  lines(generation, N2, type = "o", col = "red")
  legend("topright", legend = c("Species 1", "Species 2"), col = c("blue", "red"), lty = 1, pch = 1)
}

# Run the function with interaction coefficients for competition
pop.growth(a12 = 0.5, a21 = 1)  # Modify coefficients as needed for competition or no competition









#---------


pop.growth <- function(a12, a21) {
  num_gen <- 30
  generation <- 1:num_gen
  N1 <- rep(0, num_gen)
  N2 <- rep(0, num_gen)
  growth.rate <- 1.2 
  K1 <- 100
  K2 <- 120
  N1[1] <- 2
  N2[1] <- 2
  
  
  a12 = 0
  a21 = 0
 
   
  
  for (t in 1:(num_gen-1)) {
    N1[t+1] = N1[t] + growth.rate * N1[t] * (K1 - N1[t] - a12 * N2[t]) / K1
    N2[t+1] = N2[t] + growth.rate * N2[t] * (K2 - N2[t] - a21 * N1[t]) / K2
  }
  
  # Plot for Species 1 alone
  plot(generation, N1, type = "b", col = "blue", ylim = c(0, max(K1, K2)), 
       xlab = "Generation", ylab = "N", main = "Population Growth of Species 1 Alone")
  
  plot(generation, N2, type = "b", col = "blue", ylim = c(0, max(K1, K2)), 
       xlab = "Generation", ylab = "N", main = "Population Growth of Species 1 Alone")
  
  
  
  
  # Plot for Species 2 alone
  plot(generation, N2, type = "o", col = "red", ylim = c(0, max(N2, K2)), 
       xlab = "Generation", ylab = "Population Size", main = "Population Growth of Species 2 Alone")
  
  # Plot for both species in competition
  plot(generation, N1, type = "o", col = "blue", ylim = c(0, max(N1, N2, K1, K2)), 
       xlab = "Generation", ylab = "Population Size", main = "Competition Between Species 1 and 2")
  lines(generation, N2, type = "o", col = "red")
  legend("topright", legend = c("Species 1", "Species 2"), col = c("blue", "red"), lty = 1, pch = 1)
}

# Run the function with interaction coefficients for competition
pop.growth(a12 = 0.5, a21 = 1)  # Modify coefficients as needed for competition or no competition


#-------------
grow<-function(start_1, start_2){
  num_gen <- 30
  generation <- 1:num_gen
  N1 <- rep(0, num_gen)
  N2 <- rep(0, num_gen)
  growth.rate <- 1.2 
  K1 <- 100
  K2 <- 120
  a12<-0.8
  a21<-0.8
  N1[1]<-start_1
  N2[1]<-start_2
  for (t in 1:(num_gen-1)) {
    N1[t+1] = N1[t] + growth.rate * N1[t] * (K1 - N1[t] - a12 * N2[t]) / K1
    N2[t+1] = N2[t] + growth.rate * N2[t] * (K2 - N2[t] - a21 * N1[t]) / K2
  }
if (N1[1]>0){
  plot(N1~generation,typ="b",ylim=c(0,max(c(K1,K2))),ylab="N")
}  else {
  plot(N1~generation,typ="n",ylim=c(0,max(c(K1,K2))),ylab="N")
}
if (N2[1]>0){
  lines(N2~generation,typ="b",col=2)}
}

par(mar = c(4, 4, 2, 1), mfrow = c(3, 1), las = 1)

  grow(1,0)
  text(4,110,"Species 1 alone")
  
  grow(0,1)
  text(4,110,"Species 2 alone")
  
  grow(1,1)
  text(6,110,"Both Species competing")
  