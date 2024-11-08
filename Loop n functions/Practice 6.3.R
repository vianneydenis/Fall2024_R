



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
  for (t in 1:(num_gen-1)) {
    N1[t+1] = N1[1] + (growth.rate*N1[t]*((K1-N1[t]-a12N2[t])/K1))
    N2[t+1] = N2[1] + (growth.rate*N2[t]*((K2-N2[t]-a21N1[t])/K2))
  }
  plot(N1~generation,type="b",ylim=c(0,min(c(K1,K2))),ylab="N") 
  lines(N2~generation,typ="b",col=2)
}
    
pop.growth(0.5, 1.0)
 




