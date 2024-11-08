for(i in 1:100) {
  print("Hello world!")
  print(i*i)
  print("river")
  print(1+2)
}

demo <- seq (1,100,by=2) # sequence  1, 3, ..., 99
n<-length(demo) #  size of the foo sequence
demo.squared = NULL #  empty object

for (i in 1:n) { # our counter
  demo.squared[i] = demo[i]^2 # the task
}

demo.df<-data.frame(demo,demo.squared) 
plot (demo.df$demo~demo.df$demo.squared)




grow <- function (growth.rate) { # argument "growth.rate" of function "grow" 
  num_gen<-10
  generation<-1:num_gen
  N <- rep (0,num_gen)
  N[1] <- 1
  for (t in 2:num_gen) { 
    # not the use growth.rate argument and t-1  this time
    N[t]=growth.rate*N[t-1] 
  }
  plot(N~generation,type='b', main=paste("Rate =", growth.rate)) 
}


## Practice 6.1

grow2 <- function (growth.rate, number.generation) {
  num_gen<-number.generation
  generation<-1:num_gen
  N <- rep (0,num_gen)
  N[1] <- 1
  for (t in 2:num_gen) { 
    # not the use growth.rate argument and t-1  this time
    N[t]=growth.rate*N[t-1] 
  }
  plot(N~generation,type='b', main=paste("Rate =", growth.rate))
}


grow2(2, 10)


## Practice 6.2

log.grow <- function(growth.rate) {
  num_gen <- 50
  generation <- 1:num_gen
  N <- rep (0,num_gen)
  N[1] <- 10 
  for (i in 1:(num_gen-1)) {
    N[i+1] = N[i] + (growth.rate*N[i]*((100-N[i])/100))
  }
  plot(N~generation,type='b', main=paste("Rate =", growth.rate)) 
}

for (i in 0.5:3){
  log.grow(i) 
}
  
log.grow(0.5)





install.packages("gganimate")
install.packages("animation")
library(animation)
library(ggplot2)
library(gganimate)
library(dplyr)



function6.2 <- function(growth.rate) {
  num_gen <- 50
  generation <- 1:num_gen
  N <- rep(0, num_gen)
  N[1] <- 10
  for (t in 1:(num_gen-1)) {
    N[t+1] = N[t] + (growth.rate*N[t]*((100-N[t])/100))
  }
  plot(N~generation,type='b', main=paste("Rate =", growth.rate)) 
}
  

saveGIF({
  for (t in seq(0.5, 3, by = 0.1)){
    function6.2(t)
  }}, interval = 0.1)




# WRONG CODE
# ADVICES: START FROM SCRATCH, 
# JUST GET INSPIRATION FROM MY CODE, WORK STEP BY STEP
grow<-function(start_1){
  num_gen<=30
  N1 <- rep(0,10)
  N2 <- rep(0,10)
  generation<-rep(1,num_gen)
  growth.rate<-1.2
  K1<-100
  K2<-120
  a12<-0.8
  a21<-0.8
  N1[1]<-0
  N2[1]<-start_2
  for (i in 2:3)  {
    N1[i] = N1[i-1] + (3.2* N1[i-1] * ((K1-N1[i-1]-(a12*N2[i-1]))/K1))
    N2[i] = N2[i] + (growth.rate * N2[i-1] * ((K2-N2[i-1]-(a21*N1[i-1]))*K2))
    generation[1]=1
    print (N1[i])
  }
if (N1[1]>2){
  plot(N1~generation,typ="b",ylim=c(0,min(c(K1,K2))),ylab="N")
}  else {
  plot(N1~generation,typ="n",ylim=c(0,min(c(K1,K2))),ylab="N")
}
print(N2[1])
if (N2[1]>0){
  lines(N2~generation,typ="b",col=2)}
  }
  
  par(mar=c(9,4,1,1),mfrow=c(5,1),las=1)
  
  grow(1,0)
  text(4,110,"Species 1 alone")
  
  grow(0,1)
  text(4,110,"Species 2 alone")
  
  grow(1,2)
  text(6,110,"Both Species competing")






















