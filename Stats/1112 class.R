### 11.12 Class Notes 
## Statistics - Initiation 

# Installing and loading packages 
install.packages("psych")
installed.packages("gridExtra")
install.packages("car")
install.packages("ggpubr")
install.packages("rstatix")
library (psych)
library (tidyverse)
library(gridExtra)
library(car)
library(ggpubr)
library(rstatix)
library(ggplot2)

getwd()
setwd("/Users/riversung/NTU/113-1 Fall 2024/Ocean5098 R/Rclass_Fall2024/Stats")

#----Loading Soft Coral dataset----
softies <- read.csv("sf_res.csv", sep = ",", header = T) # read csv file 
str(softies) # look at structure of file
softies$sample_no <- NULL # removes 'sample_no' column from softies data frame 
softies$species <- as.factor(softies$species) # converts 'species' column to factor 
softies$depth <- as.factor(softies$depth) # convert 'depth' column to factor 
str(softies)


#----Descriptive----
summary(softies)
psych::describe(softies[,-c(1:2)]) # generate descriptive stats excluding first 2 columns 
psych::describe (softies[,-c(1:2)]~species) # separated by species 
psych::describeBy (softies[,-c(1:2)], softies$species)


#----Counts & Proportions----
# One variable
prop.table (table(softies$species)) # results show proportion (relative frequency) of each species, summing to 1

# two variables
table(softies$depth, softies$species) 
prop.table (table(softies$depth, softies$species))

# three variables, with nicer formatting after adding a random variables
softies_2 <- softies # creates a copy 
softies_2$site <- rep(c("A", "B"), 10) # adding new column 'site', repeating 'A' and 'B'
ftable(softies_2$depth, softies_2$species, softies_2$site)


#----Aggregate & Apply----
mean(softies$ratio) # calculate the mean value of the 'ratio' column in the 'softies' data
lit <- softies$species == 'Litophyton sp.' # a filter to subset 'species' equals 'Litophyton sp.' 
mean(softies$ratio[lit]) # calculate the mean of lit 
aggregate(softies$ratio,list (species = softies$species), FUN = "median") 
# use 'aggregate' to calculate median of 'ratio', 'list' creates a grouping, "FUN = median" is function 
tapply(softies$ratio,softies$species, FUN = "median") # another method for same result 


#----Practice 7.1----
str(iris)
summary(iris)

plot1 <-ggplot(iris, aes(x=Species, y=Sepal.Length)) + 
  geom_boxplot()
plot2 <-ggplot(iris, aes(x=Species, y=Sepal.Width)) + 
  geom_boxplot() 
plot3 <-ggplot(iris, aes(x=Species, y=Petal.Length)) + 
  geom_boxplot() 
plot4 <-ggplot(iris, aes(x=Species, y=Petal.Width)) + 
  geom_boxplot() 

grid.arrange(plot1, plot2,plot3, plot4, ncol=2)
describeBy (iris, iris$Species)

iris %>% group_by(Species) %>% summarise(across(c(1:4), length))
aggregate(iris[,1:4],by=list(iris$Species), median)
tapply(iris$Sepal.Length , iris$Species, mean)



students<-read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/students.txt',header=T, sep="\t", dec='.')
fix(students)





#----Practice 7.4----
pearson.test <- function(x, y) {
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  }
  
  complete_cases <- complete.cases(x, y)
  x <- x[complete_cases]
  y <- y[complete_cases]
  
  n <- length(x)
  
  if (n < 3) {
    stop("Not enough complete observations to perform the test")
  }
  
  r <- sum((x - mean(x)) * (y - mean(y))) / (sqrt(sum((x - mean(x))^2)) * sqrt(sum((y - mean(y))^2)))
  t_stat <- r * sqrt((n - 2) / (1 - r^2))
  p_value <- 2 * pt(-abs(t_stat), df = n - 2)
  
  result <- list(
    correlation = r,
    t_statistic = t_stat,
    p_value = p_value
  )
  
  return(result)
}

pearson.test(3, 5) #????

rairuoho <- read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/rairuoho.txt', header = T, sep = "\t") 

res_3_4 <- pearson.test(rairuoho$length_day3, rairuoho$length_day4)
print("Correlation between length at day 3 and day 4:")
print(res_3_4)

res_3_8 <- pearson.test(rairuoho$length_day3, rairuoho$length_day8)
print("Correlation between length at day 3 and day 8:")
print(res_3_8)





#-----attempt 2----
getAnywhere("cor.test.default")
  #r <- cor(x, y)
  #df <- n - 2L
  #ESTIMATE <- c(cor = r)
  #PARAMETER <- c(df = df)
  #STATISTIC <- c(t = sqrt(df) * r/sqrt(1 - r^2))
?cor
?pt

pearson.test <- function(x, y) {
  if(length(x) != length(y)) { 
    stop("Vectors must have the same length") 
  }
  
  r <- cor(x, y) 
  n <- length(x)
  t_stat <- r * sqrt((n - 2) / (1 - r^2)) 
  df <- n - 2
  p_value <- (1 - pt(abs(t_stat), df)) * 2 
  
  return(list(correlation = r, 
              t_statistic = t_stat, 
              p_value = p_value))
}

results <- list()

rairuoho <- read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/rairuoho.txt', header = T, sep = "\t") 

unique_treatments <- unique(rairuoho$treatment)
for (treatment in unique_treatments) {
  subset_data <- rairuoho[rairuoho$treatment == treatment, ]
  
  result_day34 <- pearson.test(subset_data$day3, subset_data$day4)
  result_day38 <- pearson.test(subset_data$day3, subset_data$day8)
  
  results[[treatment]] <- list(
    day3_day4 = result_day34,
    day3_day8 = result_day38
  )
}

print(results)












