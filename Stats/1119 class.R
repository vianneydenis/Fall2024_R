install.packages("gvlma")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("mvabund")

library(gvlma)
library(Hmisc)
library(corrplot)
library(mvabund)
library(ggplot2)

Plant_height <- read.csv(file = "Stats/Plant_height.csv", header = TRUE)
mod1_plant<- lm(loght ~ temp, data = Plant_height)
mod1_plant$coefficients
summary(mod1_plant)
confint(mod1_plant)
sigma(mod1_plant)*100/mean(Plant_height$loght)

fitted(mod1_plant) # predicted values
residuals(mod1_plant) # residuals
anova(mod1_plant) # anova table
vcov(mod1_plant) # covariance matrix for model parameters
influence(mod1_plant) # regression diagnostics

plot(mod1_plant, which = 1)
