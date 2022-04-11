# Loading data
life_data <- read.csv("C:/Users/haras/Desktop/Intro to DS/Lab Assig 4/Life Expectancy Data.csv")
life_data

# check the data and get the numeric columns
numeric_columns <-unlist(lapply(life_data,class))=="numeric"
numeric_columns

# check the correlation with all observations - omit NA
cor(life_data$Life.expectancy, life_data$Adult.Mortality,  use = "complete.obs")
cor(life_data[,numeric_columns],  use = "complete.obs")
cor(life_data[,numeric_columns],method="spearman", use = "complete.obs")
plot(life_data[,numeric_columns])

# create model using  Life.expectancy vs Adult.Mortality
lm(formula = Life.expectancy ~  life_data$Adult.Mortality, data = life_data)
fit1 <- lm( Life.expectancy ~ Adult.Mortality, data=life_data)
fit1


# check different variables using plot 
plot(life_data$Life.expectancy, life_data$Adult.Mortality , pch=19, col="blue")
plot(life_data$Life.expectancy, life_data$infant.deaths , pch=19, col="red")
plot(life_data$Life.expectancy, life_data$Alcohol , pch=19, col="red")
plot( life_data$Life.expectancy, life_data$GDP , pch=19, col="blue")
plot( life_data$Life.expectancy, life_data$HIV.AIDS , pch=19, col="blue")

plot( life_data$Life.expectancy, life_data$HIV.AIDS , pch=19, col="blue")
plot( life_data$Life.expectancy, life_data$Population , pch=19, col="red")
plot( life_data$Life.expectancy, life_data$Measles , pch=19, col="purple")

plot( life_data$Life.expectancy, life_data$Diphtheria , pch=19, col="purple")

plot( life_data$Life.expectancy, life_data$Alcohol , pch=19, col="red")

plot( life_data$Life.expectancy, life_data$Schooling , pch=19, col="maroon")

plot( life_data$Life.expectancy, life_data$BMI , pch=19, col="red")


#check the summary
sum1 <- summary(fit1)
names(sum1)
sum1$coefficients
pval <- sum1$coefficients[,4]
pval <= 0.05

# Get r-squared value
fit1r2 <- sum1$r.squared
fit1r2
fit1aic <- AIC(fit1)
fit1aic

# create model using  Life.expectancy vs HIV.AIDS
fit2 <- lm( Life.expectancy ~ HIV.AIDS ,data=life_data)
sum2 <- summary(fit2) # Get summary statistics
sum2
fit2r2 <- sum2$r.squared
fit2aic <- AIC(fit2)
sum2$coefficients[,4] <= 0.05
sum2$coefficients

#compare the two models
r2 <- c(fit1r2,fit2r2)
r2

# model 
round(r2,2)

names(r2) <- c("Adult.Mortality","HIV.AIDS")
round(r2,3)

# Check AIC value
aic <- c(fit1aic,fit2aic)
names(aic) <- names(r2)
round(aic,3)

fit3 <- lm(Life.expectancy ~ Country ,data=life_data) # Fit the regression model
sum3 <- summary(fit3) # Get summary statistics
sum3

fit3r2 <- sum3$r.squared
fit3aic <- AIC(fit3)

# Model with intercept
lm(Life.expectancy ~ Adult.Mortality, data=life_data)
lm(Life.expectancy  ~ 0 + HIV.AIDS, data=life_data)




###### 5 Multiple regression using Adult.Mortality , HIV.AIDS , Alcohol  #######
fit4 <- lm( Life.expectancy ~ Adult.Mortality + HIV.AIDS + Alcohol , data=life_data)
sum4 <- summary(fit4)
sum4

yy <- life_data[,colnames(life_data)=="Life.expectancy"]
yy <- yy[1:10]

xx <- matrix(runif(90),ncol=9)
ftemp <- list() 
for (i in 1:9){
  ftemp[[i]] <- lm(yy ~xx[,1:i])
}

# plot
r2temp <- unlist(lapply(ftemp,function(x){summary(x)$r.squared}))
plot(1:9, r2temp, xlab="Input", ylab="R-squared", main="Model - R-squared")

sapply(ftemp,AIC)
yy - ftemp[[9]]$fitted.values
ftemp[[10]] <- lm(yy~1) # This means just fit a constant
sapply(ftemp,AIC)
unlist(lapply(ftemp,function(x){summary(x)$r.squared}))

# used tidyverse to omit the non-numeric columns
install.packages("tidyverse")
library(tidyverse)
life_data <- life_data %>% 
  drop_na()

#Create and compare models
fit5 <- lm(Life.expectancy ~.,data= life_data)
summary(fit5)
fitmin <- lm( Life.expectancy ~ 1, data=life_data)

fit6 <- step(fitmin,direction="both",scope=formula(fit5)) 

fit7 <- step(fitmin,direction="forward",scope=formula(fit5))

summary(fit7)
fit8 <- step(fit5,direction="backward",scope=formula(fit5))
summary(fit8)

aic <- c(AIC(fit1),AIC(fit2),AIC(fit5),AIC(fit6))
names(aic) <- c(formula(fit1),formula(fit2),"Full model","Stepwise")
round(aic,4)

plot(fit6)

par(mfrow=c(2,2)) # Split into 2 by 2
plot(fit6)

resid <- fit6$residuals
fitted <- fit6$fitted.values

colnames(life_data)
par(mfrow=c(2,2))
plot(fitted,resid) # Scatter plot fitted vs. residuals
plot(life_data$Adult.Mortality,resid) # Scatter plot lstat vs. residuals
plot(life_data$Alcohol,resid) # Scatter lat vs. residuals
hist(resid,100) # Histogram of residuals with 100 bins

idx <- sort(sample(1:nrow(life_data),100))
xTest <- life_data[idx,]
xTrain <- life_data[-idx,]

fitTrain <- lm(Life.expectancy ~ Adult.Mortality + HIV.AIDS, data=xTrain)
fitTrain

# Prediction
predict(fitTrain,newdata=xTest)
