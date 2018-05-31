library(Matching)
data(lalonde)

#data cleaning
lalonde <- lalonde[sample(nrow(lalonde)),]


#For high school graduates:
lalonde.high <- subset(lalonde, lalonde$nodegr == 0)[,-6][,-7][,-9]
lalonde.low <- subset(lalonde, lalonde$nodegr == 1)[,-6][,-7][,-9]

#split train and test sets
indexes.high = sample(1:nrow(lalonde.high), size=0.2*nrow(lalonde.high))
lalonde.high.train = lalonde.high[-indexes.high,]
lalonde.high.test = lalonde.high[indexes.high,]

#regression
#model selection:
library("DAAG")
modelselection <- function(data, degrees, k){
  cv.error = integer(degrees)
  for(i in 1:degrees){
    #fit regression model
    fit.high <- lm(re78 ~ poly(age,i) +poly(re74,i) +poly(I(re74*age),i)+ educ + black + hisp + married + u74 + treat,data)
    
    #assess error by cross validation
    cv <- cv.lm(data=lalonde.high, fit.high, m=k)
    cv.error[i] = sqrt(attr(cv,"ms"))
  }
  plot(seq(1,degrees,1), cv.error)
  lines(seq(1,degrees,1),cv.error)
  best <- which.min(cv.error)
  return(best)
}

polynomial = modelselection(lalonde.high.train, 4, 10)
fit.high.best <- lm(re78 ~ poly(age,polynomial) +poly(re74,polynomial) + educ + black + hisp + married + u74 + treat,lalonde.high.train)
predictions <- predict(fit.high.best,lalonde.high.test)
rmse = sqrt(mean((predictions - lalonde.high.test$re78)^2))

#backward selection and refitting

step <- step(fit.high.best, direction = "backward")
fit.high.best <- lm(re78 ~ poly(re74,polynomial)+ treat,lalonde.high.train)
predictions <- predict(fit.high.best,lalonde.high.test)
rmse = sqrt(mean((predictions - lalonde.high.test$re78)^2))

#evaluate treatment effect

averageeffect.high <- coef(fit.high.best)["treat"]
averageeffect.high.ci <- confint(fit.high.best,"treat")

#repeat for non graduates
#split train and test sets
indexes.low = sample(1:nrow(lalonde.low), size=0.2*nrow(lalonde.low))
lalonde.low.train = lalonde.low[-indexes.low,]
lalonde.low.test = lalonde.low[indexes.low,]

#model selection
polynomial = modelselection(lalonde.low.train, 5, 10)
fit.low.best <- lm(re78 ~ poly(age,polynomial) +poly(re74,polynomial) + educ + black + hisp + married + u74 + treat,lalonde.low.train)
predictions <- predict(fit.low.best,lalonde.low.test)
rmse2 = sqrt(mean((predictions - lalonde.low.test$re78)^2))

#backward selection and refitting
step <- step(fit.low.best, direction = "backward")
fit.low.best <- lm(re78 ~ black + treat,lalonde.low.train)
predictions <- predict(fit.low.best,lalonde.low.test)
rmse2 = sqrt(mean((predictions - lalonde.low.test$re78)^2))

#evaluate treatment effect
averageeffect.low <- coef(fit.low.best)["treat"]
averageeffect.low.ci <- confint(fit.low.best,"treat")
averageeffect.diff = averageeffect.low-averageeffect.high

#decision trees
library("randomForest")
forest.high <- randomForest(re78 ~ age+re7+educ+black+hisp+married+u74+treat,lalonde.high.train)
forest.low <- randomForest(re78 ~ age+re74+educ+black+hisp+married+u74+treat,lalonde.low.train)

averageeffect.forest.high = mean(predict(forest.high,treated.high))-mean(untreated.high$re78)
averageeffect.forest.low = mean(predict(forest.low,treated.low))-mean(untreated.low$re78)
predictions <- predict(forest.high,lalonde.high.test)
rmse3 = sqrt(mean((predictions - lalonde.high.test$re78)^2))
predictions <- predict(forest.low,lalonde.low.test)
rmse3 = sqrt(mean((predictions - lalonde.low.test$re78)^2))
averageeffect.forest.diff = averageeffect.forest.low-averageeffect.forest.high

plot(forest.high)
print(importance(forest.high, type=2))
print(importance(forest.low, type=2))

#bootstrapping
library("boot")

#FET

# Assignment function
assignment <- function(fisher.data) {
  # Shuffle data, establishing random assignment
  fisher.data <- fisher.data[sample(nrow(fisher.data)),]
  
  treatment.group   <- fisher.data[1:floor(nrow(fisher.data)/2),]
  control.group     <- fisher.data[ceiling(nrow(fisher.data)/2):nrow(fisher.data),]

  return(mean(treatment.group$re78) - mean(control.group$re78))
}

# Iterating the Assignment function
iter.RI <- function(data,effect,iterations = 1000) {
  storage.vector <- NULL
  for (i in 1:iterations) 
  {
  storage.vector[i] <- assignment(data)+effect
  }
  return(storage.vector)
}

#checking different hypotheses - can't get it to work:
hypotheses <-function(min,max,data,step = 1,iter = 100){
  quantile.data <- matrix(nrow=3,ncol=((max-min)/step)+1)
  quantile.data[1,] = seq(min,max,step)
  for(i in 1:length(quantile.data[1,])){
    results <- iter.RI(data,i,iterations=iter)
    quantile.data[2,i] = quantile(results, prob = 0.05)
    quantile.data[3,i] = quantile(results, prob = 0.95)
  }
  plot(quantile.data[1,],quantile.data[2,], col="red",ylim=c(-3000,3000),type="l")
  lines(quantile.data[1,],quantile.data[3,], col="green")
  abline(v=mean(data[data$treat==1,]$re78)-mean(data[data$treat==0,]$re78))
  return(quantile.data)
}

#all
results <- iter.RI(lalonde,0)
alpha <- quantile(results, prob = c(0.95, 0.05))
plot(density(results), xlim=c(-5000, 5000))
yobs = mean(lalonde[lalonde$treat==1,]$re78)-mean(lalonde[lalonde$treat==0,]$re78)
abline(v = yobs, lwd = 2)
abline(v=alpha[1],lty=2)
abline(v=alpha[2],lty=2)
#high
results <- iter.RI(lalonde.high,0)
alpha.high <- quantile(results, prob = c(0.95, 0.05))
lines(density(results), col="green")
yobs.high = mean(lalonde.high[lalonde.high$treat==1,]$re78)-mean(lalonde.high[lalonde.high$treat==0,]$re78)
abline(v = yobs.high, lwd = 2, col = "green")
abline(v=alpha.high[1], col = "green", lty=2)
abline(v=alpha.high[2], col = "green", lty=2)
#low
results <- iter.RI(lalonde.low,0)
alpha.low <- quantile(results, prob = c(0.95, 0.05))
lines(density(results),col="red")
yobs.low = mean(lalonde.low[lalonde.low$treat==1,]$re78)-mean(lalonde.low[lalonde.low$treat==0,]$re78)
abline(v = yobs.low, lwd = 2, col = "red")
abline(v=alpha.low[1], col="red",lty=2)
abline(v=alpha.low[2], col="red",lty=2)

#cohen's d
library("lsr")
cohensD(lalonde.low$re78,lalonde.high$re78)
