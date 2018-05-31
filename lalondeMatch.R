library(foreign)
data <- read.dta("nsw_dw.dta")

#1. point estimate treatment effect
treat.mean <- sum(data$re78[data$treat==1])/nrow(data[data$treat==1,])
control.mean <- sum(data$re78[data$treat==0])/nrow(data[data$treat==0,])
point.estimate = treat.mean-control.mean

#univariate regression, confidence interval
uni.reg <- lm(data$re78 ~ data$treat)
confint(uni.reg, level = 0.90)

#2. cps controls
cps.controls <- read.dta("cps_controls.dta")
data.cps <- rbind(data[data$treat==1,],cps.controls)

#point estimate treatment effect
treat.mean.cps <- sum(data.cps$re78[data.cps$treat==1])/nrow(data.cps[data.cps$treat==1,])
control.mean.cps <- sum(data.cps$re78[data.cps$treat==0])/nrow(data.cps[data.cps$treat==0,])
point.estimate.cps = treat.mean.cps-control.mean.cps

#univariate regression, confidence interval
uni.reg.cps <- lm(data.cps$re78 ~ data.cps$treat)
confint(uni.reg.cps, level = 0.90)

#3. Matching on propensity score
library(Matching)
propensity.reg <- glm(data.cps$treat~data.cps$age + data.cps$educ + data.cps$black +
                  data.cps$hisp + data.cps$married + data.cps$nodegr + data.cps$re74+ data.cps$re75,
                  family=binomial, data=data.cps)
data.cps$propensity = propensity.reg$fitted
X  <- data.cps$propensity
Y  <- data.cps$re78
Tr  <- data.cps$treat
rr  <- Match(Y=Y, Tr=Tr, X=X, M=1)
mb <- MatchBalance(data.cps$treat~data.cps$age + data.cps$educ + data.cps$black +
                     data.cps$hisp + data.cps$married + data.cps$nodegr + data.cps$re74+ data.cps$re75,
                   match.out=rr, nboots=500)
balance1 = mb$AMsmallest.p.value

#point estimate treatment effect
treat.mean.propensity <- mean(data.cps[rr$index.treated,]$re78)
control.mean.propensity <- mean(data.cps[rr$index.control,]$re78)
point.estimate.propensity = treat.mean.propensity-control.mean.propensity

#univariate regression, confidence interval
data.propensity <- data.cps[rbind(rr$index.control,rr$index.treated),]
uni.reg.propensity <- lm(data.propensity$re78 ~ data.propensity$treat)
confint(uni.reg.propensity, level = 0.90)

#4. Matching on all variables and propensity score
rr2 <- Match(Y=Y, Tr=Tr, X=data.cps[,c(2:10,12)], M=1)
mb2 <- MatchBalance(data.cps$treat~data.cps$age + data.cps$educ + data.cps$black +
                      data.cps$hisp + data.cps$married + data.cps$nodegr + data.cps$re74+ data.cps$re75,
                    match.out=rr2, nboots=500)
balance2 = mb2$AMsmallest.p.value

#point estimate treatment effect
treat.mean.match <- mean(data.cps[rr2$index.treated,]$re78)
control.mean.match <- mean(data.cps[rr2$index.control,]$re78)
point.estimate.match = treat.mean.match-control.mean.match

#univariate regression, confidence interval
data.match <- data.cps[rbind(rr2$index.control,rr2$index.treated),]
uni.reg.match <- lm(data.match$re78 ~ data.match$treat)
confint(uni.reg.match, level = 0.90)

#5a. Genetic Matching on propensity score
genout <- GenMatch(Tr=Tr, X=propensity.reg$fitted, estimand="ATE", M=1, pop.size=16,max.generations=10,wait.generations=1)
#nonsensical

#5b. Genetic Matching on propensity score with all other variables
genout2 <- GenMatch(Tr=Tr, X=data.cps[,c(2:10,12)], estimand="ATT", M=1, pop.size=100,max.generations=20,wait.generations=5)
rr3 <- Match(Y=Y, Tr=Tr, X=data.cps[,c(2:10,12)], estimand="ATT", Weight.matrix = genout2)
mb3 <- MatchBalance(data.cps$treat~data.cps$age + data.cps$educ + data.cps$black +
                      data.cps$hisp + data.cps$married + data.cps$nodegr + data.cps$re74+ data.cps$re75,
                   match.out=rr3, nboots=500)
balance3 = mb3$AMsmallest.p.value

#point estimate treatment effect
treat.mean.gen <- mean(data.cps[rr3$index.treated,]$re78)
control.mean.gen <- mean(data.cps[rr3$index.control,]$re78)
point.estimate.gen = treat.mean.gen-control.mean.gen

#univariate regression, confidence interval
data.gen<- data.cps[rbind(rr3$index.control,rr3$index.treated),]
uni.reg.gen <- lm(data.gen$re78 ~ data.gen$treat)
confint(uni.reg.gen, level = 0.90)