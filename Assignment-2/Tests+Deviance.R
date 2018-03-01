library(faraway)
data(orings)
attach(orings)
head(orings)

# Fit a binomial GLM as before, but record the model matrix in the output of the glm function

logitmod <- glm(cbind(damage,6-damage)~temp,family=binomial,x=TRUE)

# Fisher information at the last iteration

I <- t(logitmod$x)%*%diag(logitmod$weights)%*%logitmod$x

# Inverse Fisher information matrix (i.e. as. covariance matrix of the MLEs)

I.inv <- solve(I)

# standard errors of the MLEs

sd <- sqrt(diag(I.inv))

# checking .... 

summary(logitmod)

# computing the Wald statistics (testing whether beta_j = 0 for j=1,2)

beta <- logitmod$coefficients
p.val <- pchisq((beta/sd)^2,df=1,lower.tail=FALSE)

p.val

# Testing that beta_1 = 0 AND beta_2 = 0 using the Wald statistic

library(Matrix)
as.numeric(rankMatrix(I.inv))

W <- t(matrix(beta,nrow=2))%*%I%*%matrix(beta,nrow=2)
pchisq(W,df=2,lower.tail=FALSE)

# Checking the fit of the model using the deviance statistic

deviance(logitmod)

pchisq(deviance(logitmod),df=21,lower.tail=FALSE)

# Comparing models

# sily model

sq.temp <- temp^2

logitmod2 <- glm(cbind(damage,6-damage)~temp+sq.temp,family=binomial)

# As we see from the analysis of deviance, there is no need to fit the more complex model 

anova(logitmod,logitmod2,test="Chi")








