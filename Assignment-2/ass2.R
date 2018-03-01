library(glmbb)
data(crabs)

fullModel <- glm(satell~(weight+color)^2, data=crabs, family=poisson)
summary(fullModel)

X <- model.matrix(fullModel)
W <- fullModel$weights

beta <- fullModel$coefficients
beta2 <- beta[6:8]

I <- t(X) %*% diag(W) %*% X
I2 <- I[6:8,6:8]

waldTest <- t(beta2) %*% I2 %*% beta2
pchisq(waldTest, 3, lower.tail=FALSE)

#Deviance test
reducedModel <- glm(satell~weight+color, data=crabs, family=poisson)
devTest <- deviance(reducedModel) - deviance(fullModel)
pchisq(devTest, 3, lower.tail=FALSE)

newColor <- crabs$color
levels(newColor) <- c("dark", "dark", "light", "medium")

model2 <- glm(satell~(weight+newColor)^2, data=crabs, family=poisson)
summary(model2)

#Residuals
res_p <- residuals(model2, "pearson")
plot(fitted(model2), res_p, xlab="Satellites", ylab="Pearson residual")
title("Pearson Residuals vs Fitted Response")

res_d <- residuals(model2, "deviance")
plot(fitted(model2), res_d, xlab="Satellites", ylab="Deviance residual")
title("Deviance Residuals vs Fitted Response")

deviance(model2)

