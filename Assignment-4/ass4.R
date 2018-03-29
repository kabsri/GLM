library(aod)
library(VGAM)
library(nnet)

#A12
game <- 1:24
attempts <- c(4,9,11,6,6,7,7,1,8,9,5,5,5,4,7,3,7,2,11,8,4,4,5,7)
shots <- c(0,7,4,3,5,2,3,0,1,6,0,2,0,2,5,1,3,0,8,0,0,0,2,2)

model.rayallen <- glm(cbind(shots,attempts-shots) ~ 1, family=binomial)
summary(model.rayallen)
pchisq(deviance(model.rayallen), df=23, lower.tail = FALSE)

model.rayallen.quasi <- glm(cbind(shots,attempts-shots) ~ 1, family=quasibinomial)
summary(model.rayallen.quasi)

rayallen.data <- data.frame(shots, attempts-shots)
model.rayallen.quasi2 <- quasibin(cbind(shots, attempts-shots) ~ 1, data=rayallen.data, link = c("logit"))
model.rayallen.quasi2@fm$coefficients

model.rayallen.betabin <- vglm(cbind(shots, attempts-shots) ~ 1, betabinomial(zero=2, irho=0.2))
summary(model.rayallen.betabin)

beta.rayallen <- summary(model.rayallen)$coefficients[,1]
stderr.rayallen <- summary(model.rayallen)$coefficients[,2]
beta.quasi <- summary(model.rayallen.quasi)$coefficients[,1]
stderr.quasi <- summary(model.rayallen.quasi)$coefficients[,2]
beta.quasi2 <- model.rayallen.quasi2@fm$coefficients
stderr.quasi2 <- sqrt(vcov(model.rayallen.quasi2))
beta.betabin <- coefficients(summary(model.rayallen.betabin))[,1][1]
stderr.betabin <- coefficients(summary(model.rayallen.betabin))[,2][1]

rayallen.ci <- c(beta.rayallen - qnorm(0.975)*stderr.rayallen, beta.rayallen + qnorm(0.975)*stderr.rayallen)
quasi.ci <- c(beta.quasi - qnorm(0.975)*stderr.quasi, beta.quasi + qnorm(0.975)*stderr.quasi)
quasi2.ci <- c(beta.quasi2 - qnorm(0.975)*stderr.quasi2, beta.quasi2 + qnorm(0.975)*stderr.quasi2)
betabin.ci <- c(beta.betabin - qnorm(0.975)*stderr.betabin, beta.betabin + qnorm(0.975)*stderr.betabin)

rayallen.ci
quasi.ci
quasi2.ci
betabin.ci

#A13
I <- c(14, 483, 497, 1008)
notI <- c(1105, 411111, 4624, 157342)
S <- c("Seat belt", "Seat belt", "None", "None")
E <- c("Yes", "No", "Yes", "No")

model.fatal <- glm(cbind(I, notI) ~ S+E, family=binomial)
pchisq(deviance(model.fatal), df=1, lower.tail=FALSE)
pchisq(sum(residuals(model.fatal, type="pearson")^2), df=1, lower.tail=FALSE)

summary(model.fatal)
beta <- coef(model.fatal)
odds.ratio <- exp(beta)
stdErr <- summary(model.fatal)$coefficients[,2]
lowerCI <- exp(beta-qnorm(0.975)*stdErr)
higherCI <- exp(beta+pnorm(0.975)*stdErr)

#Odds ratio and confidence interval for Seatbelt
odds.ratio[2]
lowerCI[2]
higherCI[2]
#Odds ratio and confidence interval for Ejected
odds.ratio[3]
lowerCI[3]
higherCI[3]

model.fatal2 <- glm(cbind(I, notI) ~ E, family=binomial)
beta2 <- coef(model.fatal2)
odds.ratio2 <- exp(beta2)
stdErr2 <- summary(model.fatal2)$coefficients[,2]
lowerCI2 <- exp(beta2-qnorm(0.975)*stdErr2)
higherCI2 <- exp(beta2+qnorm(0.975)*stdErr2)

#Odds and confidence interval
odds.ratio2[2]
lowerCI2[2]
higherCI2[2]

#A14
race <- c(1, 1, 0, 0)
gender <- c(1, 0, 1, 0)
heaven.yes <- c(88, 54, 397, 235)
heaven.mb <- c(16, 7, 141, 189)
heaven.no <- c(2, 5, 24, 39)

model.heaven <- multinom(cbind(heaven.yes, heaven.mb, heaven.no) ~ race+gender)
summary(model.heaven)

model.heavenB <- glm(cbind(heaven.yes, heaven.no) ~ race+gender, family=binomial)
summary(model.heavenB)

beta.gender <- coef(model.heavenB)[3]
stderr.gender <- summary(model.heavenB)$coefficients[,2][3]
gender.oddsratio <- exp(beta.gender)
lowerCI.gender <- exp(beta.gender-qnorm(0.975)*stderr.gender)
higherCI.gender <- exp(beta.gender+qnorm(0.975)*stderr.gender)
#Odds ratio and confidence interval
gender.oddsratio
lowerCI.gender
higherCI.gender

model.heavenRace <- glm(cbind(heaven.yes, heaven.no) ~ race, family=binomial)
summary(model.heavenRace)
anova(model.heavenB, model.heavenRace, test="Chi")
