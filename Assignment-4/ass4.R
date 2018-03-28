library(aod)
library(VGAM)
library(nnet)

#A12
game <- 1:24
attempts <- c(4,9,11,6,6,7,7,1,8,9,5,5,5,4,7,3,7,2,11,8,4,4,5,7)
shots <- c(0,7,4,3,5,2,3,0,1,6,0,2,0,2,5,1,3,0,8,0,0,0,2,2)

model.rayallen <- glm(cbind(shots,attempts-shots) ~ 1, family=binomial)
pchisq(deviance(model.rayallen), df=23, lower.tail = FALSE)

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
lowerCI <- exp(beta-pnorm(0.025)*stdErr)
higherCI <- exp(beta+pnorm(0.025)*stdErr)

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
lowerCI2 <- exp(beta2-pnorm(0.025)*stdErr2)
higherCI2 <- exp(beta2+pnorm(0.025)*stdErr2)

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
lowerCI.gender <- exp(beta.gender-pnorm(0.025)*stderr.gender)
higherCI.gender <- exp(beta.gender+pnorm(0.025)*stderr.gender)
#Odds ratio and confidence interval
gender.oddsratio
lowerCI.gender
higherCI.gender

model.heavenRace <- glm(cbind(heaven.yes, heaven.no) ~ race, family=binomial)
summary(model.heavenRace)
anova(model.heavenB, model.heavenRace, test="Chi")
