beetles <- read.table("Beetles2.dat", header=TRUE)
attach(beetles)

logitmod <- glm(cbind(dead, n-dead)~logdose, family=binomial)
summary(logitmod)
logitmod$coefficients

probitmod <- glm(cbind(dead,n-dead)~logdose,family=binomial(link=probit))
loglogmod <- glm(cbind(dead,n-dead)~logdose,family=binomial(link=cloglog))

plot(dead/n~logdose,xlim=c(1.6,1.9),ylim=c(0,1),xlab="Log dose",ylab="Prob of death")
x <- seq(from=1.6,to=1.9,by=0.005)

lines(x,exp(coef(logitmod)[1]+coef(logitmod)[2]*x)/(1+exp(coef(logitmod)[1]+coef(logitmod)[2]*x)),col="red")
lines(x,pnorm(coef(probitmod)[1]+coef(probitmod)[2]*x),col="darkgreen")
lines(x,1-exp(-exp(coef(loglogmod)[1]+coef(loglogmod)[2]*x)),col="blue")

exp(coef(logitmod)[1]+coef(logitmod)[2]*logdose)/(1+exp(coef(logitmod)[1]+coef(logitmod)[2]*logdose))
pnorm(coef(probitmod)[1]+coef(probitmod)[2]*logdose)
1-exp(-exp(coef(loglogmod)[1]+coef(loglogmod)[2]*logdose))

logitmod$aic
probitmod$aic
loglogmod$aic
