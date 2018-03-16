#A8 c)
a.bin <- c(0,1,3)
bin <- cbind(c(1,2,4),c(2,2,1))
a.bern <- c(0,0,0,1,1,1,1,3,3,3,3,3)
bern <- cbind(c(1,0,0,1,1,0,0,1,1,1,1,0), c(0,1,1,0,0,1,1,0,0,0,0,1))
mbin <- glm(bin~a.bin, family=binomial)
mbin.noint <- glm(bin~a.bin-1, family=binomial)
mbern <- glm(bern~a.bern, family=binomial)
mbern.noint <- glm(bern~a.bern-1, family=binomial)
summary(mbin)
summary(mbin.noint)
summary(mbern)
summary(mbern.noint)

#A9 a)
set.seed(1)
n <- 100
x <- runif(n, 0, 1)
logit.pi <- 0.4*x - 2
pi <- exp(logit.pi)/(exp(logit.pi)+1)
y <- rbinom(n, 1, pi)


model <- glm(y~x, family=binomial)
fit <- fitted(model)
beta <- coefficients(model)

sx <- sort(x)
seta <- beta[1]+beta[2]*sx
sfit <- exp(seta)/(1+exp(seta))

pres <- residuals(model, "pearson")
plot(fit, pres, xlab="Fitted values", ylab="Residuals", main="Pearson residuals")
lines(sfit, (1-sfit)/(sqrt(sfit*(1-sfit))), col="blue")
lines(sfit, (0-sfit)/(sqrt(sfit*(1-sfit))) ,col="red")

dres <- residuals(model, "deviance")
plot(fit, dres, xlab="Fitted values", ylab="Residuals", main="Deviance Residuals")
lines(sfit, sqrt(2*log(1/sfit)), col="blue")
lines(sfit, -sqrt(2*log(1/(1-sfit))), col="red")

#A9 b)
getHistogram <- function(n, s){
  set.seed(s)
  k <- n-2
  N <- 1000
  x <- runif(n, 0, 1)
  logit.pi <- 0.4*x - 2
  pi <- exp(logit.pi)/(exp(logit.pi)+1)
  D <- c()
  for (i in 1:N){
    y <- rbinom(n, 1, pi)
    M = glm(y~x, family=binomial)
    D[i] <- deviance(M)
  }
  hist(D, xlab="Deviance", main=paste("Deviances for n =",n), freq=FALSE)
  nodes <- min(D):max(D)
  lines(nodes, dchisq(nodes, k) ,col="green")
}
getHistogram(100, 1)

#A9 c)
getHistogram(1000, 1)

#A11 a)
wells <- read.table("wells.dat")
attach(wells)
dist100 <- dist/100
m1 <- glm(switch~dist100, family=binomial)
summary(m1)
bins <- seq(floor(min(dist100)),ceiling(max(dist100)),0.2)
counts.succ <- rep(0, length(bins))
m <- rep(0, length(bins))
for (i in 1:length(dist100)){
  low <- 1
  high <- length(bins)
  while (low<=high){
    k <- floor((low+high)/2)
    if (abs(bins[k]-dist100[i])<=0.1){
      if (switch[i]==1){
        counts.succ[k] <- counts.succ[k]+1
      }
      m[k] <- m[k]+1
      break
    } else if (dist100[i]<bins[k]){
      high <- k-1
    } else {
      low <- k+1
    }
  }
}
ftd <- exp(m1$coefficients[1]+m1$coefficients[2]*bins)/(exp(m1$coefficients[1]+m1$coefficients[2]*bins)+1)
groupDevs <- counts.succ*log(counts.succ/(m*ftd)) + (m-counts.succ)*log((m-counts.succ)/(m-m*ftd))
groupDevs[13:length(groupDevs)] <- 0
G2 <- 2*sum(groupDevs)
pchisq(G2, df=19, lower.tail=FALSE)
pearson.test <- sum(residuals(m1, type="pearson")^2)
pchisq(pearson.test, df=3018, lower.tail=FALSE)

m2 <- glm(switch ~ dist100+arsenic, family=binomial)
anova(m1, m2, test="Chi")
m3 <- glm(switch ~ (dist100+arsenic)^2, family=binomial)
anova(m2, m3, test="Chi")
m4 <- glm(switch ~ dist100+arsenic+educ, family=binomial)
anova(m1, m4, test="Chi")
m5 <- glm(switch ~ dist100+arsenic+educ+dist100*educ, family=binomial)
anova(m4, m5, test="Chi")
m6 <- glm(switch ~ dist100+arsenic+educ+dist100*educ+arsenic*educ, family=binomial)
anova(m5, m6, test="Chi")
m7 <- glm(switch ~ dist100+arsenic+educ+dist100*educ+assoc, family=binomial)
anova(m5, m7, test="Chi")
anova(m5, m2, test="Chi")
summary(m5)

roc.curve <- function(y,pred){
  p <- seq(from=0,to=1,by=0.01)
  out <- matrix(ncol=2,nrow=length(p))
  for(i in 1:length(p)){
    y.hat <- as.numeric(pred>p[i])
    tmp <- cbind(y,y.hat)
    I1 <- as.numeric(y==1)
    I2 <- as.numeric(y.hat==1)
    a <- sum(I1*I2)
    b <- sum(I1*(1-I2))
    c <- sum((1-I1)*I2)
    d <- sum((1-I1)*(1-I2))
    sens <- a/(a+b)
    spec <- d/(c+d)
    out[i,1] <- 1-spec
    out[i,2] <- sens
  }
  out  
}

pred1 <- predict(m1, type="response")
roc1 <- roc.curve(switch, pred1)
pred5 <- predict(m5, type="response")
roc5 <- roc.curve(switch, pred5)

plot(roc1,type="l",xlab="x",ylab="y",main="ROC curves",col="red")
lines(roc5, col="blue")
