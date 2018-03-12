set.seed(1)
n <- 100
x <- runif(n, 0, 1)
logit.pi <- 0.4*x - 2
pi <- exp(logit.pi)/(exp(logit.pi)+1)
y <- c()
for (i in 1:n){
  y[i] <- rbinom(1, 1, pi[i])
}
model <- glm(y~logit.pi, family=binomial)
