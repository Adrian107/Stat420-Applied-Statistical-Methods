# simulation study for the distribution of beta

# set the true parameters and X 

n = 10
sigma = 1.5
x = rnorm(n, 0, 2)
beta0 = 1.5
beta1 = 2

# simualtion 
hatb = matrix(NA, 10000, 2)

for (i in 1:10000)
{
  y = beta0 + beta1*x + rnorm(n, 0, sigma)
  hatb[i, ] = lm(y ~ x)$coef
}

# the simulated variance-covariance of hat beta (page 8 of notes)
hatb[,1] = hatb[,1] - beta0
hatb[,2] = hatb[,2] - beta1
t(hatb) %*% hatb / 10000
cov(hatb)
# the theoritical value 
X = cbind(1, x)
sigma^2 * solve(t(X) %*% X)  # page 8 of notes


# p-value and CI 

library(faraway)
data(cheddar)

# a SLR case: use Lactic and intercept 
# the design matrix 

X = cbind(1, cheddar$Lactic)
n = nrow(cheddar)
sigma2 = sum(lm(taste ~ Lactic, data= cheddar)$residuals^2)/(n-2)
betah = lm(taste ~ Lactic, data= cheddar)$coef

# calculate the variance of beta0, without sigma^2

1/n + mean(X[,2])^2 / (n-1) / var(X[,2])

# calculate the variance of beta1,without sigma^2

1 / (n-1) / var(X[,2])

# check the calculations, X'X inverse without sigma^2

V = solve(t(X) %*% X)

# the standard errors of the two paramters 

sqrt(sigma2)*sqrt(c(V[1,1], V[2,2]))

# check the standard errors from the lm() fit 
summary(lm(taste ~ Lactic, data= cheddar))

# the 95% CI for each parameter, are they significant at 5% level?

betah - qt(0.975, df = n-2)*sqrt(sigma2)*sqrt(c(V[1,1], V[2,2])) # lower bound
betah + qt(0.975, df = n-2)*sqrt(sigma2)*sqrt(c(V[1,1], V[2,2])) # upper bound

# check these results with the confint() function

fit = lm(taste ~ Lactic, data= cheddar)
confint(fit, level = 0.95)


# caluclate the p-values for testing beta0 = 0 vs. beta0 != 0 (two-sided)

2*pt(abs(betah[1] / sqrt(sigma2) / sqrt(V[1,1])), df = n-2, lower.tail = FALSE)


# caluclate the p-values for testing beta1 = 0 vs. beta1 != 0 (two-sided)

2*pt(abs(betah[2] / sqrt(sigma2) / sqrt(V[2,2])), df = n-2, lower.tail = FALSE)


# prediction on new subjects (mean value)

xnew <- data.frame(Lactic = 1)
predict.lm(fit, xnew, interval = c("confidence"), level = 0.9)


xnew <- data.frame(Lactic = 2)
predict.lm(fit, xnew, interval = c("confidence"), level = 0.95)

# predition on new subjects (response)

xnew <- data.frame(Lactic = 2)
predict.lm(fit, xnew, interval = c("prediction"), level = 0.95)

# hand calculation
x = cheddar$Lactic

betah %*% c(1, 2) + qt(0.975, df = n-2)*sqrt(sigma2)*sqrt(1 + 1/n + (2-mean(x))^2 / var(x) / (n-1))
betah %*% c(1, 2) + qt(0.975, df = n-2)*sqrt(sigma2)*sqrt(1/n + (2-mean(x))^2 / var(x) / (n-1))



X = cbind(1, cheddar$Acetic)
n = nrow(cheddar)
sigma2 = sum(lm(taste ~ Acetic, data= cheddar)$residuals^2)/(n-2)
betah = lm(taste ~ Acetic, data= cheddar)$coef


sqrt(sigma2)/sqrt(var(X[,2])*(n-1))


betah[2] + qt(0.95, df = n-2)*sqrt(sigma2)/sqrt(var(X[,2])*(n-1))
betah[2] - qt(0.95, df = n-2)*sqrt(sigma2)/sqrt(var(X[,2])*(n-1))

# check these results with the confint() function

fit = lm(taste ~ Acetic, data= cheddar)
confint(fit, level = 0.9)





# the star data 

data(star)

fit = lm(temp~light, data = star)
summary(fit)

xnew <- data.frame(light = 6)
predict.lm(fit, xnew, interval = c("confidence"), level = 0.90)


predict.lm(fit, xnew, interval = c("prediction"), level = 0.90)

