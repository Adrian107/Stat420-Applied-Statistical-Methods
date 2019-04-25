n = 500
p = 1

X = matrix(rnorm(n*p), n, p)
beta = rep(1, p)

y = 1 + X %*% beta + rnorm(n)*exp(X %*% beta / 3)

fit = lm(y~X)

plot(X, y)
plot(fit$fitted.values, fit$residuals)

# 

ATT = read.table("ATT.txt", header = FALSE)
colnames(ATT) = c("FPC", "Work", "OS", "DMS", "Lang")

fit = lm(Work ~ FPC, data = ATT)
plot(fit$fitted.values, fit$residuals)

plot(ATT$FPC, fit$residuals)

plot(log(ATT$Work), log(ATT$FPC))


     
     
# bp test 

y = fit$residuals^2
x = ATT$FPC
bp.fit <- lm(y~x)
BP = summary(bp.fit)$r.squared*nrow(ATT)
1-pchisq(BP, 1)

bptest(fit)


# BP test 

library(faraway)
data(gala)

fit = lm(Species ~ Elevation + Adjacent, data = gala)
summary(fit)

# use the bptest function 

#install.packages("lmtest")
library(lmtest)

bptest(fit)

# if we want to calcualte the test statistic directly
y = fit$residuals^2
x1 = gala$Elevation
x2 = gala$Adjacent

bp.fit = lm(y ~ x1 + x2)
BP = summary(bp.fit)$r.squared*nrow(gala)
1-pchisq(BP, 2)


bp.fit = lm(fit$residuals^2 ~ gala$Elevation + gala$Adjacent)
BP = summary(bp.fit)$r.squared*nrow(gala)
1-pchisq(BP, 2)






# an example where bp test will not work
n = 500
p = 3

X = matrix(rnorm(n*p), n, p)

beta = rep(1, p)

y = 1 + X %*% beta + rnorm(n)*exp(X[,3]^2/6)

plot(y~X[,3])

fit = lm(y~X)

bptest(fit)

plot(fit$fitted.values, fit$residuals)

bptest(fit)

plot(X[,3], fit$residuals)
plot(X[,3], fit$residuals^2)



# white test 
# include all square terms and pairwise interactions

fit = lm(Species ~ Elevation + Adjacent, data = gala)
summary(fit)

white.fit = lm(fit$residuals^2 ~ gala$Elevation + gala$Adjacent + 
                                 I(gala$Elevation^2) + I(gala$Adjacent^2) + 
                                 gala$Elevation*gala$Adjacent)

white = summary(white.fit)$r.squared*nrow(gala)
1-pchisq(white, 5)



fit = lm(Species ~ Area + Elevation + Adjacent, data = gala)
summary(fit)

x1 = gala$Elevation
x2 = gala$Adjacent
x3 = gala$Area

white.fit = lm(fit$residuals^2 ~ x1 + x2 + x3 + 
                 I(x1^2) + I(x2^2) + I(x3^3) + 
                 x1*x2 + x2*x3 + x3*x1)

white = summary(white.fit)$r.squared*nrow(gala)
1-pchisq(white, 9)

plot(fit$fitted.values, fit$residuals)



x = rnorm(10000)
y = 1 + 2*x + rnorm(10000)
plot(x, y)


# an example where white test will not work
n = 500
p = 2

X = matrix(rnorm(n*p), n, p)

y = 1 + X[,1] + X[,2]^2 + rnorm(n)

fit = lm(y~X)

plot(fit$fitted.values, fit$residuals)

white.fit = lm(fit$residuals^2 ~ X[,1] + X[,2] + 
                 I(X[,1]^2) + I(X[,2]^2) + 
                 X[,1]*X[,2])

white = summary(white.fit)$r.squared*n
1-pchisq(white, 5)

plot(X[,2], fit$residuals)

# this is due to model mis-specification 

fit = lm(y~X[,1] + I(X[,2]^2))
plot(X[,2], fit$residuals)



x = rnorm(1000)
y = 1 + x^2 + rnorm(1000)
plot(x, y)
lm(y~x)
lm(y~x + I(x^2))
   g = seq(-3, 3, 0.01)
lines(g, 0.953 + -0.4312*g + 1.0414 *g^2, lwd = 2,col = "red"   )

abline(a = 2.03, b = -0.03, col = "red")

e2 = lm(y~x)$residuals^2

plot(x, e2)

summary(lm(e2 ~ x + I(x^2)))


bptest(lm(y~x))


# WLS 

# a simulation study under heteroscedasticity

n = 200
X = cbind(1, rnorm(n))
y = X %*% c(1, 1) + rnorm(n)*exp(X[,2])

plot(X[,2], y)

fit = lm(y~X-1)
betahat = fit$coef[2] # we know that the estimated coefficient should be around 1
sigma2 =  sum(fit$residuals^2) / (n-2)
betasd = sqrt(solve(t(X) %*% X)[2,2]*sigma2)

(betahat - 1)/betasd  # we know this one follows a t distribution if all assumptions are satisfied 

# lets try a simulation to see what happens if the error variance is not constant

betahat_dist = rep(NA, 10000)
for (i in 1:10000)
{
  y = X %*% c(1, 1) + rnorm(n)   #*exp(X[,2])
  fit = lm(y~X-1)
  betahat = fit$coef[2] # we know that the estimated coefficient should be around 1
  sigma2 =  sum(fit$residuals^2) / (n-2)
  betasd = sqrt(solve(t(X) %*% X)[2,2]*sigma2)
  betahat_dist[i] =  (betahat - 1)/betasd  # we know this one follows a t distribution if all assumptions are satisfied 
}

library(car)
qqPlot(betahat_dist, distribution="t", df = 198)




# WLS refitting 

# this is just one iteration of the refitting process
fit = lm(Species ~ Elevation + Adjacent, data = gala)
summary(fit)
qqPlot(fit)

# use the bp model to estimate the variance 

efit = lm(fit$residuals^2 ~ gala$Elevation + gala$Adjacent)
wlsfit = lm(Species ~ Elevation + Adjacent, data = gala, w = 1/efit$fitted^2)
summary(wlsfit)
qqPlot(wlsfit)



# the sandwich estimator

library(car)
vcbeta = hccm(fit, type = "hc0")
round(vcbeta, 4)

e = fit$residuals
X = cbind(1, gala$Elevation, gala$Adjacent)
sw = solve(t(X) %*% X) %*% t(X) %*% diag(e^2) %*% X %*% solve(t(X) %*% X)
round(sw, 4)

# with correction of bias 
vcbeta = hccm(fit, type = "hc1")
round(sw*nrow(gala)/(nrow(gala) - 3), 4)


