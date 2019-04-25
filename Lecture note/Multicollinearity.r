n = 200

x1 = rnorm(n)
x2 = rnorm(n) # x1 and x2 are independent 
x3 = 1 + 2 * x1 + 3 * x2
y = 3 + x1 + x2 + x3 + rnorm(n)

mydata = data.frame(x1, x2, x3, y)

fit = lm(y~., data = mydata)
summary(fit)

# however, lm only removes the last linearly correlated variable, so if 

mydata = data.frame(x1, x3, x2, y)

fit = lm(y~., data = mydata)
summary(fit) # x2 will be removed


# exactly linearly dependency doest not appear that frequently. 
# in practice, we ofen see highly correlated varaibles 

set.seed(1)
x1 = rnorm(n)
x2 = rnorm(n) # x1 and x2 are independent 
x3 = 1 + 2 * x1 + 3 * x2 + rnorm(n, sd = 0.01)
y = 3 + x1 + x2 + x3 + rnorm(n)

mydata = data.frame(x1, x2, x3, y)

# is there anything wrong with the model fitting?

fit = lm(y~., data = mydata)
summary(fit)


# investigate the X'X matrix 

X = cbind(1, x1, x2, x3)

solve(t(X) %*% X) # why this is so large?

# perform a eigen decomposition to the matrix 

ee = eigen(t(X) %*% X)

# the inverse matrix is 
ee$vectors %*% diag(ee$values^(-1)) %*% t(ee$vectors)

# hence extreamly small eigen values lead to large (X'X)^-1, causing large variance of the beta estimations.


library(faraway)
data(seatpos)

# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(seatpos)

fit.full = lm(hipcenter~., data = seatpos)
summary(fit.full)

# again, we have very small eigen values in X'X
X = as.matrix(cbind("Intercept" = 1, seatpos[, -9]))
eigen(t(X) %*% X)$values

# reformulating the variance 

sigma2 = deviance(fit.full)/df.residual(fit.full)

Rj = summary(lm(HtShoes~., data = seatpos[, -9]))$r.squared

varj = sigma2/(1-Rj)/var(seatpos$HtShoes)/(nrow(seatpos)-1)

# this matches the sd of HtShoes in the lm fit (full model)

sqrt(varj)

# In general, the Variation Inflation Factor is a measure 
# of how much the variance is inflated.

vif(seatpos[, -9])

vif(seatpos[, -c(3, 4, 9)])

summary(lm(hipcenter~., data = seatpos[, -c(3, 4)]))

summary(lm(hipcenter~ Age + Weight + Ht, data = seatpos))

# ridge regression 

library(MASS)

lm.ridge(y~., data = mydata, lambda=1)

# be careful that the ridge regression will first scale the predictors to sd = 1, 
# and then apply the ridge techique 

ridge.fit = lm.ridge(hipcenter~., data = seatpos, lambda = seq(1, 100, 1))
plot(ridge.fit)
# this helps to select the best tuning parameter 
which.min(ridge.fit$GCV)

lm.ridge(hipcenter~., data = seatpos, lambda= 22)

