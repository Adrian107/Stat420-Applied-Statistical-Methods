# analysis on the prostate cancer data

library(ElemStatLearn)
library(glmnet)
library(MASS)
library(lars)

# the prostate cancer data is trying to predict lpsa, with the first 8 covariates

dat   <- prostate
train <- subset(dat,  train, select = -train)
test  <- subset(dat, !train, select = -train)

train.x <- as.matrix(subset(train, select = -lpsa))
train.y <- as.matrix(train$lpsa)

lam = exp(seq(-10, 10, 0.1))
ridge1 <- lm.ridge(train.y ~ train.x, lambda = lam)

# the default selection criteria generalized cross validation error
select(ridge1)

# the best lambda based on GCV
lam[which.min(ridge1$GCV)]



# another popular package is the glmnet
# you don't need to specify the lambda
# make sure to use alpha = 0

ridge2 <- glmnet(train.x, train.y, alpha = 0)

# plot use L1 norm as x-axes
plot(ridge2, xlim = c(0, 2.5))
text(2.4, ridge2$beta[, ncol(ridge2$beta)], colnames(train.x), cex = 1.5)
abline(h = 0, lty = 3)




# how the Lasso penalty shrinks the parameter estimates
# suppose I have an intercept model: y = 1 + epsilon
n = 100
y = rnorm(n, 1, 1)

# then we can calculate the loss function for all candidate beta values

b = seq(-1, 3, 0.01)
LOSS = rep(NA, length(b))

for (k in 1:length(b))
  LOSS[k] = sum((y - b[k])^2)/n

plot(b, LOSS, type= "l", ylim =c(0, 5))

# now we add penalty function
Pen = 0.25*abs(b)

lines(b, Pen, col = "red")

# the sum of two functions
# the minimization point is not the original point, it shrinks towards 0

lines(b, Pen+LOSS, col = "blue")






# lasso fitting, which uses alpha = 1, its the default value.

lasso.fit = glmnet(train.x, train.y)

# plot use L1 norm as x-axes
plot(lasso.fit)

# lars
lars.fit = lars(train.x, train.y, type = "lar")

# a piecewise linear solution path
plot(lars.fit, xvar = "norm", breaks = FALSE)

# forward stepwise regression
stepwise.fit = lars(train.x, train.y, type = "stepwise")
plot(stepwise.fit, xvar = "norm", breaks = FALSE)




# to select the best lambda using cross validation

lasso.fit = cv.glmnet(train.x, train.y, nfolds = 10)

plot(lasso.fit)

# usually two tuning parameters are used
coef(lasso.fit, s = "lambda.min")
coef(lasso.fit, s = "lambda.1se")

# potential problem for lasso if variables are highly correlated

set.seed(1)
n = 30
# highly correlated variables
X = mvrnorm(n, c(0, 0), matrix(c(1,0.999, 0.999, 1), 2,2))
y = rnorm(n, mean=1 + X[,1] + X[,2])

# the parameter estimates
# lasso only picks up one of the variables, and puts all effects on that one

lasso.fit = glmnet(train.x, train.y)
plot(lasso.fit)

# a potential fix is to use elastic net, which is done by

elnet.fit = glmnet(train.x, train.y, alpha = 0.5)
plot(elnet.fit)










