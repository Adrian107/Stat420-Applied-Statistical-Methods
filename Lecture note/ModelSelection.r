# generate n observations with p variables 
# they are all independent 
set.seed(3)

n = 100
p = 95

X = matrix(rnorm(n*p), n, p)
y = rnorm(n)

mydata = data.frame(cbind(X, y))

summary(lm(y~., data = mydata))

# the R sqaured can be very large although nothing is related to Y.

summary(lm(y~., data = mydata))$r.squared
summary(lm(y~., data = mydata))$adj.r.squared

# the gala data 

library(faraway)
data(gala)

fit = lm(Species~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
summary(fit)$r.squared
summary(fit)$adj.r.squared

library(PerformanceAnalytics)
chart.Correlation(gala)

# install.packages("wle")
library(wle)
mle.cp(fit)

mle.cp(fit)$cp
plot(mle.cp(fit))


fit1 = lm(Species~ Area + Scruz , data = gala)
sum(fit1$residuals^2) + 2 * sum(fit$residuals^2)/(nrow(gala)- 6)*3



# AIC and BIC 
n = nrow(gala)
p = 6

?AIC
AIC(fit, k = 2) # a build-in function for calculating AIC using -2log likelihood

# note that there is another 2 in this definition 
# because it count the variance estimation as one more parameter.

fit 
fit2 = lm(Species~ Area + Scruz, data = gala)
AIC(fit, k = 2)
AIC(fit2, k = 2)

    
n*log(sum(residuals(fit)^2/n)) + n + n*log(2*pi) + 2 + 2*p


# there are some other build-in functions 

?extractAIC
extractAIC(fit) # AIC for the full model
RSS = sum(residuals(fit)^2)
n*log(RSS/n) + 2*p

# so the BIC for the full model is 
extractAIC(fit, k = log(n))
n*log(RSS/n) + log(n)*p



# the naive stepwise regression: use alpha = 0.15

summary(fit)

# remove Nearest 
fit1 = update(fit, . ~ . - Nearest)
summary(fit1)

# remove Area
fit1 = update(fit, . ~ . - Area - Nearest)
summary(fit1)

# remove Scruz
fit1 = update(fit, . ~ . - Area - Nearest - Scruz)
summary(fit1)

# compare the two
anova(fit1, fit)
# the reduced model is not significantly different from the full model



# use the AIC as the selection criterion
# forward and backward 
step(fit, direction="backward")

step(fit, direction="both") 
step(fit, direction="backward", k = log(n))
step(lm(Species~1, data=gala), scope=list(upper=fit, lower=~1), direction="forward")







# best subset selection 
# install.packages("leaps")
library(leaps)

# performs an exhaustive search over models, and gives back the best model 

RSSleaps=regsubsets(gala[, 3:7], gala[,1], nvmax=13)
summary(RSSleaps, matrix=T)

RSSleaps=regsubsets(gala[, 3:7], gala[,1], nvmax=5)
summary(RSSleaps,matrix=T)


# select the best model across all model sizes
sumleaps = summary(RSSleaps,matrix=T)
sumleaps$which

msize=apply(sumleaps$which,1,sum)
n=dim(gala)[1]
p=dim(gala)[2]
Cp = sumleaps$rss/(summary(fit)$sigma^2) + 2*msize - n;
AIC = n*log(sumleaps$rss/n) + 2*msize;
BIC = n*log(sumleaps$rss/n) + msize*log(n);

cbind(Cp, sumleaps$cp)
cbind(BIC, sumleaps$bic)  # It seems regsubsets uses a formula for BIC different from the one we used. 
BIC-sumleaps$bic  # But the two just differ by a constant, so won't affect the model selection result. 
n*log(sum((gala[,1] - mean(gala[,1]))^2/n)) # the difference is the score of an intercept model

# a plot that puts all measurements into the same plot
# Rescale Cp, AIC, BIC to (0,1).
inrange <- function(x) { (x - min(x)) / (max(x) - min(x)) }

Cp = sumleaps$cp; Cp = inrange(Cp);
BIC = sumleaps$bic; BIC = inrange(BIC);
AIC = n*log(sumleaps$rss/n) + 2*msize; AIC = inrange(AIC);


plot(range(msize), c(0, 1.1), type="n", xlab="Model Size (with Intercept)", ylab="Model Selection Criteria")
points(msize, Cp, col="red", type="b")
points(msize, AIC, col="blue", type="b")
points(msize, BIC, col="black", type="b")
legend("topright", lty=rep(1,3), col=c("red", "blue", "black"), cex = 2, legend=c("Cp", "AIC", "BIC"))




