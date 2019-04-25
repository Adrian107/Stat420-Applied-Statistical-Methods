initech = read.csv("initech.csv")

# Fit a simple linear regression to the original data

fit = lm(salary ~ years, data = initech)
summary(fit)

# fitted line 
plot(salary ~ years, data = initech, pch =19)
abline(fit, col = "red", lwd = 2)

# residuals
plot(fit$fitted.values, fit$residuals, pch = 19, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red", lwd = 2)


# log transformation

logfit = lm(log(salary) ~ years, data = initech)
summary(logfit)
plot(log(salary) ~ years, data = initech, pch =19)
abline(logfit, col = "red", lwd = 2)


plot(salary ~ years, data = initech, pch =19)
abline(fit, col = "darkorange", lty = 2, lwd = 2)
lines(sort(initech$years), exp(logfit$coef[1] + logfit$coef[2] * sort(initech$years)), 
       type = "l", col = "red", lwd = 2)

# the residuals look better
plot(logfit$fitted.values, logfit$residuals, pch = 19, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red", lwd = 2)



# box-cox transformation

library(MASS)
bc = boxcox(fit, plotit = T)
# the optimal lambda close to 0 suggests a log transformation
bc$x[which.max(bc$y)]

# calculate your own box-cox transformation

y = initech$salary
x = initech$years
n = nrow(initech)

y = y/exp(mean(log(y))) # now sum(log(y)) = 0, so we dont need the second term

lambda = -0.5 # this is the 60th entry in the bc output

gy = (y^lambda - 1) / lambda

# the second term is not needed since its 0
# the first term is not in the original scale, but since all the calculations will be 
# based on the same y, we are fine

LL = -n/2*log(sum(lm(gy ~ x)$residuals^2))

# compare this LL to the boxcox function

bc$y[60]



# tranforming the X variables 

ATT = read.table("ATT.txt", header = FALSE)
colnames(ATT) = c("FPC", "Work", "OS", "DMS", "Lang")


plot(ATT$FPC, ATT$Work)
plot(log(ATT$FPC), log(ATT$Work)

par(mfrow=c(2,2))

plot(lm(Work ~ FPC, data = ATT))
plot(lm(log(Work) ~ log(FPC), data = ATT))

# marketing data 

marketing = read.csv("marketing.csv")
plot(marketing$advert, marketing$sales)

# if I want to include second order terms

fit = lm(sales~ poly(advert, 2), data= marketing)
plot(fit)

# higher order terms seems to be better 
fit = lm(sales~ poly(advert, 3), data= marketing)
plot(fit)


# the polym function creates interactions and higher order terms

fit = lm(Work~ polym(FPC, DMS, degree=2), data= ATT)






