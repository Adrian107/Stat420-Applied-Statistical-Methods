ATT = read.table("ATT.txt", header = FALSE)
colnames(ATT) = c("FPC", "Work", "OS", "DMS", "Lang")



# lets model the work hours using FPC

fit = lm(Work ~ FPC, data = ATT)
res = fit$residuals
yhat = fit$fitted.values
sigma = sqrt(sum(fit$residuals^2)/fit$df.residual)

# simple histogram 

h = hist(res, main = "residual density plot", xlab = "residuals", breaks = 20)

xgrid <- seq(min(res),max(res),length=100)
yden <- dnorm(xgrid,mean=0,sd=sigma)
yden <- yden*length(res)*diff(h$mids[1:2])
lines(xgrid, yden, col="blue", lwd=2)

x = rnorm(10000)
h = hist(x, breaks = 50)
xgrid <- seq(-3, 3, length=100)
yden <- dnorm(xgrid)*10000*diff(h$mids[1:2])
lines(xgrid, yden, col = "red")

# checking normality using qq plot

# first we get the percentiles associated with each observation
perct = (rank(res) - 0.5)/length(res)

# then we get the theoritical normal z value at each percentile
z = qnorm(perct)

# plot the observed residuals vs. the theoritical z value
# the residual values are standerdized to have mean = 0, sd = 1
plot(z, res/sigma, xlab = "theoritical value", ylab = "observed residuals")
abline(0, 1)


x = rnorm(10000)
perct = (rank(x) - 0.5)/length(x)
z = qnorm(perct)
plot(z, x)


# another "automatic" way to generate the line
qqnorm(res)
qqline(res)

text(z, res, labels = c(1:length(z)), pos = 1)
points(z[16], res[16], cex = 2, col="red")


# some default functions in the package "car", with a nice "confidence bound"

library(car)
qqPlot(fit, main="QQ Plot", col = "blue", pch = 19, cex = 1)


x = rnorm(1000)
qqPlot(x, main="QQ Plot", col = "blue", pch = 19, cex = 1)

# what the qqplot looks like under different distributions

qqPlot(rt(100, 2), main="QQ Plot", col = "blue", pch = 19, cex = 1)

qqPlot(rchisq(1000, 2), main="QQ Plot", col = "blue", pch = 19, cex = 1)

qqPlot(rexp(100), main="QQ Plot", col = "blue", pch = 19, cex = 1)

qqPlot(rcauchy(100), main="QQ Plot", col = "blue", pch = 19, cex = 1)

qqPlot(runif(200), main="QQ Plot", col = "blue", pch = 19, cex = 1)





# testing for normality 

# the Shapiro-Wilks test 

shapiro.test(res)

library(faraway)
data(gala)
colnames(gala)

fit = lm(Species ~ Area+ Elevation+Nearest + Scruz + Adjacent, data = gala)
shapiro.test(fit$residuals)

qqPlot(fit, main="QQ Plot", col = "blue", pch = 19, cex = 1)


# Kolmogorov-Smirnov Tests

ks.test(res, "pnorm")

# about the empericial distribution:
plot(ecdf(res))

plot(ecdf(rnorm(20)))
lines(seq(-3, 3, 0.1), pnorm(seq(-3, 3, 0.1)), col = "blue")

# the Adnderson-Darling tests

install.packages("nortest")
library(nortest)
ad.test(res)

# Loone & Gulledge correlation test 

lg.test <- function(x) {
  z <- qnorm((rank(x) - 0.375)/(length(x) + 0.25))
  c(cor(x, z), length(x))
}

lg.test(res)


cor(res, qnorm((rank(res) - 0.375)/(length(res) + 0.25)))

# a simulation study to approaximate the p-value of the correlation test 

scores = rep(NA, 10000)

for (i in 1:10000)
{
  x = rnorm(length(res))
  scores[i] = lg.test(x)[1]
}

lgcrit = quantile(scores, prob = 0.05)
hist(scores, xlim = c(0.95, 1), xlab = "correlations")
abline( v = lgcrit, col = "red")
abline( v = lg.test(res)[1], col = "blue")
