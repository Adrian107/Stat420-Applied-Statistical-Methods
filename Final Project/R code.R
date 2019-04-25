#part one
train = read.csv(file="C:/Users/Hans/Desktop/training.csv", header=TRUE)
#Log transformation
fit9 = lm(log(health)~PCDH12+DLG5+BC038559+SHISA5+AF161342+CARKD+F2R+PHKG1+CDCP1+PLEKHM1+SMC2+PSMB6+BX440400+A_24_P936373+PPAN+BC007917+C14orf143+LOC440104+THC2578957+ANKIB1,data = train)
summary(fit9)$r.squared

#Box-cox transformation

library(MASS)
bc = boxcox(health~PCDH12+DLG5+BC038559+SHISA5+AF161342+CARKD+F2R+PHKG1+CDCP1+PLEKHM1+SMC2+PSMB6+BX440400+A_24_P936373+PPAN+BC007917+C14orf143+LOC440104+THC2578957+ANKIB1,data = train,plotit = T)
lambda = bc$x[which.max(bc$y)]
health_1 = ((train$health^lambda)-1)/lambda
fit = lm((health_1)~PCDH12+DLG5+BC038559+SHISA5+AF161342+CARKD+F2R+PHKG1+CDCP1+PLEKHM1+SMC2+PSMB6+BX440400+A_24_P936373+PPAN+BC007917+C14orf143+LOC440104+THC2578957+ANKIB1,data = train)
summary(fit)$r.squared

#AIC & BIC
AIC(fit)
BIC(fit)
AIC(fit9)
BIC(fit9)

# Based on the r-square for both y-transformation, even though box-cox has slight lower r-square than log transformation, 
# but box-cox's AIC and BIC is much smaller than log's, which means that box-cox has better goodness-of-fit, thus, we
# choose box-cox transformation to do the diagnostics.

#Residuals and Normality check
res = fit$residuals
plot(res)
his = hist(res, main = "residual plot")
shapiro.test(res)
xgrid =seq(min(res) ,max(res) ,length=100)
yden =dnorm( xgrid ,mean=0,sd=0.2083) 
yden = yden*length (res)* diff (his$mids[1:2])
lines ( xgrid , yden, col="blue", lwd=2)

# Cook's Distance & Outliers

plot(cooks.distance(fit))
text(cooks.distance(fit))


#Remove point 11
train1=train[c(-11),]
fit1 = lm(log(health)~PCDH12+DLG5+BC038559+SHISA5+AF161342+CARKD+F2R+PHKG1+CDCP1+PLEKHM1+SMC2+PSMB6+BX440400+A_24_P936373+PPAN+BC007917+C14orf143+LOC440104+THC2578957+ANKIB1,data = train1)
summary(fit1)
res1 = fit1$residuals
his1 = hist(res1, main = "residual plot")
shapiro.test(res1)

sigma1 = sqrt(sum(fit1$residuals^2)/fit1$df.residual)
perct1 = (rank(res1) - 0.5)/length(res1)
z1 = qnorm(perct1)
plot(z1, res1/sigma1, xlab = "theoritical value", ylab = "observed residuals")
text(z1, res1/sigma1, c(1:length(res),pos=3))

#Remove point 15
train2=train1[c(-15),]
fit2 = lm(log(health)~PCDH12+DLG5+BC038559+SHISA5+AF161342+CARKD+F2R+PHKG1+CDCP1+PLEKHM1+SMC2+PSMB6+BX440400+A_24_P936373+PPAN+BC007917+C14orf143+LOC440104+THC2578957+ANKIB1,data = train2)
summary(fit2)
res2 = fit2$residuals
his2 = hist(res2, main = "residual plot")
shapiro.test(train$CDCP1)

sigma2 = sqrt(sum(fit2$residuals^2)/fit2$df.residual)
perct2 = (rank(res2) - 0.5)/length(res2)
z2 = qnorm(perct2)
plot(z2, res2/sigma2, xlab = "theoritical value", ylab = "observed residuals")
text(z2, res2/sigma2, c(1:length(res2),pos=3))

# Cook's Distance & Outliers


sort(cooks.distance(fit),decreasing = TRUE)[1:3]
sort(cooks.distance(fit1),decreasing = TRUE)[1:3]
sort(cooks.distance(fit2),decreasing = TRUE)[1:6]

plot(cooks.distance(fit))
text(cooks.distance(fit))
plot(cooks.distance(fit1))
text(cooks.distance(fit1))
plot(cooks.distance(fit2))
text(cooks.distance(fit2))
plot(res2)
text(res2)


# # After removing outliers

# Final model # Using Box-cox




#Histogram plot
hist(res)
hist(res1)
hist(res2)

#BP test
library(lmtest)
bptest(fit2)

# AIC & BIC before X transformation and model selection
AIC(fit)
BIC(fit)
AIC(fit1)
BIC(fit1)
AIC(fit2)
BIC(fit2)
# Decreasing value means better goodness-of-fit






# PART TWO

library(PerformanceAnalytics)
chart.Correlation(train)


# X Transformation

#log
PCDH12_1 = log(train$PCDH12)
AF161342_1 = log(train$AF161342+1)
CARKD_1 = log(train$CARKD)
SMC2_1 = log(train$SMC2)
A_24_P936373_1 = log(train$A_24_P936373)
C14orf143_1 = log(train$C14orf143)

fit3 = lm(health~PCDH12_1+DLG5+BC038559+SHISA5+AF161342_1+CARKD_1+F2R+PHKG1+CDCP1+PLEKHM1+SMC2_1+PSMB6+BX440400+A_24_P936373_1+PPAN+BC007917+C14orf143_1+LOC440104+THC2578957+ANKIB1,data = train)
summary(fit3)$r.squared
plot(train$health, cooks.distance(fit3))
text(cooks.distance(fit3))

sort(cooks.distance(fit3),decreasing = TRUE)[1:3]
# Removing point 11
train4=train[c(-11),]
PCDH12_1 = log(train4$PCDH12)
AF161342_1 = log(train4$AF161342+1)
CARKD_1 = log(train4$CARKD)
SMC2_1 = log(train4$SMC2)
A_24_P936373_1 = log(train4$A_24_P936373)
C14orf143_1 = log(train4$C14orf143)

fit4 = lm(health~PCDH12_1+DLG5+BC038559+SHISA5+AF161342_1+CARKD_1+F2R+PHKG1+CDCP1+PLEKHM1+SMC2_1+PSMB6+BX440400+A_24_P936373_1+PPAN+BC007917+C14orf143_1+LOC440104+THC2578957+ANKIB1,data = train4)
summary(fit4)$r.squared



sort(cooks.distance(fit4),decreasing = TRUE)[1:3]
# Removing point 15
train5 = train4[c(-15),]
PCDH12_1 = log(train5$PCDH12)
AF161342_1 = log(train5$AF161342+1)
CARKD_1 = log(train5$CARKD)
SMC2_1 = log(train5$SMC2)
A_24_P936373_1 = log(train5$A_24_P936373)
C14orf143_1 = log(train5$C14orf143)

fit5 = lm(health~PCDH12_1+DLG5+BC038559+SHISA5+AF161342_1+CARKD_1+F2R+PHKG1+CDCP1+PLEKHM1+SMC2_1+PSMB6+BX440400+A_24_P936373_1+PPAN+BC007917+C14orf143_1+LOC440104+THC2578957+ANKIB1,data = train5)
summary(fit5)$r.squared




sort(cooks.distance(fit5),decreasing = TRUE)[1:3]
# Removing point 252
train6 = train4[c(-252),]
PCDH12_1 = log(train6$PCDH12)
AF161342_1 = log(train6$AF161342+1)
CARKD_1 = log(train6$CARKD)
SMC2_1 = log(train6$SMC2)
A_24_P936373_1 = log(train6$A_24_P936373)
C14orf143_1 = log(train6$C14orf143)

fit6 = lm(health~PCDH12_1+DLG5+BC038559+SHISA5+AF161342_1+CARKD_1+F2R+PHKG1+CDCP1+PLEKHM1+SMC2_1+PSMB6+BX440400+A_24_P936373_1+PPAN+BC007917+C14orf143_1+LOC440104+THC2578957+ANKIB1,data = train6)
summary(fit6)$r.squared


sort(cooks.distance(fit6),decreasing = TRUE)[1:3]
# Removing point 15
train7 = train6[c(-15),]
PCDH12_1 = log(train7$PCDH12)
AF161342_1 = log(train7$AF161342+1)
CARKD_1 = log(train7$CARKD)
SMC2_1 = log(train7$SMC2)
A_24_P936373_1 = log(train7$A_24_P936373)
C14orf143_1 = log(train7$C14orf143)

fit7 = lm(health~PCDH12_1+DLG5+BC038559+SHISA5+AF161342_1+CARKD_1+F2R+PHKG1+CDCP1+PLEKHM1+SMC2_1+PSMB6+BX440400+A_24_P936373_1+PPAN+BC007917+C14orf143_1+LOC440104+THC2578957+ANKIB1,data = train7)
summary(fit7)$r.squared




sort(cooks.distance(fit7),decreasing = TRUE)[1:3]
# Removing point 218
train8 = train7[c(-216),]
PCDH12_1 = log(train8$PCDH12)
AF161342_1 = log(train8$AF161342+1)
CARKD_1 = log(train8$CARKD)
SMC2_1 = log(train8$SMC2)
A_24_P936373_1 = log(train8$A_24_P936373)
C14orf143_1 = log(train8$C14orf143)

fit8 = lm(health~PCDH12_1+DLG5+BC038559+SHISA5+AF161342_1+CARKD_1+F2R+PHKG1+CDCP1+PLEKHM1+SMC2_1+PSMB6+BX440400+A_24_P936373_1+PPAN+BC007917+C14orf143_1+LOC440104+THC2578957+ANKIB1,data = train8)
summary(fit8)$r.squared


sort(cooks.distance(fit8),decreasing = TRUE)[1:6]
plot(cooks.distance(fit8))
# Upon here, we would like to conclude that all of the outliers are removed, since the 
# largest six cook's distance is very close and could seen in the graph. 



# AIC & BIC Between log & box-cox
AIC(fit3)
BIC(fit3)
AIC(fit8)
BIC(fit8)

# Stepwise
step(fit8,direction = "both")
fit10 = lm(health ~ PCDH12_1 + DLG5 + SHISA5 + AF161342_1 + 
             CARKD_1 + F2R + PHKG1 + PLEKHM1 + SMC2_1 + C14orf143_1 + 
             LOC440104 + THC2578957, data = train8)


# Best subset
# install.packages("leaps")
library("leaps")
health_1 = log(train8$health)
PCDH12_1 = log(train8$PCDH12)
AF161342_1 = log(train8$AF161342+1)
CARKD_1 = log(train8$CARKD)
SMC2_1 = log(train8$SMC2)
A_24_P936373_1 = log(train8$A_24_P936373)
C14orf143_1 = log(train8$C14orf143)
RSSleaps=regsubsets(health~PCDH12_1+DLG5+BC038559+SHISA5+AF161342_1+CARKD_1+F2R+PHKG1+CDCP1+PLEKHM1+SMC2_1+PSMB6+BX440400+A_24_P936373_1+PPAN+BC007917+C14orf143_1+LOC440104+THC2578957+ANKIB1,data = train8, nvmax = 20)
summary(RSSleaps, matrix=T)
RSSleaps=regsubsets(train8[,3:22],train8[,2],nvmax = 20)
sumleaps=summary(RSSleaps,matrix=T)
sumleaps$which
inrange = function(x) { (x - min(x)) / (max(x) - min(x)) }

sumleaps = summary(RSSleaps,matrix=T)
msize = apply(sumleaps$which,1,sum)
n=nrow(train8)
p=nrow(train8)
Cp = sumleaps$rss/(summary(fit8)$sigma^2) + 2*msize - n
AIC = n*log(sumleaps$rss/n) + 2*msize
BIC = n*log(sumleaps$rss/n) + msize*log(n)
Cp1=inrange(Cp)
BIC1 = inrange(BIC)
AIC1 = inrange(AIC)
plot(range(msize), c(0, 1.1), type="n", xlab="Model Size (with Intercept)", ylab="Model Selection Criteria")
points(msize, Cp1, col="red", type="b")
points(msize, AIC1, col="blue", type="b")
points(msize, BIC1, col="black", type="b")
legend("topright", lty=rep(1,3), col=c("red", "blue", "black"), cex = 2, legend=c("Cp", "AIC", "BIC"))
AIC
BIC
Cp

# By implementing both stepwise and best subset method, we get the same result, wich keep 
# PCDH12+DLG5+SHISA5+AF161342+CARKD_1+F2R+PHKG1+PLEKHM1+SMC2_1+C14orf143+LOC440104_1+THC2578957
PCDH12_1 = log(train8$PCDH12)
AF161342_1 = log(train8$AF161342+1)
CARKD_1 = log(train8$CARKD)
SMC2_1 = log(train8$SMC2)
A_24_P936373_1 = log(train8$A_24_P936373)
C14orf143_1 = log(train8$C14orf143)
fit11 = lm(health ~ PCDH12_1 + DLG5 + SHISA5 + AF161342_1 + 
             CARKD_1 + F2R + PHKG1 + PLEKHM1 + SMC2_1 + C14orf143_1 + 
             LOC440104 + THC2578957, data = train8)
summary(fit11)

# so the final model would be 
# health = -3.8045+ PCDH12_1 + DLG5 + SHISA5 + AF161342_1 + 
# CARKD_1 + F2R + PHKG1 + PLEKHM1 + SMC2_1 + C14orf143_1 + 
# LOC440104 + THC2578957
fitted(fit11)
write.csv(fitted(fit11),file = "fitted value")