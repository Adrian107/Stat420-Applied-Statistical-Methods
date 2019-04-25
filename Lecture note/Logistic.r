library(MASS)

# can we use linear regression on classification?
# what is the interpretation of the fitting line?

x=seq(0, 1, 0.01)
y=rbinom(length(x), 1, 0.5 + 0.5*sign(x-0.5)*abs(2*x-1)^0.2)
plot(x, y, ylim = c(-0.5, 1.5), col = ifelse(y== 1, "red", "blue"))
lines(x, lm(y~x)$fitted.values, lwd = 2)
lines(x, glm(as.factor(y)~x, family = binomial)$fitted.values, lty = 2, lwd = 2)



# a logistic regression example

library(ElemStatLearn)
data(SAheart)

dim(SAheart)
colnames(SAheart)

# chd (coronary heart disease) is the outcome
# model the probability using age and ldl

heart.fit = glm(chd~ ldl + age, data=SAheart, family=binomial)

plot(SAheart$age, SAheart$ldl, 
     col = ifelse(SAheart$chd==1, "red", "blue"),
     pch = 19, xlab = "Age", ylab = "LDL")

# the seperating line
c = -heart.fit$coef/heart.fit$coef[2]
abline(c[1], c[3], lwd = 2)

# prediction?
predict(heart.fit, data.frame(age = 50, ldl = 6))
a = exp(heart.fit$coef %*% c(1, 6, 50))
a / (1+a)




