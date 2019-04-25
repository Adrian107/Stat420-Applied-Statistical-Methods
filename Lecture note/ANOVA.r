library(faraway)

data(coagulation)

# box plot for visualization

boxplot(coag ~ diet, data = coagulation)
stripchart(coag ~ diet, vertical = TRUE, data = coagulation, add = TRUE, cex = 2, pch = 20, col = 'blue', method = "jitter")

newcoag = data.frame(cbind("coag" = coagulation$coag,"diet"= as.numeric(coagulation$diet)))

summary(lm(coag ~ as.factor(diet), data = newcoag))
summary(lm(coag ~ -1 + as.factor(diet), data = newcoag))



class(coagulation$diet)


# One factor model: you can either use ANOVA, or simply fit a regression:

summary(aov(coag ~ diet, data = coagulation))

summary(lm(coag ~ diet, data = coagulation ))


# two-way ANOVA

Time = c(12, 2, 8, 1, 7, 20, 14, 17, 12, 17, 13, 7, 13, 8, 14, 11, 5, 10, 3, 6)
A = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4)
B = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5)

fit = lm(Time ~ A + B) # this is wrong!!!!

fit = lm(Time ~ factor(A) + factor(B))

summary(aov(fit))

# two-way ANOVA

data(broccoli)
fit = lm(wt ~ factor(grower), data = broccoli)
fit = lm(wt ~ factor(box), data = broccoli)         
fit = lm(wt ~ factor(grower) + factor(box), data = broccoli)
summary(aov(fit))

fit = lm(wt ~ factor(grower)*factor(box), data = broccoli)
summary(aov(fit))



library(ggplot2)
ggplot(aes(y = wt, x = grower, fill = box), data = broccoli) + geom_boxplot()

fit = lm(wt ~ factor(grower)*factor(box), data = broccoli)
summary(aov(fit))


# 
data(butterfat)

fit = lm(Butterfat ~ factor(Breed), data = butterfat)
fit = lm(Butterfat ~ factor(Age), data = butterfat)         
fit = lm(Butterfat ~ factor(Breed) + factor(Age), data = butterfat)
fit = lm(Butterfat ~ factor(Breed)*factor(Age), data = butterfat)
summary(aov(fit))

library(ggplot2)
ggplot(aes(y = Butterfat, x = Age, fill = Breed), data = butterfat) + geom_boxplot()

# ANCOVA

data(diabetes)

plot(diabetes$age, diabetes$chol, col = ifelse(diabetes$gender == "male", "blue", "red"), pch = 19)
abline(lm(chol ~ age, subset = (gender == "male"), diabetes), col = "blue", lwd = 2)
abline(lm(chol ~ age, subset = (gender == "female"), diabetes), col = "red", lwd = 2)
legend("topright", c("male", "female"), col = c("blue", "red"), pch = 19, lty = 1, cex = 1.5)


fit = lm(chol ~ age, diabetes)
summary(aov(fit))

fit = lm(chol ~ age + gender, diabetes) # why I dont need to specify "factor(gender)"?
summary(aov(fit))

class(diabetes$gender)

fit = lm(chol ~ age*gender, diabetes)
summary(aov(fit))


