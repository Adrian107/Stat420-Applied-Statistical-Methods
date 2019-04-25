ATT = read.table("ATT.txt", header = FALSE)
colnames(ATT) = c("FPC", "Work", "OS", "DMS", "Lang")

# Again, lets model the work hours using FPC

fit = lm(Work ~ FPC, data = ATT)

plot(ATT$FPC, ATT$Work)
abline(fit$coefficients[1], fit$coefficients[2], col = "red", lwd = 3)

# Remove one subject: 58

points(ATT$FPC[58], ATT$Work[58], cex = 3, col = "blue")
ATTi = ATT[-58,]
fiti = lm(Work ~ FPC, data = ATTi)

abline(fiti$coefficients[1], fiti$coefficients[2], col = "blue", lty = 2, lwd = 3)

# Cook's Di (first appraoch)
X = cbind(1, ATT$FPC)
diff = X %*% (fit$coefficients - fiti$coefficients)
sigma2 = sum(fit$residuals^2)/(nrow(ATT)-2)
t(diff) %*% diff / 2 / sigma2

# Cook's Di (second appraoch), this is the default approach

H = X %*% solve(t(X) %*% X) %*% t(X)
h58 = H[58, 58]
(h58 / (1 - h58)^2)* fit$residuals[58]^2 / 2/ sigma2

# validate using build-in functions

hat(X, intercept =FALSE) # this one gives you the diagnal elements in H
cooks.distance(fit) # this one gives you the D_i
plot(ATT$FPC, cooks.distance(fit))

points(ATT$FPC[24], cooks.distance(fit)[24], cex = 3, col = "red")



text(ATT$FPC, cooks.distance(fit), c(1:nrow(ATT)), pos = 2)

hat(X, intercept =FALSE)[58]
cooks.distance(fit)[58]
which.max(cooks.distance(fit))
points(ATT$FPC[24], ATT$Work[24], cex = 3, col = "blue")
fit$residuals[c(24, 58)]

# DFFITS 

diff[58] / sqrt((H[58, 58] * sum(fiti$residuals^2)/(nrow(ATTi)-2) ))


# this function provides a summary of potential influencial points 
summary(influence.measures(fit))


