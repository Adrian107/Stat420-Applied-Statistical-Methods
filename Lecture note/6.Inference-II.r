

# the gala data 

library(faraway)
data(gala)

fit = lm(Species~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
summary(fit)

sigma2 = deviance(fit) / df.residual(fit)

X = as.matrix(cbind(1, gala[, 3:7]))
beta_sd = sqrt(diag(solve(t(X) %*% X)*sigma2))

round(solve(t(X) %*% X)*sigma2, 4)

round(sqrt(diag(solve(t(X) %*% X)*sigma2)), 6)


# test the parameters Scruz and Adjacent jointly using F test
# we already have the full model SSE

SSEF = deviance(fit)

# now fit the restricted model 

fit2 = lm(Species~ Area + Elevation + Nearest, data = gala)

SSER = deviance(fit2)

# calculate the F test statistic 

Fstat = ((SSER - SSEF) / 2) / (SSEF / df.residual(fit))

1 - pf(Fstat, 2, df.residual(fit))


# full model 
SSEF = deviance(fit)
fit3 =lm(Species~ Area + Elevation + Nearest + Adjacent, data = gala)
SSER = deviance(fit3)

((SSER - SSEF) / 1) / (SSEF / df.residual(fit))



# another way to derive the F distribution from the distribution of beta hat:

A = cbind(c(0,0,0,0,1,0), c(0,0,0,0,0,1))

# the SSER - SSEF

num = fit$coefficients %*% A %*% solve(t(A) %*% solve(t(X) %*% X) %*% A) %*% t(A) %*% fit$coefficients 

dem = deviance(fit)

num / dem *(24)/2

# testing for linear constrains (beta4 = beta5)
# the linear combination matrix 
A = c(0, 0, 0, 0, 1, 1)
c = -1 

# the variance of this linear combination:

VA = t(A) %*% solve(t(X) %*% X) %*% A

sigma2 = deviance(fit) / df.residual(fit)

tstat = (t(A) %*% fit$coefficients - c)/ sqrt(VA * sigma2)

2*(1- pt(abs(tstat), df.residual(fit)))



# Confidence Ellipses for beta4 and beta5
library(car)
library(Cairo)
CairoPDF("Ellipse.pdf", 6, 6, bg="transparent")
confidenceEllipse(fit, which.coef = c(5, 6),
                  levels = 0.95, xlim = c(-1, 0.4), ylim = c(-0.15, 0.025))
# the test of beta4 = beta5 = 0
points(0, 0, pch = 16, cex = 2, col = "green")

# the test of beta4 - beta5 = 0
abline(a = 0, b = 1, col = "blue")
dev.off()


# Confidence Intervels for new subject

xnew = data.frame(Area = 260, Elevation = 360, Nearest = 10, Scruz = 60, Adjacent = 260)
# for mu_new
predict.lm(fit, xnew, interval = c("confidence"), level = 0.90)
# for Y_new
predict.lm(fit, xnew, interval = c("prediction"), level = 0.90)



