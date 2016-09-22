#R Regression Modelling = Quiz 3

#Q1
data(mtcars)
fit <- lm(mpg~factor(cyl)+wt, data=mtcars)
summary(fit)$coef

#Q2
fit1 <- lm(mpg~factor(cyl), mtcars)
cbind(summary(fit)$coef[,1], c(summary(fit1)$coef[,1], NA))

#Q3
fit3 <- lm(mpg~factor(cyl)*wt, mtcars)
anova(fit, fit3, test = "Chisq")

#Q4
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)

#Q5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit5 <- lm(y~x)
hatvalues(fit5)

#Q6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit6 <- lm(y~x)
dfbetas(fit6)

#Q7
