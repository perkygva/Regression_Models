#Regression Models
#Quiz 1

#Q1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
minu = sum(x*w)/sum(w)
final = sum(w*(x-minu)^2)
c(minu, final)
#alternative weighted.mean(x,w)

#Q2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y ~ 0+x)$coef

#Q3
data(mtcars)
lm(mpg~wt, mtcars)

#Q4
#sd(X) == 0.5*sd(Y)
#corr(X,Y) == .5
#slope == cor(X,Y)*sd(Y)/sd(X) == .5*1/.5

#Q5
1.5*0.4

#Q6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
(x-mean(x))/sd(x)

#Q7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y~x)

#Q9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

#Q10
#y outcome / x predictor -> Beta = cor(y, x) *sd(x)/sd(y)
#x outcome / y predictor -> Gamma = cor(x, y) * sd(y)/sd(x)
# Beta/Gamma =  sd(x)^2/sd(y)^2