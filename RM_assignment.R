
p_load(ggplot2, GGally, gridExtra, Leaps, corrplot)

data(mtcars)
head(mtcars)
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))

ggpairs(mtcars, lower = list(continuous = "smooth"))

g = ggplot(mtcars, aes(mpg))
h = g+ geom_histogram(aes(y=..density..),fill = "light blue", stat = "bin", binwidth= 3) + geom_density() 
fg = h + facet_wrap(~am)
           
b = ggplot(mtcars, aes(y=mpg, x=am)) + geom_boxplot()
 
auto <- mtcars[mtcars$am == "Automatic",]    
manual <- mtcars[mtcars$am == "Manual", ]

#Correlation
c <- cor(mtcars)
corrplot(c, type = "lower", title = "correlation plot of mtcars")
         
#Test for normality
t.test(mtcars$mpg)
t.test(mpg~am, mtcars)

           
#Regression Models
fitaut <- lm(mpg~am, mtcars)
fitall <- lm(mpg~., mtcars)
lower = fitaut
upper = fitall
fitstp <- step(fitaut, scope = list(lower=lower, upper=upper), direction = "forward")  

#Alternative using leaps
leaps = regsubsets(mpg~., mtcars)
plot(leaps, scale="adjr2")
#Residual Analysis
par(mfrow=c(2,2))
plot(fit)

#QQplot            
ggplot(mtcars, aes(sample = mpg)) + 
  stat_qq(geom = "point", aes(colour= am)) + ggtitle("QQPLOT Miles per gallon")

