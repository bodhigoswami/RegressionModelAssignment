require(datasets); require(ggplot2)

data("mtcars")
mtcars$am<-as.factor(mtcars$am)

#Plot Showing Higher MPG for Manual Cars over Automatic Cars
g = ggplot(mtcars, aes( x = as.factor(am), y = mpg))
g = g + geom_boxplot(aes(fill=as.factor(am)), aplha = 0.35)
g = g + geom_point(size =4, col = "purple", alpha = 0.75)
g = g + xlab("Transmission Type (0 for Automatic, 1 for Manual") +
            ylab("Miles per Gallon (MPG)") + ggtitle("Mpg v/s Transmission")
g = g + theme(plot.title = element_text(hjust='0.5'))
g = g + labs(fill = "Transmission")
g

#Quantilfying all variables with mpg
fit<-lm(mpg~. -1,data=mtcars)
summary(fit) $coef

#Quantifying Esimate of 7.245 mpg more on an average for Manual Transmission
lm.fit<-lm(mpg~as.factor(am), data=mtcars)
summary(lm.fit)$coef

#Qunatifying Estimate of 7.245 with Generalized Linear Model
glm.fit<-glm(mpg~as.factor(am), data=mtcars)

#Residual Plot


#Uncertainty in conclusions

#Executive Summary
