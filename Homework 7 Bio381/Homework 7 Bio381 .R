## Bio 381 Homework 7 
## March 3 2018 
## Luke Daniels 

library(TeachingDemos)
library(ggplot2)
char2seed('espesso',set=FALSE)
char2seed('espresso')


##############################################################

# FUNCTION Creating Data 
# Calculates: The data frame
# Input: Mean, Standard, Deviation of Each column

#-------------------------------------------------------------

# Creating the Fucntion with Random Sample
myFunction3 <- function(mean1 = 2, mean2 = 9, n1= 150, n2 = 150, sd1=.7, 
                     sd2=.1){
  myDF <- data.frame(lowTemp=rnorm(mean=mean1, n=n1,
                                   sd=sd1), highTemp= rnorm(mean=mean2, n=n2, sd=sd2))
  return(myDF)
}

myDF <- myFunction3()
head(myDF)

###############################################################


# Using ggplot to regress


regPlot <- ggplot(data=myDF,aes(x= myDF$lowTemp, y= myDF$highTemp)) +
  geom_point() + 
  stat_smooth(method = aov, se=0.95)

Regression <- regPlot

Regression


# ---------- Trying the Regression With Different Parameters 

# Creating the Fucntion with Random Sample Number One 

myDF <- myFunction()

regPlot <- ggplot(data=myDF,aes(x= myDF$sepLength, y= myDF$sepWidth)) +
  geom_point() + 
  stat_smooth(method = lm, se=0.95)

Regression <- regPlot

Regression

# Creating the Function with Random Sample Number Two 

myDF <- myFunction()

regPlot <- ggplot(data=myDF,aes(x= myDF$sepLength, y= myDF$sepWidth)) +
  geom_point() + 
  stat_smooth(method = lm, se=0.95)

Regression <- regPlot

Regression

