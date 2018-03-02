

library(TeachingDemos)
library(ggplot2)
char2seed('espesso',set=FALSE)
char2seed('espresso')



# Creating the Fucntion with Random Sample
myFunction <- function(mean1 = 2, mean2 = 9, n1= 150, n2 = 150, sd1=.7, 
                     sd2=.1){
  myDF <- data.frame(sepLength=rnorm(mean=mean1, n=n1,
                                   sd=sd1), sepWidth= rnorm(mean=mean2, n=n2, sd=sd2))
  return(myDF)
}

myDF <- myFunction()
head(myDF)

# Creating the Regression
regIris <- lm(myDF$sepLength ~ myDF$sepWidth, data= myDF)
print(regIris)
str(regIris) 
head(regIris$residuals)


# Using ggplot to regress


regPlot <- ggplot(data=myDF,aes(x= myDF$sepLength, y= myDF$sepWidth)) +
  geom_point() + 
  stat_smooth(method = lm, se=0.95)

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

