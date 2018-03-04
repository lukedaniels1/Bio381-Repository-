library(TeachingDemos)
library(ggplot2)
char2seed('espesso',set=FALSE)
char2seed('espresso')


##############################################################

# FUNCTION Creating Data 
# Calculates: The data frame
# Input: Mean, Standard, Deviation of Each column
remove(list=ls(all=TRUE))
#-------------------------------------------------------------

  myFunction <- function(nGroup = 2, nName1 = c("Low", "High"),
                          nSize1 = c(50,50), nMean1 =c(200,240)){
    ID <- 1:100
    resVar <- c(rnorm(n=nSize1[1], mean= nMean1[1],sd = 40),
                rnorm(n=nSize1[2], mean= nMean1[2],sd = 60))
    TGroup <- rep(nName1,nSize1) 
    ANOdata <- data.frame(ID, TGroup, resVar)
  return(ANOdata)
  }
#######################
myDF <- myFunction()

ANOmodel <- aov(myDF$resVar ~ myDF$TGroup, data = myDF)
  print(ANOmodel) 
  summary(ANOmodel) 
  
ggplot(myDF)
plot(myDF$TGroup, myDF$resVar)
  # basic ggplot of ANOVA data 
  
  
ggplot(data = myDF, aes(x=TGroup,y=resVar))
ANOVA <- ANOplot
ANOVA
  
  