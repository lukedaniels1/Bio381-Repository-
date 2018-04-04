library(TeachingDemos)
library(ggplot2)
char2seed('espesso',set=FALSE)
char2seed('espresso')
#------------------------------------Number 1

##############################################################

# FUNCTION Creating Data 
# Calculates: The data frame
# Input: Mean, Standard, Deviation of Each column
remove(list=ls(all=TRUE))
#-------------------------------------------------------------

  myFunction <- function(nGroup = 2, nName1 = c("Low", "High"),
                          nSize1 = c(50,50), nMean1 =c(1.5,2)){
    ID <- 1:100
    resVar <- c(rnorm(n=nSize1[1], mean= nMean1[1],sd = .5),
                rnorm(n=nSize1[2], mean= nMean1[2],sd = .75))
    TGroup <- rep(nName1,nSize1) 
    ANOdata <- data.frame(ID, TGroup, resVar)
  return(ANOdata)
  }
#############################################################
myDF <- myFunction()
myDF

#------ Summary stats Function

  myDF <- myFunction()
  
  AnoFunction <- function(data=myDF){
  ANOmodel <- aov(myDF$resVar ~ myDF$TGroup, data = myDF)
  ANOSummary <- summary(ANOmodel)
  return(ANOSummary)
  }

AnoFunction()

# 1.)      2.) 0.00721  3.) 0.000635  4.) 0.000124 5.) 0.0019  6.) 0.0267  7.)0.0011 

# 8.)0.000374   9.)0.00112  10.)0.0386  11.)0.00052  12.) 0.00093  13.) 0.049


#------

  
  
  
  
  
  
# ------- Ggplot Box Plot 
ANOplot <- function(data=myDF){
  plot <- ggplot(data = myDF, aes(x=myDF$TGroup,y=myDF$resVar, fill = myDF$TGroup))
  return(plot)
}
print(ANOplot)
  #---------------------------------------- Number 2 

  

#------------------------------------------------------------- Repeat Function 2 

myFunction2 <- function(nGroup = 2, nName1 = c("Low", "High"),
                       nSize1 = c(300,300), nMean1 =c(1.5,2.25)){
  ID <- 1:100
  resVar <- c(rnorm(n=nSize1[1], mean= nMean1[1],sd = 40),
              rnorm(n=nSize1[2], mean= nMean1[2],sd = 60))
  TGroup <- rep(nName1,nSize1) 
  ANOdata <- data.frame(ID, TGroup, resVar)
  return(ANOdata)
}
#############################################################
myDF <- myFunction2()

#---------------------- Summary Stats 
ANOmodel <- aov(myDF$resVar ~ myDF$TGroup, data = myDF)
print(ANOmodel) 
summary(ANOmodel) 

myDF <- myFunction2()
ANOmodel <- aov(myDF$resVar ~ myDF$TGroup, data = myDF)
print(ANOmodel) 
summary(ANOmodel) 


#----------------------- basic ggplot of ANOVA data 

ANOplot <- ggplot(data = myDF, aes(x=myDF$TGroup,y=myDF$resVar, fill = myDF$TGroup))+
  geom_boxplot(colour = "Black")+ 
  scale_y_continuous(name = "Metabolic Rate")+ 
  scale_x_discrete(name = "Temperature Treatment") +
  ggtitle("Metabolic Rates in High vs Low Temperature")+
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black"))
ANOplot
#---------------------------------------- Number 3




#------------------------------------------------------------- Repeat Function 3

myFunction <- function(nGroup = 2, nName1 = c("Low", "High"),
                       nSize1 = c(50,50), nMean1 =c(1.5,2.25)){
  ID <- 1:100
  resVar <- c(rnorm(n=nSize1[1], mean= nMean1[1],sd = 1),
              rnorm(n=nSize1[2], mean= nMean1[2],sd = 1))
  TGroup <- rep(nName1,nSize1) 
  ANOdata <- data.frame(ID, TGroup, resVar)
  return(ANOdata)
}
#############################################################
myDF <- myFunction()

#--------------------------Summary Stats 

ANOmodel <- aov(myDF$resVar ~ myDF$TGroup, data = myDF)
print(ANOmodel) 
summary(ANOmodel) 

#---------------------------Ggplot 

ANOplot <- ggplot(data = myDF, aes(x=myDF$TGroup,y=myDF$resVar))+
  geom_boxplot(fill = "Blue" , colour = "Black")+ 
  scale_y_continuous(name = "Metabolic Rate")+ 
  scale_x_discrete(name = "Temperature Treatment") +
  ggtitle("Metabolic Rates in High vs Low Temperature")+
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black"))
ANOplot

# basic ggplot of ANOVA data 

#---------------------------------------- Number 4


ANOplot <- function(data=myDF){
  PlotAno <- ggplot(data = myDF,
                    aes(x=myDF$TGroup,
                        y=myDF$resVar,
                        fill= myDF$TGroup))+
    geom_boxplot(colour = "Black")+
    scale_y_continuous(name = "Metabolic Rate")+ 
    scale_x_discrete(name = "Temperature Treatement")+ 
    ggtitle("Metabolic Rates in High vs Low Temperature")+
    theme_bw()
  return(PlotAno)
  }
  ANOplot(myDF)
  
  
  ggplot(data = myDF, aes(x=myDF$TGroup,y=myDF$resVar, fill = myDF$TGroup))+
  geom_boxplot(colour = "Black")+ 
  scale_y_continuous(name = "Metabolic Rate")+ 
  scale_x_discrete(name = "Temperature Treatment") +
  ggtitle("Metabolic Rates in High vs Low Temperature")+
  theme_bw()
ANOplot