# Equation 1
# Y = B1*X1 + B2*X2 +B3*X3 + B4*X4 + B5*X5 + B6*X3^2 + B7*X1^2 + B8 * X5^2

# Equation 2: Mutation freequency:  qt =1−p0e−ut

##########################################

# FUNCTION: Equation 
# Calculates the change in allelic frequency  
# input: p0 = Initial Allelic Frequency of A
#      : e = Constat 
#      : u = Mutation rate
#      : t = time in generations
# output: q =  change in allelic frequency  
# --------------------------------

MutationChange <- function(p0=0.90,
                           u=10^-03,
                           t=1:10^5){ 
  q <- (1- (p0*exp(-u*t)))
  return(q)
} 

head(MutationChange())
View(MutationChange())



##########################################
MutationChangePlot <- function(p0=0.90,
                               u=10^-03,
                               t=1:10^3) {
  plot(x = t, y = MutationChange(p0,u,t),
       type = "l", 
       xlab = "Time", 
       ylab = "Change in The B Allele", 
       ylim = c(0,1))
  mtext(paste("u=", u, "p0=", p0), cex= 0.7)
  return()
}
MutationChangePlot()
#########################################

# now build a grid of plots 
# each with different parameter values
# global variables 

p0Pars <- c(0.25, 0.65, 0.95)
uPars <- c(10^-01, 10^-02, 10^-03, 10^-04)
par(mfrow = c(3,4))

# enter into a double loop for plotting 

for(i in 1:length(p0Pars)){
  for(j in 1:length(uPars)){
    MutationChangePlot(p0=p0Pars[i], u=uPars[j])
  }
}


par(mfrow=c(1,1))
# Plotting redux with ggplot 
library(ggplot2)

p0Pars <- c(.25, .65, .95)
uPars <- c(10^-02, 10^-03, 10^-04)
Time <- 1:10^3

modelFrame <- expand.grid(p0 = p0Pars, 
                          u = uPars, 
                          t = Time)

modelFrame
modelFrame$q <- NA


# tricky double for loop for filling new data frame 

for(i in 1:length(p0Pars)){
  for(j in 1:length(uPars)){
    modelFrame[modelFrame$p0==p0Pars[i] &  modelFrame$u == uPars[j], "q"] <- MutationChange(t=Time, p0= p0Pars[i], u=uPars[j])  # each combination of z parameter and c parameter - give me just those rows and the S column
  }
}

# Starts with cpars for every one changes with zpars specifics
# colS is the sumber of species given all the parameters
print(modelFrame)
View(modelFrame)
dev.off()

p1 <- ggplot(data= modelFrame)
p1 + geom_line(mapping=aes(x=t, y=q)) +
  facet_grid(p0~u)

p2 <- ggplot(data = modelFrame, mapping = aes(x=t, y=q)) +
  geom_line()+
  facet_grid(.~p0)

p2


p2 <- p1
p2 + geom_line(mapping = aes(x=t, y=q, group =u)) +
  facet_grid(.~u)


##### RANDOMIZATION ###########################################

# Preliminaries 
library(ggplot2)
library(TeachingDemos)
char2seed("Cruel April")

##########################################

# FUNCTION: readData 
# read in or generate data frame 
# input: file name (or nothin for demo)
# output: 3-column data frame of observed data (ID, x, y) 
# --------------------------------
iris
set.seed(100)

readData <- function(data=iris) {
    xVar <- iris$Sepal.Length
    yVar <- iris$Petal.Length
    dF <- data.frame(xVar,yVar) 
  
  return(dF)
}
readData(dF)      

##########################################

##########################################

# Function: getMetric 
# calculate metric for randomization test
# input: 3-column data frame for regression 
# output: regression slope 

#------------------------------------------
readData <- function(z=NULL){
  if(is.null(z)){
    xVar <- c(iris$Sepal.Length)
    yVar <- c(iris$Sepal.Width)
    dF <- data.frame(ID=seq_along
                     (xVar), xVar, yVar)
    return(dF)}
}
dF
##########################################

# Function: shuffleData 
# randomize data for regression analysis 
# input: 3-column data frame (ID,xVar, yVar) 
# output: 3-column data frame (ID, xVar, YVar) 

#------------------------------------------
getMetric <- function(z=NULL){
  if(is.null(z)){
    xVar <- c(iris$Sepal.Length)
    yVar <- c(iris$Sepal.Width)
    z <- data.frame(ID=seq_along
                    (xVar), xVar, yVar)}
  . <- lm(z[,3]~z[,2])
  . <- summary(.)
  . <- .$coefficients[2,1]
  slope <- .
  return(slope)
}
getMetric()

######################################

shuffleData <- function(z=NULL){
  if(is.null(z)){
    xVar <- c(iris$Sepal.Length)
    yVar <- c(iris$Sepal.Width)
    z <- data.frame(ID=seq_along
                    (xVar), xVar, yVar)}
  
  z[,3] <- sample(z[,3])
  return(z)
}
shuffleData()



#########################################

#Function: getPVAL
# calculate p value for observed, simulated data 
# input: list of observed metric and vector of simulated metric
# output: lower, upper tail probability vector

getPval <- function(z=NULL) {
  if(is.null(z)){
    z <- list(xObs=runif(1), xSim=runif(1000))}
  
  pLower <- mean(z[[2]] <=z[[1]])
  pUpper <- mean(z[[2]] >=z[[1]])
  
  return(c(pL=pLower, pU= pUpper))
  
}
getPval()





#--------------------------------------- Main Body 

nSim <- 10000 # number of simulations
Xsim <- rep(NA, nSim) #will hold simulated slopes

dF <- readData()
Xobs <- getMetric(dF)

for (i in seq_len(nSim)) {
  Xsim[i] <- getMetric(shuffleData(dF)) }

slopes <- list(Xobs,Xsim)
getPval(slopes)

#------------------------------------------
## FUNCTION: plotRanTest 
## ggplot graph 
## input : list of observed metric and vector of the simulated metric 
## output: ggplot graph 

plotRantTest <- function(z=NULL){
  if(is.null(z)){
    z <- list(xObs=runif(1), xSim=runif(1000))}
  df <- data.frame(ID= seq_along(z[[2]]), 
                   simX=z[[2]])
  p1 <- ggplot(data=df, mapping=aes(x=simX))
  p1 + geom_histogram(mapping=aes(fill=I("mistyrose1"), color=I("black"))) +
    geom_vline(aes(xintercept=z[[1]], col="blue"))
}
plotRantTest()


###################################


lmiris <- lm(iris$Sepal.Length ~ iris$Sepal.Width, data = readData())
summary(lmiris)

lmirisShuffle <- lm(iris$Sepal.Length ~ iris$Sepal.Width, data = shuffleData())
summary(lmirisShuffle)
