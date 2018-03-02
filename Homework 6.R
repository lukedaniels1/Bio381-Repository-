library(MASS)
library(ggplot2)
library(haven) 
driving <- read_dta("driving_2004-2.dta")

# Plotting the Initial Histogram 

driving2 <- data.frame(driving$totfat)
                       names(driving2) <- c("totalfatal")
      
View(driving2)
mean(driving2) 

p1 <- ggplot(data=driving2, aes(x=totalfatal, y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) 
print(p1)

# Empirical Density Curve 

p1 <-  p1 +  geom_density(linetype="dotted",size=0.75)
print(p1)

# Maximum Likelihood of Parameters

normPars <- fitdistr(driving$totfat,"normal")
print(normPars)

normPars$estimate["mean"]

str(normPars)

# Normal Distribution 

meanML <- normPars$estimate["mean"]
sdML <- normPars$estimate["sd"]

xval <- seq(0,max(driving$totfat),len=length(driving$totfat))

stat <- stat_function(aes(x = xval, y = ..y..), fun = dnorm, colour="red", n = length(driving$totfat), args = list(mean = meanML, sd = sdML))
print(p1 + stat)

# Exponential Probability Distribution 

expoPars <- fitdistr(driving$totfat,"exponential")
rateML <- expoPars$estimate["rate"]

stat2 <- stat_function(aes(x = xval, y = ..y..), fun = dexp, colour="blue", n = length(driving$totfat), args = list(rate=rateML))
print(p1 + stat + stat2)


# Uniform 

stat3 <- stat_function(aes(x = xval, y = ..y..), fun = dunif, colour="darkgreen", n = length(driving$totfat), args = list(min=min(driving$totfat), max=max(driving$totfat)))
p1 + stat + stat2 + stat3


# Gamma Distribution 

gammaPars <- fitdistr(driving$totfat,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]

stat4 <- stat_function(aes(x = xval, y = ..y..), fun = dgamma, colour="brown", n = length(driving$totfat), args = list(shape=shapeML, rate=rateML))
p1 + stat + stat2 + stat3 + stat4

# Beta Distribution 

pSpecial <- ggplot(data=driving2, aes(x=driving$totfat/(max(driving$totfat + 0.1)), y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) + 
  xlim(c(0,1)) +
  geom_density(size=0.75,linetype="dotted")

betaPars <- fitdistr(x=driving$totfat/max(driving$totfat + 0.1),start=list(shape1=1,shape2=2),"beta")
shape1ML <- betaPars$estimate["shape1"]
shape2ML <- betaPars$estimate["shape2"]

statSpecial <- stat_function(aes(x = xval, y = ..y..), fun = dbeta, colour="orchid", n = length(driving$totfat), args = list(shape1=shape1ML,shape2=shape2ML))
pSpecial + statSpecial


# Compare Log Likelihoods 
# High Likelihoods are better fit 

normPars$loglik 
gammaPars$loglik 
expoPars$loglik

# Gamma is the best fit! 

z <- rgamma(n= 48, shape=shapeML, rate=rateML) 
z <- data.frame(1:48,z)

names(z) <- list("ID","myVar")
z <- z[z$myVar>0,]
str(z)

p5 <- ggplot(data=z, aes(x=myVar, y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) 
print(p5)

normPars <- fitdistr(z$myVar,"normal")
print(normPars)

str(normPars)

gammaPars <- fitdistr(z$myVar,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]

stat4 <- stat_function(aes(x = xval, y = ..y..), fun = dgamma, colour="brown", n = length(z$myVar), args = list(shape=shapeML, rate=rateML))
p5 + stat4

p1 + stat4
