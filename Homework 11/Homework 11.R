

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
                             u=10^-6,
                             t=1:10^5){ 
  q <- 1- (p0*exp(-u*t))
  return(q)
} 

head(MutationChange())
View(MutationChange())



par("mar")
par(mar=c(1,1,1,1))
##########################################
MutationChangePlot <- function(p0=0.90,
                               u=10^-06,
                               t=1:500) {
  plot(x = t, y = MutationChange(p0,u,t),
       type = "l", 
       xlab = "Island Area", 
       ylab = "S", 
       ylim = c(0,1))
  mtext(paste("c=", u, "z=", p0), cex= 0.7)
       return()
}
MutationChangePlot()
#########################################

# now build a grid of plots



SpeciesAreaCurve <- function(A=0.90,
                             c=10^-06,
                             z=1:10){ 
  S <- c*(A^z)
  return(S)
} 

head(SpeciesAreaCurve())



