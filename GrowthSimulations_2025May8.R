###################################################################################################################
###################################################################################################################
#
# INTRODUCTION
# This script is part of the supplementary materials for Altitudinal range shifts among high-elevation plant 
# species from the tropical Andes.
# 
# PURPOSE OF THIS SCRIPT
# Simulate stem length growth in Espeletia (Espeletiinae, Asteraceae)
#
# DATA FILES REQUIRED TO RUN THIS SCRIPT
# "Fagua_Gonzalez_2007_points.csv"
#
# TABLE OF CONTENTS
# 1) Preliminaries: load packages, read and examine data.
# 2) Calculate initial stem length in centimeters and growth rate in centimeters per year.
# 3) Models for the distribution of growth rate.
# 4a) Simulations stem length growth: gr.factor
# 4b) Simulations stem length growth: modified mean and sd
# 5) Examine simulation results.
#
#
###################################################################################################################
###################################################################################################################


###################################################################################################################
# 1) Preliminaries:load packages, read and examine data.
###################################################################################################################

#load packages
library(MASS)

#read and examine data from Fagua and Gonzalez (2007)
#setwd("C:/_transfer/Papers/EspeletiaOccupancy/Data") 
setwd("C:/Users/espej/Desktop/TROPIMUNDO/Frailejones_Stats") 
FG.data <- read.table("Fagua_Gonzalez_2007_points.csv", header=T, sep=",")
class(FG.data)
dim(FG.data)
colnames(FG.data)
head(FG.data)


###################################################################################################################
# 2) Calculate initial stem length in centimeters and growth rate in centimeters per year.
###################################################################################################################

#calculate initial stem length in centimeters
FG.sl <- 10^(FG.data$x)
summary(FG.sl)

#calculate growth rate in centimenters per year
FG.gr <- 10^(FG.data$x + FG.data$y) - 10^(FG.data$x)
summary(FG.gr)


###################################################################################################################
# 3) Models for the distribution of growth rate.
###################################################################################################################

#model.0: growth rate, possibly transformed, is a normal distribution
boxcox(lm(FG.gr ~ 1)) #Boxcox transformation 
abline(v=0.5, col="red") #square root transformation is appropriate (i.e., lambda=0.5)
summary(FG.gr^0.5)
#par(mar=c(5, 4, 4, 2) + 0.1) #default
par(mar=c(5, 5, 4, 5) + 0.1)
hist(FG.gr^0.5, breaks=seq(0, 5.2, 0.1), xlim=c(0, 5.2),
     xlab=expression(sqrt("Growth rate (cm/year)")),
     ylab="Plants", main="",
     cex.axis=1.5, cex.lab=1.5)
par(new=T)
plot(seq(0, 5.2, 0.1), dnorm(seq(0, 5.2, 0.1), mean = mean(FG.gr^0.5), sd = sd(FG.gr^0.5)),
     type="l", bty="n", xaxt="n", yaxt="n",, xlab="", ylab="", xlim=c(0,5.2), lty=2, lwd=2)
axis(4, cex.axis=1.5)
mtext("Probability density", side=4, cex=1.5, line=2.8) 

#model.1: growth rate, possibly transformed, is a zero-truncated normal distribution
boxcox(lm(FG.gr ~ 1)) #Boxcox transformation 
abline(v=0.5, col="red") #square root transformation is appropriate (i.e., lambda=0.5)
summary(FG.gr^0.5)
alpha <- - mean(FG.gr^0.5)/sd(FG.gr^0.5)
mean.zero.truncated <- mean(FG.gr^0.5) + sd(FG.gr^0.5)*dnorm(alpha)/(1-pnorm(alpha))
variance.zero.truncated <- var(FG.gr^0.5)*( 1 + (alpha*dnorm(alpha)/(1-pnorm(alpha))) - (dnorm(alpha)/(1-pnorm(alpha))^2))

#par(mar=c(5, 4, 4, 2) + 0.1) #default
par(mar=c(5, 5, 4, 5) + 0.1)
hist(FG.gr^0.5, breaks=seq(0, 5.2, 0.1), xlim=c(0, 5.2),
     xlab=expression(sqrt("Growth rate (cm/year)")),
     ylab="Plants", main="",
     cex.axis=1.5, cex.lab=1.5)
par(new=T)
plot(seq(0, 5.2, 0.1), dnorm(seq(0, 5.2, 0.1), mean = mean.zero.truncated, sd = variance.zero.truncated^0.5),
     type="l", bty="n", xaxt="n", yaxt="n",, xlab="", ylab="", xlim=c(0,5.2), lty=2, lwd=2)
axis(4, cex.axis=1.5)
mtext("Probability density", side=4, cex=1.5, line=2.8) 

#model.2: growth rate is a function of initial stem length
boxcox(lm(FG.gr ~ FG.sl)) #Boxcox transformation 
abline(v=0.5, col="red") #square root transformation is appropriate (i.e., lambda=0.5)
model.2 <- lm(I(FG.gr^0.5) ~ FG.sl)
summary(model.2) #not a significant relationship between growth rate and initial stem length
par(mar = c(5, 5, 3, 2))
plot(FG.sl, FG.gr^0.5, bty="n", cex=1.5, cex.axis=1.5, cex.lab=1.5, pch=21, bg="gray80",
     xlab="Stem length (cm)", ylab=expression(sqrt("Growth rate (cm/year)")))
axis(1, at=seq(0, 150, 10), labels=F)
abline(model.2)



###################################################################################################################
# 4a) Simulations stem length growth: gr.factor
###################################################################################################################

k.plants <- 1000 #number of plants to simulate
t.years <- 100 #time in years to simulate growth of each plant
gr.factor <- 1 #factor to reduce growth rate, >0 and <=1
gr.factor * mean(FG.gr) #mean growth rate for simulation
size.ti <- min(FG.sl) #initial size, as measured by stem length
size.tf <- matrix(NA, t.years, k.plants) #matrix to be populated with the size (as measured by stem length) of each plant each year
gr <- matrix(NA, t.years, k.plants) #matrix to be populated with the growth rate of each plant each year
plot(0:t.years, (0:t.years)*10, xlab="Years", ylab="Stem lenght (cm)", bty="n", type="n", cex.lab=1.5, cex.axis=1.5) #plot to track the simulation
points(0, min(FG.sl), type="p", col="gray", pch=19)

#begin simulation
for(j in 1:k.plants)
{
  if(gr.factor <= 0 | gr.factor > 1){
    print("ERROR: growth rate factor is <0 or >1")
    break
  }
  size.ti <- min(FG.sl) 
  for(i in 1:t.years)
  {
    gr[i,j] <- gr.factor * (rnorm(1, mean(FG.gr^0.5), sd = sd(FG.gr^0.5)))^2
    size.tf[i,j] <- size.ti + gr[i,j]
    size.ti <- size.tf[i,j]
    points(i, size.tf[i,j], type="p", col="gray", pch=19)
  }
}
#end simulation


###################################################################################################################
# 4b) Simulations stem length growth: modified mean and sd
###################################################################################################################
k.plants <- 100 #number of plants to simulate
t.years <- 99 #time in years to simulate growth of each plant
gr.factor <- 1 #factor to reduce growth rate, >0 and <=1
gr.factor * mean(FG.gr) #mean growth rate for simulation
size.ti <- min(FG.sl) #initial size, as measured by stem length
size.tf <- matrix(NA, t.years, k.plants) #matrix to be populated with the size (as measured by stem length) of each plant each year
gr <- matrix(NA, t.years, k.plants) #matrix to be populated with the growth rate of each plant each year
plot(0:t.years, (0:t.years)*10, xlab="Years", ylab="Stem lenght (cm)", bty="n", type="n", cex.lab=1.5, cex.axis=1.5) #plot to track the simulation
points(0, min(FG.sl), type="p", col="gray", pch=19)


#mean and sd ^0.5
sqrt.mean <- mean(FG.gr^0.5)        
sqrt.sd <- sd(FG.gr^0.5)
var(FG.gr)



# Define targets
target.mean <- 7
target.var <- 15.77


# Combined Adjustments: adjust to the target mean and sd
library(nleqslv)
solve.parameters <- function(target.mean, target.var) {
  f <- function(x) {
    sqrt.mean <- x[1]
    sqrt.sd <- x[2]
    c(
      sqrt.mean^2 + sqrt.sd^2 - target.mean,
      4*sqrt.mean^2*sqrt.sd^2 + 2*sqrt.sd^4 - target.var
    )
  }
  solution <- nleqslv(c(sqrt.mean, sqrt.sd), f)
  return(list(sqrt.mean = solution$x[1], sqrt.sd = solution$x[2]))
}

#solve parameter
params <- solve.parameters(target.mean, target.var)


#begin simulation
for(j in 1:k.plants)
{
  if(gr.factor <= 0 | gr.factor > 1){
    print("ERROR: growth rate factor is <0 or >1")
    break
  }
  size.ti <- min(FG.sl) 
  for(i in 1:t.years)
  {
    gr[i,j] <- (rnorm(1, mean = params$sqrt.mean, sd = params$sqrt.sd))^2
    size.tf[i,j] <- size.ti + gr[i,j]
    size.ti <- size.tf[i,j]
    points(i, size.tf[i,j], type="p", col="gray", pch=19)
  }
}
#end simulation

#Check for the mean 
mean(as.vector(gr))
var(as.vector(gr))


###################################################################################################################
# 5) Examine simulation results
###################################################################################################################

#were there any NA values?
sum(is.na(gr))
sum(is.na(size.tf))

#were there negative simulated growth rates?
summary(as.vector(gr))

#the range of simulated sizes, as measured by stem length (cm)
summary(as.vector(size.tf))

###################################################################################################################
# 5.1) Relationship between stem length and years
###################################################################################################################

#over all t.years simulated
#par(mar=c(5, 4, 4, 2) + 0.1) #default
par(mar=c(5, 5, 4, 5) + 0.1)
plot(0:t.years, (0:t.years)*10, xlab="Years", ylab="Stem length (cm)", bty="n", type="n", pch=19, col="gray80",
     xlim=c(0,t.years), ylim=c(0,t.years*10), xaxt="n", cex.lab=1.5, cex.axis=1.5)
axis(1, at=seq(0, t.years, 20), cex.axis=1.5)
axis(1, at=seq(0, t.years, 5), labels=F)
for(j in 1:t.years)
{
  points(0:t.years, c(min(FG.sl), size.tf[,j]), type="o", pch=19, col="gray80")
}


#focus on initial years
#par(mar=c(5, 4, 4, 2) + 0.1) #default
par(mar=c(5, 5, 4, 2) + 0.1)
plot(0:t.years, (0:t.years)*10, xlab="Years", ylab="Stem length (cm)", bty="n", type="n", pch=19, col="gray80",
     xlim=c(0,20), ylim=c(0,200), xaxt="n", cex.lab=1.5, cex.axis=1.5)
axis(1, at=seq(0, t.years, 5), cex.axis=1.5)
axis(1, at=seq(0, t.years, 1), labels=F)
for(j in 1:t.years)
{
  points(0:t.years, c(min(FG.sl), size.tf[,j]), type="o", pch=19, col="gray80")
}

