###################################################################################################################
###################################################################################################################
###################################################################################################################
# 
# INTRODUCTION
# This script is part of the supplementary materials for Altitudinal range shifts among high-elevation plant 
# species from the tropical Andes. 
# 
# PURPOSE OF THIS SCRIPT
# Test predictions derived from the hypothesis that Espeletia species from the páramo de Sumapaz are responding
# to global warming by expanding or leaning their geographic ranges, as discussed under different categories of range
# shifts by Lenoir and Svenning (2015. Ecography 38: 15-28). 
# Only undisturbed quadrats are considered.
#
# DATA FILES REQUIRED TO RUN THIS SCRIPT
# i) "Occupancy_2025April15.csv" with data on occupancy.
# ii) "phenodata_species_taxa_determinations_2025March30.csv" with data on taxonomic name and phenotype group for 
#     herbarium specimens.
# iii) "DisturbioEstratos_2019Aug14.csv" with data on disturbed cells (crops and pastures).
#
# TABLE OF CONTENTS
# 1) Read and examine occupancy data.
# 2) Read and select the morphological group data.
# 3) Extract occupancy data for a species or morphological group.
# 4) Read and exclude land use data (pasture and crop).
# 5) Define younger and older cohorts.
# 6) Estimate mean, minimum and maximum elevation across occupied sampling cells, for each plant size category.
# 7) Null model.
# 8) Compare null model values with observed values.
# 9) Store the results for each species in a list when executed with a "for" loop.
#
###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
# 1) Read and examine occupancy data.
###################################################################################################################
###################################################################################################################


#set working directory
#setwd("C:/_transfer/papers/EspeletiaOccupancy/Data") 
setwd("C:/Users/espej/Desktop/TROPIMUNDO/Frailejones_Stats")  
#confirm current working directory
getwd() 

#read and examine occupancy data
occupancy <- read.table("Occupancy_2025April15.csv", header=TRUE, sep=",")
dim(occupancy)
head(occupancy)
summary(occupancy)

#examine survey dates
unique(occupancy$Fecha)
data.frame(occupancy$Fecha, as.Date(occupancy$Fecha, format="%d-%m-%y")) #check conversion to date format

#examine total number of sampled 2 x 2 km quadrats
unique(occupancy$Cuadrante) #unique sampled quadrats
length(unique(occupancy$Cuadrante)) #number of unique sampled quadrats
occupancy[occupancy$Cuadrante==246,] #examine data for a particular quadrat


#examine total number of sampled 30 x 30 m grid cells
unique(occupancy$Celda) #unique sampled grid cells
length(unique(occupancy$Celda)) #number of unique sampled grid cells
occupancy[occupancy$Celda==10438039,] #examine data for a particular grid cell


###################################################################################################################
###################################################################################################################
# 2) Read and select the morphological group data.
###################################################################################################################
###################################################################################################################

#set working directory
#setwd("C:/_transfer/papers/EspeletiaOccupancy/Data") 
setwd("C:/Users/espej/Desktop/TROPIMUNDO/Frailejones_Stats")
#confirm current working directory
getwd() 

#read and examine specimen data
pheno.group <- read.table("phenodata_species_taxa_determinations_2025March30.csv", header=TRUE, sep=",")
dim(pheno.group)
head(pheno.group)
summary(pheno.group)


###################################################################################################################
# 2.1) To execute the code manually. (Run either 2.1 or 2.2)
###################################################################################################################

#select the taxonomic species or phenotypic groups to test focal taxonimic group:
#"E. argentea", "E. cabrerensis", "E. grandiflora", "E. killipii", "E. miradorensis", 
#"E. summapacis", "complejo_sin_bracteas".

focal.sp <- "E. grandiflora"

#examine data for specimens under the selected specimen.
pheno.group[pheno.group$Species.Taxa.Assignment.For.Occupancy.Analysis==focal.sp,]
pheno.group[which(pheno.group$Species.Taxa.Assignment.For.Occupancy.Analysis==focal.sp),]  #avoiding NAs

#examine collection number for specimens under the selected specimen.
pheno.group$Corrected.Collector.Collection.Number[
  pheno.group$Species.Taxa.Assignment.For.Occupancy.Analysis==focal.sp]
pheno.group$Corrected.Collector.Collection.Number[
  which(pheno.group$Species.Taxa.Assignment.For.Occupancy.Analysis==focal.sp)] #avoiding NAs



##################################################################################################################
# 2.2) To execute the code for several species at a time with a "for" loop.(Run either 2.1 or 2.2)
##################################################################################################################

#Beginning of the loop to run the code several species
species.list <- c("E. grandiflora", "E. killipii", "E. summapacis", "complejo_sin_bracteas")
results <- list()
for (focal.sp in species.list) {

#End of the "for" loop in section 9.1

###################################################################################################################
###################################################################################################################
# 3) Extract occupancy data for a species or morphological group.
###################################################################################################################
###################################################################################################################


#set of collection numbers belonging to the selected group
focal.sp.col.num <- pheno.group$Corrected.Collector.Collection.Number[
  which(pheno.group$Species.Taxa.Assignment.For.Occupancy.Analysis == focal.sp)] #avoiding NAs
focal.col <- focal.sp.col.num[grepl("YAM", focal.sp.col.num)]
focal.col.num <- gsub("[^[:digit:]]", "", focal.col)
summary(focal.col.num)
sort(focal.col.num)
length(focal.col.num)


#determine which rows of the occupancy data frame correspond
#to the collection numbers of the focal species or morphological group
sort(occupancy$Numerodecoleccion_Plantula1)
table(occupancy$Numerodecoleccion_Plantula1)                                     
mP1 <- match(focal.col.num, occupancy$Numerodecoleccion_Plantula1)
mP1
length(mP1[!is.na(mP1)])
#
sort(occupancy$Numerodecoleccion_Plantula2)
table(occupancy$Numerodecoleccion_Plantula2)
mP2 <- match(focal.col.num, occupancy$Numerodecoleccion_Plantula2)
mP2
length(mP2[!is.na(mP2)])
#
sort(occupancy$Numerodecoleccion_Plantula3)
table(occupancy$Numerodecoleccion_Plantula3)
mP3 <- match(focal.col.num, occupancy$Numerodecoleccion_Plantula3)
mP3
length(mP3[!is.na(mP3)])
#
sort(occupancy$Numerodecoleccion_Adulto1)
table(occupancy$Numerodecoleccion_Adulto1)
mA1 <- match(focal.col.num, occupancy$Numerodecoleccion_Adulto1)
mA1
length(mA1[!is.na(mA1)])
#
sort(occupancy$Numerodecoleccion_Adulto2)
table(occupancy$Numerodecoleccion_Adulto2)
mA2 <- match(focal.col.num, occupancy$Numerodecoleccion_Adulto2)
mA2
length(mA2[!is.na(mA2)])
#
sort(occupancy$Numerodecoleccion_Adulto3)
table(occupancy$Numerodecoleccion_Adulto3)
mA3 <- match(focal.col.num, occupancy$Numerodecoleccion_Adulto3)
mA3
length(mA3[!is.na(mA3)])
#
mPA <- c(mP1, mP2, mP3, mA1, mA2, mA3)
mPA <- mPA[!is.na(mPA)]
length(unique(mPA))
table(mPA)

#extract occupancy data for focal species
occ.focal <- occupancy[unique(mPA),]
dim(occ.focal)
head(occ.focal)

#remove rows corresponding to re-sampling of cells
class(occ.focal$Repeticion)
unique(occ.focal$Repeticion)
occ.focal <- occ.focal[occ.focal$Repeticion<1,]
dim(occ.focal)
head(occ.focal)

#determine if any survey cell is duplicated
unique(occ.focal$Celda)
length(unique(occ.focal$Celda))
dim(occ.focal)
table(occ.focal$Celda)
table(occ.focal$Celda)[table(occ.focal$Celda)>1]

#code to examine one case at a time
occ.focal[occ.focal$Celda == 10503225,]
occ.focal[occ.focal$Celda == 7907378,]
occ.focal[occ.focal$Celda == 8082619,]

#code that examine all duplicated cells at once
duplicated.cells <- as.numeric(names(table(occ.focal$Celda)[table(occ.focal$Celda)>1]))
index.duplicated.cells <- which(!is.na(match(occ.focal$Celda, duplicated.cells)))
occ.focal.duplicated.cells <- occ.focal[index.duplicated.cells,]
occ.focal.duplicated.cells <- occ.focal.duplicated.cells[order(occ.focal.duplicated.cells$Celda),]
occ.focal.duplicated.cells 

#i <- 6884811
class(occ.focal)
head(occ.focal)

#if there are duplicated cells, merge their occupancy data
for(i in duplicated.cells){
  index.merge.cells <- which(occ.focal$Celda==i)
  merge.occ.focal <- as.numeric(colSums(occ.focal[index.merge.cells,8:21]) > 0)
  occ.focal[index.merge.cells[1],8:21] <- merge.occ.focal
  occ.focal <- occ.focal[-index.merge.cells[2:length(index.merge.cells)],]
}


#check the result, there should be no duplicated cells
unique(occ.focal$Celda)
length(unique(occ.focal$Celda))
dim(occ.focal)
table(occ.focal$Celda)
table(occ.focal$Celda)[table(occ.focal$Celda)>1]


#remove NA´s in the occupancy matrix
summary(!is.na(occ.focal[,8:19]))
occ.focal <- occ.focal[complete.cases(occ.focal[, 8:19]), ]
print(occ.focal)
dim(occ.focal)
summary(occ.focal)



###################################################################################################################
###################################################################################################################
# 4) Read and exclude land use data (pasture and crop).
###################################################################################################################
###################################################################################################################

#set working directory
#setwd("C:/_transfer/papers/EspeletiaOccupancy/Data") 
setwd("C:/Users/espej/Desktop/TROPIMUNDO/Frailejones_Stats") 
#confirm current working directory
getwd() 

#read and examine occupancy data
land.use <- read.table("DisturbioEstratos_2019Aug14.csv", header=TRUE, sep=",")
dim(land.use)
head(land.use)
summary(land.use)

#review pasture data for all the 30x30 m grid cells
pasture <- land.use[land.use$Estrato == "Potrero",]
total.pasture <-rowSums(pasture[,8:23])
dim(pasture)
hist(total.pasture, breaks = seq(-0.5,16.5,1)) 
plot(pasture$Altitud, total.pasture, xlab = "Elevation", ylab = "Pastures")

#review pasture data for all the 30x30 m grid cells for the focal species 
occ.focal.cells <- occ.focal$Celda
pasture.cells <- match(occ.focal.cells, pasture$Celda)
pasture.focal <- pasture[unique(pasture.cells),]
dim(pasture.focal)
total.pasture.focal <-rowSums(pasture.focal[,8:23])
hist(total.pasture.focal)
plot(pasture.focal$Altitud, total.pasture.focal, xlab = "Elevation", ylab = "Pastures")
table(total.pasture.focal)

#exclude pastures and crops from occupancy matrix
pasture.crops <- land.use[land.use[, "Estrato"] %in% c("Potrero", "Cultivo"), ]
dim(pasture.crops)
disturbed.cells <- pasture.crops[rowSums(pasture.crops[, 8:23], na.rm = TRUE) > 0, ]
cells.to.exclude <- unique(disturbed.cells$Celda)
occ.focal <- occ.focal[!(occ.focal$Celda %in% cells.to.exclude), ]
dim(occ.focal)


###################################################################################################################
###################################################################################################################
# 5) Define younger and older cohorts.
###################################################################################################################
###################################################################################################################

#define the younger and older cohorts
categories <- occ.focal[, 8:19]
younger <- names(categories)[1:3]  
print(younger)
older <- names(categories) [6:12]
print(older)


###################################################################################################################
###################################################################################################################
# 6) Estimate mean, minimum and maximum elevation across occupied sampling cells, for each plant size category.
###################################################################################################################
###################################################################################################################

#total elevation range
occ.focal$Altitud
summary(occ.focal$Altitud)

#younger elevation range
younger.altitudes <- occ.focal$Altitud[rowSums(occ.focal[younger])>0]
sum(rowSums(occ.focal[younger])>0)
length(younger.altitudes)
summary(younger.altitudes)
hist(younger.altitudes, xlim = range(3200 , 4300), xlab = "Elevation of younger individuals",
     main = focal.sp, breaks = seq(3100, 4200, 100))
ME.younger <- mean(younger.altitudes)  # mean elevation
ME.younger
LE.younger <- min(younger.altitudes)   # lower elevation limit
LE.younger
UE.younger <- max(younger.altitudes)   # upper elevation limit
UE.younger

#older elevation range
older.altitudes <- occ.focal$Altitud[rowSums(occ.focal[older])>0]
older.altitudes
length(older.altitudes)
summary(older.altitudes)
hist(older.altitudes, xlim = range(3200 , 4300), xlab = "Elevation of older individuals",
     main = focal.sp, breaks = seq(3100, 4200, 100))
ME.older <- mean(older.altitudes)  # mean elevation
ME.older
LE.older <- min(older.altitudes)# lower elevation limit
LE.older
UE.older <- max(older.altitudes)   # upper elevation limit
UE.older

###################################################################################################################
###################################################################################################################
# 7) Null model.
###################################################################################################################
###################################################################################################################

#prepare objects to run the null model
om <- as.matrix(occ.focal[, 8:19])  # occupancy matrix for younger and older categories
head(occ.focal)
n.present <- sum(rowSums(om) > 0)  # number of cells where the species is present
identical(n.present, nrow(om))  # should be TRUE
rows.present <- which(rowSums(om) > 0)  # rows (corresponding to cells) where the species is present
n.cat.present <- colSums(om > 0)  # number of cells where each size category is present

#define number of iterations of the null model
k.max <- 10000

#create vectors to store null EM, LE and UE values for younger and older categories
Null.ME.younger <- rep(NA, times = k.max)
Null.LE.younger <- rep(NA, times = k.max)
Null.UE.younger <- rep(NA, times = k.max)

Null.ME.older <- rep(NA, times = k.max)
Null.LE.older <- rep(NA, times = k.max)
Null.UE.older <- rep(NA, times = k.max)

# define initial value of the iteration variable "k"
k <- 1

#run the null model
while(k <= k.max)
{
  #create null occupancy matrix, initially it will have zero in all entries
  Null.om <- om
  Null.om[,] <- 0
  
  #generate null occupancy data for each age category 
  for(i in 1:ncol(Null.om)) 
  {
    Null.rows.cat.present <- sample(rows.present, n.cat.present[i]) 
    Null.om[Null.rows.cat.present, i] <- 1
  }
  
  #if the null occupancy matrix does not preserve the number of cells where the species is present,
  #go back to the beginning of the iteration
  if(sum(rowSums(Null.om) > 0) != n.present) next
  
  #convert null occupancy matrix to data.frame
  Null.om <- data.frame(Null.om)
  Null.om[1:5,younger]
  class(Null.om)
  
  #calculate null EM, LE and UE values for younger category
  younger_present <- rowSums(Null.om[, younger]) > 0
  Null.ME.younger[k] <- mean(occ.focal$Altitud[younger_present])  # mean elevation
  Null.LE.younger[k] <- min(occ.focal$Altitud[younger_present])   # lower elevation limit
  Null.UE.younger[k] <- max(occ.focal$Altitud[younger_present])   # upper elevation limit
  
  #calculate null EM, LE and UE values for older category
  older_present <- rowSums(Null.om[, older]) > 0
  Null.ME.older[k] <- mean(occ.focal$Altitud[older_present])  # mean elevation
  Null.LE.older[k] <- min(occ.focal$Altitud[older_present])   # lower elevation limit
  Null.UE.older[k] <- max(occ.focal$Altitud[older_present])   # upper elevation limit
  
  #graph to monitor progression of null model iterations
  par(mar = c(5, 5, 4, 5) + 0.1)
  hist(occupancy$Altitud, breaks = seq(3100, 4200, 100), xlim = c(3100, 4200), main = "", col = "gray", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  axis(4, cex.axis = 1.5, cex.lab = 1.5)
  axis(1, cex.axis = 1.5, cex.lab = 1.5)
  mtext("Sampled 30 x 30 m cells (bars)", side = 4, line = 2.5, cex = 1.5)
  mtext("Elevation (m)", side = 1, line = 2.5, cex = 1.5)
  par(new = T)
  h.younger <- hist(occ.focal$Altitud[younger_present], breaks = seq(3100, 4200, 100), plot = F)
  h.null.younger <- hist(occ.focal$Altitud[Null.om[, 1] > 0], breaks = seq(3100, 4200, 100), plot = F)
  plot(seq(3150, 4150, 100), h.younger$counts/sum(h.younger$counts), type = "o", xlim = c(3100, 4200), ylim = c(0, 1), pch = 19, col = "lightgreen", 
       xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n") 
  points(seq(3150, 4150, 100), h.null.younger$counts/sum(h.null.younger$counts), type = "o", pch = 19, col = "blue")
  title(paste("younger category, null model iteration = ", k))
  axis(2, cex.axis = 1.5, cex.lab = 1.5)
  mtext("Occupancy (lines and points)", side = 2, line = 3, cex = 1.5)
  abline(v = range(occ.focal$Altitud), lwd = 6, col = "gray90")
  abline(v = c(ME.younger, LE.younger, UE.younger), lty = 3, lwd = 3, col = "lightgreen")
  abline(v = c(Null.ME.younger[k], Null.LE.younger[k], Null.UE.younger[k]), lty = 3, lwd = 3, col = "blue")
  legend("topleft", c("Observed", "Null model", "PG range"), lty = 1, lwd = c(1, 1, 6), pch = c(19, 19, NA), col = c("lightgreen", "blue", "gray90"))
  
  #increase by one the value of the iteration variable
  k <- k + 1
}

#examine null model results

#younger
length(Null.ME.younger)
summary(Null.ME.younger)
length(Null.LE.younger)
summary(Null.LE.younger)
length(Null.UE.younger)
summary(Null.UE.younger)

#older
length(Null.ME.older)
summary(Null.ME.older)
length(Null.LE.older)
summary(Null.LE.older)
length(Null.UE.older)
summary(Null.UE.older)


###################################################################################################################
###################################################################################################################
# 8) Compare null model values with observed values.
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 8.1) Within size categories.
###################################################################################################################

#younger
#ME
length(Null.ME.younger)
summary(Null.ME.younger)
hist(Null.ME.younger, breaks=seq(3100, 4200, 10), main = "ME - younger", 
     xlab="Elevation (m)", ylab="Frequency")
abline(v=ME.younger, col="red", lwd=2)
#LE
length(Null.LE.younger)
summary(Null.LE.younger)
hist(Null.LE.younger, breaks = seq(3100, 4200, 10), main = "LE - younger",
     xlab = "Elevation (m)", ylab = "Frequency")
abline(v=LE.younger, col="red", lwd=2)
#UE
length(Null.UE.younger)
summary(Null.UE.younger)
hist(Null.UE.younger, breaks = seq(3100, 4200, 10), main = "UE - younger",
     xlab = "Elevation (m)", ylab = "Frequency")
abline(v=UE.younger, col="red", lwd=2)

#older
#ME
length(Null.ME.older)
summary(Null.ME.older)
hist(Null.ME.older, breaks=seq(3100, 4200, 10), main = "ME - older", 
     xlab="Elevation (m)", ylab="Frequency")
abline(v=ME.older, col="red", lwd=2)
#LE
length(Null.LE.older)
summary(Null.LE.older)
hist(Null.LE.older, breaks = seq(3100, 4200, 10), main = "LE - older",
     xlab = "Elevation (m)", ylab = "Frequency")
abline(v=LE.older, col="red", lwd=2)
#UE
length(Null.UE.older)
summary(Null.UE.older)
hist(Null.UE.older, breaks = seq(3100, 4200, 10), main = "UE - older",
     xlab = "Elevation (m)", ylab = "Frequency")
abline(v=UE.older, col="red", lwd=2)


###################################################################################################################
# 8.2) Between age categories. 
###################################################################################################################

#test the three predictions: younger vs older distributions
#ME
range.ME.younger.older <- range(c(Null.ME.younger - Null.ME.older, ME.younger - ME.older)) #examine range to determine histogram range
hist(Null.ME.younger - Null.ME.older, 
     breaks=seq((floor(range.ME.younger.older[1]/10)*10), 
                (ceiling(range.ME.younger.older[2]/10)*10), 2), 
     main= c(focal.sp, "Mean Elevation: younger vs older")) #distribution of null model values
abline(v=ME.younger - ME.older, col="red") #observed value
p.value.ME.upper <- sum((Null.ME.younger - Null.ME.older) >= (ME.younger - ME.older))/k.max #upper tail p-value
p.value.ME.lower <- sum((Null.ME.younger - Null.ME.older) <= (ME.younger - ME.older))/k.max #lower tail p-value
print(paste("ME p-values: Upper =", p.value.ME.upper))

#LE
range.LE.younger.older <- range(c(Null.LE.younger - Null.LE.older, LE.younger - LE.older)) #examine range to determine histogram range
hist(Null.LE.younger - Null.LE.older, 
     breaks=seq((floor(range.LE.younger.older[1]/10)*10), 
                (ceiling(range.LE.younger.older[2]/10)*10), 5), 
     main= c(focal.sp, "Lower Elevation Limit: younger vs older")) #distribution of null model values
abline(v=LE.younger - LE.older, col="red") #observed value
p.value.LE.upper <- sum((Null.LE.younger - Null.LE.older) >= (LE.younger - LE.older))/k.max #upper tail p-value
p.value.LE.lower <- sum((Null.LE.younger - Null.LE.older) <= (LE.younger - LE.older))/k.max #lower tail p-value
print(paste("LE p-values: Upper =", p.value.LE.upper))

#UE
range.UE.younger.older <- range(c(Null.UE.younger - Null.UE.older, UE.younger - UE.older)) #examine range to determine histogram range
hist(Null.UE.younger - Null.UE.older, 
     breaks=seq((floor(range.UE.younger.older[1]/10)*10), 
                (ceiling(range.UE.younger.older[2]/10)*10), 5), 
     main= c(focal.sp, "Upper Elevation Limit: younger vs older")) #distribution of null model values
abline(v=UE.younger - UE.older, col="red") #observed value
p.value.UE.upper <- sum((Null.UE.younger - Null.UE.older) >= (UE.younger - UE.older))/k.max #upper tail p-value
p.value.UE.lower <- sum((Null.UE.younger - Null.UE.older) <= (UE.younger - UE.older))/k.max #lower tail p-value
print(paste("UE p-values: Upper =", p.value.UE.upper))



###################################################################################################################
###################################################################################################################
# 9) Store the results for each species in a list when executed with a "for" loop for several species
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 9.1) store the results for each species in a list
###################################################################################################################

#store the results for each species in a list
results[[focal.sp]] <- list(
  obs.diff = list(
    ME = ME.younger - ME.older,
    LE = LE.younger - LE.older,
    UE = UE.younger - UE.older
  ),
  null.diff = list(
    ME = Null.ME.younger - Null.ME.older,
    LE = Null.LE.younger - Null.LE.older,
    UE = Null.UE.younger - Null.UE.older
  ),
  p.values.upper = list(
    ME = p.value.ME.upper,
    LE = p.value.LE.upper,
    UE = p.value.UE.upper
  ),
  p.values.lower = list(
    ME = p.value.ME.lower,
    LE = p.value.LE.lower,
    UE = p.value.UE.lower
  ),
  ci.one.tail = list(
    ME = quantile(Null.ME.younger - Null.ME.older, probs = c(0, 0.95)),
    LE = quantile(Null.LE.younger - Null.LE.older, probs = c(0, 0.95)),
    UE = quantile(Null.UE.younger - Null.UE.older, probs = c(0, 0.95))
  ),
  ci.two.tails= list(
    ME = quantile(Null.ME.younger - Null.ME.older, probs = c(0.025, 0.975)),
    LE = quantile(Null.LE.younger - Null.LE.older, probs = c(0.025, 0.975)),
    UE = quantile(Null.UE.younger - Null.UE.older, probs = c(0.025, 0.975))
  )
)
}

results
#End of the "for" loop

###################################################################################################################
# 9.2) Store the summary of the results for each species in a data frame
###################################################################################################################

#Espeletia "grandiflora" complex
grandiflora.list <- results[["E. grandiflora"]]
grandiflora.results <- data.frame(
  Predictions = c("LE", "ME", "UE"),
  Observed.value = c(grandiflora.list$obs.diff$LE, grandiflora.list$obs.diff$ME, grandiflora.list$obs.diff$UE),
  Lower.CI= c(grandiflora.list$ci.one.tail$LE[1], grandiflora.list$ci.one.tail$ME[1], grandiflora.list$ci.one.tail$UE[1]),
  Upper.CI= c(grandiflora.list$ci.one.tail$LE[2], grandiflora.list$ci.one.tail$ME[2], grandiflora.list$ci.one.tail$UE[2]),
  Lower.CI.two.tails= c(grandiflora.list$ci.two.tails$LE[1], grandiflora.list$ci.two.tails$ME[1], grandiflora.list$ci.two.tails$UE[1]),
  Upper.CI.two.tails= c(grandiflora.list$ci.two.tails$LE[2], grandiflora.list$ci.two.tails$ME[2], grandiflora.list$ci.two.tails$UE[2]),
  P.value.upper= c(grandiflora.list$p.values.upper$LE, grandiflora.list$p.values.upper$ME, grandiflora.list$p.values.upper$UE),
  P.value.lower= c(grandiflora.list$p.values.lower$LE, grandiflora.list$p.values.lower$ME, grandiflora.list$p.values.lower$UE)
)
write.csv(grandiflora.results, file = "grandiflora.results.csv")

#Espeletia killipii
killipii.list <- results[["E. killipii"]]
killipii.results <- data.frame(
  Predictions = c("LE", "ME", "UE"),
  Observed.value = c(killipii.list$obs.diff$LE, killipii.list$obs.diff$ME, killipii.list$obs.diff$UE),
  Lower.CI= c(killipii.list$ci.one.tail$LE[1], killipii.list$ci.one.tail$ME[1], killipii.list$ci.one.tail$UE[1]),
  Upper.CI= c(killipii.list$ci.one.tail$LE[2], killipii.list$ci.one.tail$ME[2], killipii.list$ci.one.tail$UE[2]),
  Lower.CI.two.tails= c(killipii.list$ci.two.tails$LE[1], killipii.list$ci.two.tails$ME[1], killipii.list$ci.two.tails$UE[1]),
  Upper.CI.two.tails= c(killipii.list$ci.two.tails$LE[2], killipii.list$ci.two.tails$ME[2], killipii.list$ci.two.tails$UE[2]),
  P.value.upper= c(killipii.list$p.values.upper$LE, killipii.list$p.values.upper$ME, killipii.list$p.values.upper$UE),
  P.value.lower= c(killipii.list$p.values.lower$LE, killipii.list$p.values.lower$ME, killipii.list$p.values.lower$UE)
)
write.csv(killipii.results, file = "killipii.results.csv")

#Espeletia summapacis
summapacis.list <- results[["E. summapacis"]]
summapacis.results <- data.frame(
  Predictions = c("LE", "ME", "UE"),
  Observed.value = c(summapacis.list$obs.diff$LE, summapacis.list$obs.diff$ME, summapacis.list$obs.diff$UE),
  Lower.CI= c(summapacis.list$ci.one.tail$LE[1], summapacis.list$ci.one.tail$ME[1], summapacis.list$ci.one.tail$UE[1]),
  Upper.CI= c(summapacis.list$ci.one.tail$LE[2], summapacis.list$ci.one.tail$ME[2], summapacis.list$ci.one.tail$UE[2]),
  Lower.CI.two.tails= c(summapacis.list$ci.two.tails$LE[1], summapacis.list$ci.two.tails$ME[1], summapacis.list$ci.two.tails$UE[1]),
  Upper.CI.two.tails= c(summapacis.list$ci.two.tails$LE[2], summapacis.list$ci.two.tails$ME[2], summapacis.list$ci.two.tails$UE[2]),
  P.value.upper= c(summapacis.list$p.values.upper$LE, summapacis.list$p.values.upper$ME, summapacis.list$p.values.upper$UE),
  P.value.lower= c(summapacis.list$p.values.lower$LE, summapacis.list$p.values.lower$ME, summapacis.list$p.values.lower$UE)
)
write.csv(summapacis.results, file = "summapacis.results.csv")

#Espeletia "bractless" complex
bractless.list <- results[["complejo_sin_bracteas"]]
bractless.results <- data.frame(
  Predictions = c("LE", "ME", "UE"),
  Observed.value = c(bractless.list$obs.diff$LE, bractless.list$obs.diff$ME, bractless.list$obs.diff$UE),
  Lower.CI= c(bractless.list$ci.one.tail$LE[1], bractless.list$ci.one.tail$ME[1], bractless.list$ci.one.tail$UE[1]),
  Upper.CI= c(bractless.list$ci.one.tail$LE[2], bractless.list$ci.one.tail$ME[2], bractless.list$ci.one.tail$UE[2]),
  Lower.CI.two.tails= c(bractless.list$ci.two.tails$LE[1], bractless.list$ci.two.tails$ME[1], bractless.list$ci.two.tails$UE[1]),
  Upper.CI.two.tails= c(bractless.list$ci.two.tails$LE[2], bractless.list$ci.two.tails$ME[2], bractless.list$ci.two.tails$UE[2]),
  P.value.upper= c(bractless.list$p.values.upper$LE, bractless.list$p.values.upper$ME, bractless.list$p.values.upper$UE),
  P.value.lower= c(summapacis.list$p.values.lower$LE, summapacis.list$p.values.lower$ME, summapacis.list$p.values.lower$UE)
)
write.csv(bractless.results, file = "bractless.results.csv")




