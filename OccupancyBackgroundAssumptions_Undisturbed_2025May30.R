###################################################################################################################
###################################################################################################################
###################################################################################################################
#
# INTRODUCTION
# This script is part of the supplementary materials for Altitudinal range shifts among high-elevation plant 
# species from the tropical Andes.. 
# 
# PURPOSE OF THIS SCRIPT
# Examine background assumptions related to tests of predictions derived from hypotheses proposing that Espeletia
# species from the páramo de Sumapaz are responding to global warming by expanding or leaning their geographic
# ranges, as discussed under different categories of range shifts by Lenoir and Svenning (2015. Ecography 38: 15-28).
#
# i) The trailing edge (lower altitudinal limit) of the species' distribution (or, at least, of the distribution of
# the younger cohort) is included within the range of elevations surveyed along the altitudinal gradient.
# ii) The leading edge  (upper altitudinal limit) of the species' distribution (or, at least, of the distribution 
# of the older cohort) is located before the upper limit of the realized altitudinal gradient.
# iii) The leading edge (upper altitudinal limit) of the species' distribution is at the upper limit of the 
# realized altitudinal gradient.
#
# Only undisturbed sampled cells are considered establish the occupancy of the species and the cohorts.
#
# DATA FILES REQUIRED TO RUN THIS SCRIPT
# i) "Occupancy_2025April15.csv", occupancy data.
# ii) "phenodata_species_taxa_determinations_2025March30.csv", herbarium specimen data with species and phenotypic group.
# iii) "DisturbioEstratos_2019Aug14.csv", pastures and crops dataset
#
# TABLE OF CONTENTS
# 1) Read and examine data.
# 2) Choose between executing the code for a single species (or morphological group) or executing the
#    code for several species (or morphological groups) using a "for" loop.
# 3) Extract occupancy data for a species or morphological group.
# 4) Read and exclude land use data (pasture and crop).
# 5) Define younger and older cohorts.
# 6) Estimate mean, minimum and maximum elevation across occupied sampling cells, for the species or morphological
#    group and for younger and older cohorts.
# 7) Null model.
# 8) Compare null model values with observed values.
# 9) Store the results for each species in a list when executed with a "for" loop for several species.
#
###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
# 1) Read and examine data.
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 1.1) Read and examine occupancy data.
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

alt<-unique(occupancy$Altitud)
min(alt)
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
unique.cells <- occupancy[match(unique(occupancy$Celda), occupancy$Celda),2:6] #total sampled cells
length(unique(unique.cells$Celda))

###################################################################################################################
# 1.2) Read and examine specimen data.
###################################################################################################################

#read and examine specimen data
pheno.group <- read.table("phenodata_species_taxa_determinations_2025April30.csv", header=TRUE, sep=",")
dim(pheno.group)
head(pheno.group)
summary(pheno.group)

###################################################################################################################
###################################################################################################################
# 2) Choose between executing the code for a single species (or morphological group) or executing the
#    code for several species (or morphological groups) using a "for" loop.
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 2.1) To execute the code for a single species or morphological group.
###################################################################################################################

#select the taxonomic species or phenotypic groups to test focal taxonimic group:
#"E. argentea", "E. cabrerensis", "E. grandiflora", "E. killipii", "E. miradorensis", 
#"E. summapacis", "complejo_sin_bracteas".

focal.sp <- "complejo_sin_bracteas"

#examine data for specimens under the selected specimen.
pheno.group[pheno.group$Species.Taxa.Assignment.For.Occupancy.Analysis==focal.sp,]
pheno.group[which(pheno.group$Species.Taxa.Assignment.For.Occupancy.Analysis==focal.sp),]  #avoiding NAs

#examine collection number for specimens under the selected specimen.
pheno.group$Corrected.Collector.Collection.Number[
  pheno.group$Species.Taxa.Assignment.For.Occupancy.Analysis==focal.sp]
pheno.group$Corrected.Collector.Collection.Number[
  which(pheno.group$Species.Taxa.Assignment.For.Occupancy.Analysis==focal.sp)] #avoiding NAs

##################################################################################################################
# 2.2) To execute the code for several species or morphological groups using a "for" loop.
##################################################################################################################

#Beginning of the loop to run the code several species
species.list <- c("E. grandiflora", "E. killipii", "E. summapacis", "complejo_sin_bracteas")
results.BA <- list()
for (focal.sp in species.list) {
  
  
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
  #print(occ.focal)
  head(occ.focal)
  dim(occ.focal)
  
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
  #print(younger)
  younger
  older <- names(categories) [6:12]
  #print(older)
  older
  
  #histogram of frequency for each size category
  category.sums <- colSums(occ.focal[, 8:19] >0, na.rm = TRUE)
  colors <- rep("grey", length(categories))
  names(colors) <- names(category.sums)
  colors[younger] <- "lightgreen" 
  colors[older] <- "darkgreen"  
  barplot(category.sums,
          ylab = "Frequency",
          col = colors,
          las = 2,
          cex.names = 0.5,
          main = focal.sp)
  legend("topright", 
         legend = c("Younger", "Gap", "Older"),
         fill = c("lightgreen", "grey", "darkgreen"),
         cex = 0.8)
  
  
  ###################################################################################################################
  ###################################################################################################################
  # 6) Estimate mean, minimum and maximum elevation across occupied sampling cells, for the species or morphological
  #    group and for younger and older cohorts.
  ###################################################################################################################
  ###################################################################################################################
  
  #total elevation range
  length(occ.focal$Altitud)
  summary(occ.focal$Altitud)
  hist(occ.focal$Altitud, xlim = range(3200 , 4300), xlab = "Elevation of all the species",
       main = focal.sp, breaks = seq(3100, 4200, 100))
  ME <- mean(occ.focal$Altitud)  # mean elevation
  ME
  LE <- min(occ.focal$Altitud)   # lower elevation limit
  LE
  UE <- max(occ.focal$Altitud)   # upper elevation limit
  UE
  
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
  # 7) Null model
  ###################################################################################################################
  ###################################################################################################################

###################################################################################################################
# 7.1) Prepare objects to run the null model.
###################################################################################################################

# Vector of elevations of all sampled cells
all.sampled.altitudes <- unique.cells$Altitud
summary(all.sampled.altitudes)


# Create occupancy matrix for size categories from presences only
om.p <- as.matrix(occ.focal[, 8:19]) 
n.present <- sum(rowSums(om.p) > 0)
n.cat.present <- colSums(om.p > 0)


#matrix with presences and absence cells
absences.focal.sp <- unique.cells[is.na(match(unique.cells$Celda, occ.focal$Celda)),]
head(absences.focal.sp)
dim(absences.focal.sp)
om.t <- rbind(om.p, matrix(0, nrow=nrow(absences.focal.sp), ncol=ncol(om.p)))
rownames(om.t) <- 1:nrow(om.t)
dim(om.t)

# Define number of rows = number of sampled cells
n.cells.total <- nrow(unique.cells)
Altitud <- unique.cells$Altitud

# Define number of iterations
k.max <- 10000

#create vectors to store null EM, LE and UE values for the species or morphological group
Null.ME <- rep(NA, times = k.max)
Null.LE <- rep(NA, times = k.max)
Null.UE <- rep(NA, times = k.max)

#create vectors to store null EM, LE and UE values for younger and older categories
Null.ME.younger <- rep(NA, times = k.max)
Null.LE.younger <- rep(NA, times = k.max)
Null.UE.younger <- rep(NA, times = k.max)

Null.ME.older <- rep(NA, times = k.max)
Null.LE.older <- rep(NA, times = k.max)
Null.UE.older <- rep(NA, times = k.max)

# define initial value of the iteration variable "k"
k <- 1

###################################################################################################################
# 7.2) Run null model.
###################################################################################################################

while(k <= k.max)
{
  #create null occupancy matrix, initially it will have zero in all entries
  Null.om <- om.t
  Null.om[,] <- 0
  
  #generate the null occupancy data for the species (or morphological group) 
  null.rows.present <- sample(1:nrow(om.t), n.present) 
  
  #generate the null occupancy data for each size category
  for(i in 1:ncol(Null.om)) 
  {
    Null.rows.cat.present <- sample(null.rows.present, n.cat.present[i])
    Null.om[Null.rows.cat.present, i] <- 1
  }
  
  #if the null occupancy matrix does not preserve the number of cells where the species is present,
  #go back to the beginning of the iteration
  if(sum(rowSums(Null.om) > 0) != n.present) next
 
  #convert null occupancy matrix to data.frame
  Null.om <- data.frame(Null.om)
  Null.om[1:5,younger]
  class(Null.om)
  
  #calculate null EM, LE and UE values for species or morphological group
  Null.ME[k] <- mean(Altitud[null.rows.present])  # mean elevation
  Null.LE[k] <- min(Altitud[null.rows.present])   # lower elevation limit
  Null.UE[k] <- max(Altitud[null.rows.present])   # upper elevation limit
  
  #calculate null EM, LE and UE values for younger category
  null.younger_present <- rowSums(Null.om[, younger]) > 0
  Null.ME.younger[k] <- mean(Altitud[null.younger_present])  # mean elevation
  Null.LE.younger[k] <- min(Altitud[null.younger_present])   # lower elevation limit
  Null.UE.younger[k] <- max(Altitud[null.younger_present])   # upper elevation limit
  
  #calculate null EM, LE and UE values for older category
  null.older_present <- rowSums(Null.om[, older]) > 0
  Null.ME.older[k] <- mean(Altitud[null.older_present])  # mean elevation
  Null.LE.older[k] <- min(Altitud[null.older_present])   # lower elevation limit
  Null.UE.older[k] <- max(Altitud[null.older_present])   # upper elevation limit
  
  #graph to monitor progression of null model iterations
  par(mar = c(5, 5, 4, 5) + 0.1)
  h.sampled <- hist(occupancy$Altitud, breaks = seq(3100, 4200, 100), xlim = c(3100, 4200), main = "", col = "gray", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  axis(4, cex.axis = 1.5, cex.lab = 1.5)
  axis(1, cex.axis = 1.5, cex.lab = 1.5)
  mtext("Sampled 30 x 30 m cells (bars)", side = 4, line = 2.5, cex = 1.5)
  mtext("Elevation (m)", side = 1, line = 2.5, cex = 1.5)
  par(new = T)
  h.PG <- hist(occ.focal$Altitud, breaks = seq(3100, 4200, 100), plot = F)
  h.null.PG <- hist(Altitud[null.rows.present], breaks = seq(3100, 4200, 100), plot = F)
  h.younger <- hist(younger.altitudes, breaks = seq(3100, 4200, 100), plot = F)
  h.null.younger <- hist(Altitud[null.younger_present], breaks = seq(3100, 4200, 100), plot = F)
  h.older <- hist(older.altitudes, breaks = seq(3100, 4200, 100), plot = F)
  h.null.older <- hist(Altitud[null.older_present], breaks = seq(3100, 4200, 100), plot = F)
  plot(seq(3150, 4150, 100), h.PG$counts/sum(h.sampled$counts), type = "o", xlim = c(3100, 4200),
       pch = 1, col = "lightgreen", cex=2, 
       xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  points(seq(3150, 4150, 100), h.younger$counts/sum(h.sampled$counts), type = "o", pch = 19, col = "lightgreen")
  points(seq(3150, 4150, 100), h.older$counts/sum(h.sampled$counts), type = "o", pch = 2, col = "lightgreen", cex=1.5)
  points(seq(3150, 4150, 100), h.null.PG$counts/sum(h.sampled$counts), type = "o", pch = 1, col = "blue", cex=2)
  points(seq(3150, 4150, 100), h.null.younger$counts/sum(h.sampled$counts), type = "o", pch = 19, col = "blue")
  points(seq(3150, 4150, 100), h.null.older$counts/sum(h.sampled$counts), type = "o", pch =2, col = "blue", cex=1.5)
  title(paste("Null model iteration = ", k), font.main=1)
  axis(2, cex.axis = 1.5, cex.lab = 1.5)
  mtext("Occupancy (lines and points)", side = 2, line = 3, cex = 1.5)
  abline(v = range(occ.focal$Altitud), lwd = 6, col = "gray90")
  abline(v = c(ME.younger, LE.younger, UE.younger), lty = 3, lwd = 3, col = "lightgreen")
  abline(v = c(Null.ME.younger[k], Null.LE.younger[k], Null.UE.younger[k]), lty = 3, lwd = 3, col = "blue")
  legend("topleft", c("Observed PG", "Observed younger", "Observed older", "Null model PG", "Null model younger", "Null model older", "PG range"),
         lty = 1, lwd = c(1, 1, 1, 1, 1, 1, 6), pch = c(1, 19, 2, 1, 19, 2, NA), pt.cex=c(2, 1, 1.5, 2, 1, 1.5, NA),
         col = c("lightgreen", "lightgreen", "lightgreen", "blue", "blue", "blue", "gray90"))
  
  
  
  #increase by one the value of the iteration variable
  k <- k + 1
}

###################################################################################################################
# 7.3) Examine null model results.
###################################################################################################################

#species or morphological group
length(Null.ME)
summary(Null.ME)
length(Null.LE)
summary(Null.LE)
length(Null.UE)
summary(Null.UE)

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

#Mean elevation (ME) limit
#species or morphological group
range(Null.ME) #examine range to determine histogram range
hist(Null.ME, 
     breaks=seq(3000, 4200, 100), 
     main= c(focal.sp, "Lower elevation limit: species")) #distribution of null model values
abline(v=ME, col="red") #observed value
sp.p.value.ME.upper <- sum(Null.ME >= ME) / k.max #upper tail p-value
sp.p.value.ME.lower <- sum(Null.ME <= ME) / k.max #upper tail p-value
print(paste("Species ME p-values: Upper =", sp.p.value.ME.upper, ", Lower =", sp.p.value.ME.lower))

#younger cohort
range(Null.ME.younger) #examine range to determine histogram range
hist(Null.ME.younger, 
     breaks=seq(3000, 4200, 100), 
     main= c(focal.sp, "Lower elevation limit: younger cohort")) #distribution of null model values
abline(v=ME.younger, col="red") #observed value
younger.p.value.ME.upper <- sum(Null.ME.younger >= ME.younger) / k.max #upper tail p-value
younger.p.value.ME.lower <- sum(Null.ME.younger <= ME.younger) / k.max #upper tail p-value
print(paste("Younger ME p-values: Upper =", younger.p.value.ME.upper, ", Lower =", younger.p.value.ME.lower))

#older cohort
range(Null.ME.older) #examine range to determine histogram range
hist(Null.ME.older, 
     breaks=seq(3000, 4200, 100), 
     main= c(focal.sp, "Lower elevation limit: older cohort")) #distribution of null model values
abline(v=ME.older, col="red") #observed value
older.p.value.ME.upper <- sum(Null.ME.older >= ME.older) / k.max #upper tail p-value
older.p.value.ME.lower <- sum(Null.ME.older <= ME.older) / k.max #upper tail p-value
print(paste("Older ME p-values: Upper =", older.p.value.ME.upper, ", Lower =", older.p.value.ME.lower))


#Lower elevation (LE) limit
#species or morphological group
range(Null.LE) #examine range to determine histogram range
hist(Null.LE, 
     breaks=seq(3000, 4200, 100), 
     main= c(focal.sp, "Lower elevation limit: species")) #distribution of null model values
abline(v=LE, col="red") #observed value
sp.p.value.LE.upper <- sum(Null.LE >= LE) / k.max #upper tail p-value
sp.p.value.LE.lower <- sum(Null.LE <= LE) / k.max #upper tail p-value
print(paste("Species LE p-values: Upper =", sp.p.value.LE.upper, ", Lower =", sp.p.value.LE.lower))

#younger cohort
range(Null.LE.younger) #examine range to determine histogram range
hist(Null.LE.younger, 
     breaks=seq(3000, 4200, 100), 
     main= c(focal.sp, "Lower elevation limit: younger cohort")) #distribution of null model values
abline(v=LE.younger, col="red") #observed value
younger.p.value.LE.upper <- sum(Null.LE.younger >= LE.younger) / k.max #upper tail p-value
younger.p.value.LE.lower <- sum(Null.LE.younger <= LE.younger) / k.max #upper tail p-value
print(paste("Younger LE p-values: Upper =", younger.p.value.LE.upper, ", Lower =", younger.p.value.LE.lower))

#older cohort
range(Null.LE.older) #examine range to determine histogram range
hist(Null.LE.older, 
     breaks=seq(3000, 4200, 100), 
     main= c(focal.sp, "Lower elevation limit: older cohort")) #distribution of null model values
abline(v=LE.older, col="red") #observed value
older.p.value.LE.upper <- sum(Null.LE.older >= LE.older) / k.max #upper tail p-value
older.p.value.LE.lower <- sum(Null.LE.older <= LE.older) / k.max #upper tail p-value
print(paste("Older LE p-values: Upper =", older.p.value.LE.upper, ", Lower =", older.p.value.LE.lower))


#Upper elevation (UE) limit
#species or morphological group
range(Null.UE) #examine range to determine histogram range
hist(Null.UE, 
     breaks=seq(3000, 4200, 100), 
     main= c(focal.sp, "Lower elevation limit: species")) #distribution of null model values
abline(v=UE, col="red") #observed value
sp.p.value.UE.upper <- sum(Null.UE >= UE) / k.max #upper tail p-value
sp.p.value.UE.lower <- sum(Null.UE <= UE) / k.max #upper tail p-value
print(paste("Species UE p-values: Upper =", sp.p.value.UE.upper, ", Lower =", sp.p.value.UE.lower))

#younger cohort
range(Null.UE.younger) #examine range to determine histogram range
hist(Null.UE.younger, 
     breaks=seq(3000, 4200, 100), 
     main= c(focal.sp, "Lower elevation limit: younger cohort")) #distribution of null model values
abline(v=UE.younger, col="red") #observed value
younger.p.value.UE.upper <- sum(Null.UE.younger >= UE.younger) / k.max #upper tail p-value
younger.p.value.UE.lower <- sum(Null.UE.younger <= UE.younger) / k.max #upper tail p-value
print(paste("Younger UE p-values: Upper =", younger.p.value.UE.upper, ", Lower =", younger.p.value.UE.lower))

#older cohort
range(Null.UE.older) #examine range to determine histogram range
hist(Null.UE.older, 
     breaks=seq(3000, 4200, 100), 
     main= c(focal.sp, "Lower elevation limit: older cohort")) #distribution of null model values
abline(v=UE.older, col="red") #observed value
older.p.value.UE.upper <- sum(Null.UE.older >= UE.older) / k.max #upper tail p-value
older.p.value.UE.lower <- sum(Null.UE.older <= UE.older) / k.max #upper tail p-value
print(paste("Older UE p-values: Upper =", older.p.value.UE.upper, ", Lower =", older.p.value.UE.lower))



###################################################################################################################
###################################################################################################################
# 9) Store the results for each species in a list when executed with a "for" loop for several species.
###################################################################################################################
###################################################################################################################

###################################################################################################################
# 9.1) store the results for each species in a list
###################################################################################################################

#store the results for each species in a list
results.BA[[focal.sp]] <- list(
  sp.obs.value = list(
    ME = ME,
    LE = LE,
    UE = UE
  ),
  sp.null.value = list(
    ME = Null.ME,
    LE = Null.LE,
    UE = Null.UE
  ),
  sp.p.values.upper = list(
    ME = sp.p.value.ME.upper,
    LE = sp.p.value.LE.upper,
    UE = sp.p.value.UE.upper
  ),
  sp.p.values.lower = list(
    ME = sp.p.value.ME.lower,
    LE = sp.p.value.LE.lower,
    UE = sp.p.value.UE.lower
  ),
  sp.ci.one.tail = list(
    ME = quantile(Null.ME, probs = c(0, 0.95)),
    LE = quantile(Null.LE, probs = c(0, 0.95)),
    UE = quantile(Null.UE, probs = c(0, 0.95))
  ),
  sp.ci.two.tails= list(
    ME = quantile(Null.ME, probs = c(0.025, 0.975)),
    LE = quantile(Null.LE, probs = c(0.025, 0.975)),
    UE = quantile(Null.UE, probs = c(0.025, 0.975))
  ),
  younger.obs.value = list(
    ME = ME.younger,
    LE = LE.younger,
    UE = UE.younger
  ),
  younger.null.value = list(
    ME = Null.ME.younger,
    LE = Null.LE.younger,
    UE = Null.UE.younger
  ),
  younger.p.value.upper = list(
    ME = younger.p.value.ME.upper,
    LE = younger.p.value.LE.upper,
    UE = younger.p.value.UE.upper
  ),
  younger.p.value.lower = list(
    ME = younger.p.value.ME.lower,
    LE = younger.p.value.LE.lower,
    UE = younger.p.value.UE.lower
  ),
  younger.ci.one.tail = list(
    ME = quantile(Null.ME.younger, probs = c(0, 0.95)),
    LE = quantile(Null.LE.younger, probs = c(0, 0.95)),
    UE = quantile(Null.UE.younger, probs = c(0, 0.95))
  ),
  younger.ci.two.tails= list(
    ME = quantile(Null.ME.younger, probs = c(0.025, 0.975)),
    LE = quantile(Null.LE.younger, probs = c(0.025, 0.975)),
    UE = quantile(Null.UE.younger, probs = c(0.025, 0.975))
  ),
  older.obs.value = list(
    ME = ME.older,
    LE = LE.older,
    UE = UE.older
  ),
  older.null.value = list(
    ME = Null.ME.older,
    LE = Null.LE.older,
    UE = Null.UE.older
  ),
  older.p.value.upper = list(
    ME = older.p.value.ME.upper,
    LE = older.p.value.LE.upper,
    UE = older.p.value.UE.upper
  ),
  older.p.value.lower = list(
    ME = older.p.value.ME.lower,
    LE = older.p.value.LE.lower,
    UE = older.p.value.UE.lower
  ),
  older.ci.one.tail = list(
    ME = quantile(Null.ME.older, probs = c(0, 0.95)),
    LE = quantile(Null.LE.older, probs = c(0, 0.95)),
    UE = quantile(Null.UE.older, probs = c(0, 0.95))
  ),
  older.ci.two.tails= list(
    ME = quantile(Null.ME.older, probs = c(0.025, 0.975)),
    LE = quantile(Null.LE.older, probs = c(0.025, 0.975)),
    UE = quantile(Null.UE.older, probs = c(0.025, 0.975))
  )
)
}

results.BA
#End of the "for" loop

###################################################################################################################
# 9.2) Store the summary of the results for each species in a data frame
###################################################################################################################

# Espeletia "grandiflora" complex
gr.list.BA <- results.BA[["E. grandiflora"]]
grandiflora.results.BA.undisturbed <- data.frame(
  Predictions = c("LE", "ME", "UE"),
  sp.Observed.value = c(gr.list.BA$sp.obs.value$LE, 
                        gr.list.BA$sp.obs.value$ME, 
                        gr.list.BA$sp.obs.value$UE),
  sp.Lower.CI = c(gr.list.BA$sp.ci.one.tail$LE[1],
                  gr.list.BA$sp.ci.one.tail$ME[1],
                  gr.list.BA$sp.ci.one.tail$UE[1]),
  sp.Upper.CI = c(gr.list.BA$sp.ci.one.tail$LE[2],
                  gr.list.BA$sp.ci.one.tail$ME[2],
                  gr.list.BA$sp.ci.one.tail$UE[2]),
  sp.Lower.CI.two.tails = c(gr.list.BA$sp.ci.two.tails$LE[1],
                            gr.list.BA$sp.ci.two.tails$ME[1],
                            gr.list.BA$sp.ci.two.tails$UE[1]),
  sp.Upper.CI.two.tails = c(gr.list.BA$sp.ci.two.tails$LE[2],
                            gr.list.BA$sp.ci.two.tails$ME[2],
                            gr.list.BA$sp.ci.two.tails$UE[2]),
  sp.P.value.upper = c(gr.list.BA$sp.p.values.upper$LE,
                       gr.list.BA$sp.p.values.upper$ME,
                       gr.list.BA$sp.p.values.upper$UE),
  sp.P.value.lower = c(gr.list.BA$sp.p.values.lower$LE,
                       gr.list.BA$sp.p.values.lower$ME,
                       gr.list.BA$sp.p.values.lower$UE),
  
  y.Observed.value = c(gr.list.BA$younger.obs.value$LE, 
                       gr.list.BA$younger.obs.value$ME, 
                       gr.list.BA$younger.obs.value$UE),
  y.Lower.CI = c(gr.list.BA$younger.ci.one.tail$LE[1],
                 gr.list.BA$younger.ci.one.tail$ME[1],
                 gr.list.BA$younger.ci.one.tail$UE[1]),
  y.Upper.CI = c(gr.list.BA$younger.ci.one.tail$LE[2],
                 gr.list.BA$younger.ci.one.tail$ME[2],
                 gr.list.BA$younger.ci.one.tail$UE[2]),
  y.Lower.CI.two.tails = c(gr.list.BA$younger.ci.two.tails$LE[1],
                           gr.list.BA$younger.ci.two.tails$ME[1],
                           gr.list.BA$younger.ci.two.tails$UE[1]),
  y.Upper.CI.two.tails = c(gr.list.BA$younger.ci.two.tails$LE[2],
                           gr.list.BA$younger.ci.two.tails$ME[2],
                           gr.list.BA$younger.ci.two.tails$UE[2]),
  y.P.value.upper = c(gr.list.BA$younger.p.value.upper$LE,
                      gr.list.BA$younger.p.value.upper$ME,
                      gr.list.BA$younger.p.value.upper$UE),
  y.P.value.lower = c(gr.list.BA$younger.p.value.lower$LE,
                      gr.list.BA$younger.p.value.lower$ME,
                      gr.list.BA$younger.p.value.lower$UE),
  
  o.Observed.value = c(gr.list.BA$older.obs.value$LE, 
                       gr.list.BA$older.obs.value$ME, 
                       gr.list.BA$older.obs.value$UE),
  o.Lower.CI = c(gr.list.BA$older.ci.one.tail$LE[1],
                 gr.list.BA$older.ci.one.tail$ME[1],
                 gr.list.BA$older.ci.one.tail$UE[1]),
  o.Upper.CI = c(gr.list.BA$older.ci.one.tail$LE[2],
                 gr.list.BA$older.ci.one.tail$ME[2],
                 gr.list.BA$older.ci.one.tail$UE[2]),
  o.Lower.CI.two.tails = c(gr.list.BA$older.ci.two.tails$LE[1],
                           gr.list.BA$older.ci.two.tails$ME[1],
                           gr.list.BA$older.ci.two.tails$UE[1]),
  o.Upper.CI.two.tails = c(gr.list.BA$older.ci.two.tails$LE[2],
                           gr.list.BA$older.ci.two.tails$ME[2],
                           gr.list.BA$older.ci.two.tails$UE[2]),
  o.P.value.upper = c(gr.list.BA$older.p.value.upper$LE,
                      gr.list.BA$older.p.value.upper$ME,
                      gr.list.BA$older.p.value.upper$UE),
  o.P.value.lower = c(gr.list.BA$older.p.value.lower$LE,
                      gr.list.BA$older.p.value.lower$ME,
                      gr.list.BA$older.p.value.lower$UE)
)

write.csv(grandiflora.results.BA.undisturbed, file = "grandiflora.results.BA.undisturbed.csv")

# Espeletia killipii
ki.list.BA <- results.BA[["E. killipii"]]
killipii.results.BA.undisturbed <- data.frame(
  Predictions = c("LE", "ME", "UE"),
  sp.Observed.value = c(ki.list.BA$sp.obs.value$LE, 
                        ki.list.BA$sp.obs.value$ME, 
                        ki.list.BA$sp.obs.value$UE),
  sp.Lower.CI = c(ki.list.BA$sp.ci.one.tail$LE[1],
                  ki.list.BA$sp.ci.one.tail$ME[1],
                  ki.list.BA$sp.ci.one.tail$UE[1]),
  sp.Upper.CI = c(ki.list.BA$sp.ci.one.tail$LE[2],
                  ki.list.BA$sp.ci.one.tail$ME[2],
                  ki.list.BA$sp.ci.one.tail$UE[2]),
  sp.Lower.CI.two.tails = c(ki.list.BA$sp.ci.two.tails$LE[1],
                            ki.list.BA$sp.ci.two.tails$ME[1],
                            ki.list.BA$sp.ci.two.tails$UE[1]),
  sp.Upper.CI.two.tails = c(ki.list.BA$sp.ci.two.tails$LE[2],
                            ki.list.BA$sp.ci.two.tails$ME[2],
                            ki.list.BA$sp.ci.two.tails$UE[2]),
  sp.P.value.upper = c(ki.list.BA$sp.p.values.upper$LE,
                       ki.list.BA$sp.p.values.upper$ME,
                       ki.list.BA$sp.p.values.upper$UE),
  sp.P.value.lower = c(ki.list.BA$sp.p.values.lower$LE,
                       ki.list.BA$sp.p.values.lower$ME,
                       ki.list.BA$sp.p.values.lower$UE),
  
  y.Observed.value = c(ki.list.BA$younger.obs.value$LE, 
                       ki.list.BA$younger.obs.value$ME, 
                       ki.list.BA$younger.obs.value$UE),
  y.Lower.CI = c(ki.list.BA$younger.ci.one.tail$LE[1],
                 ki.list.BA$younger.ci.one.tail$ME[1],
                 ki.list.BA$younger.ci.one.tail$UE[1]),
  y.Upper.CI = c(ki.list.BA$younger.ci.one.tail$LE[2],
                 ki.list.BA$younger.ci.one.tail$ME[2],
                 ki.list.BA$younger.ci.one.tail$UE[2]),
  y.Lower.CI.two.tails = c(ki.list.BA$younger.ci.two.tails$LE[1],
                           ki.list.BA$younger.ci.two.tails$ME[1],
                           ki.list.BA$younger.ci.two.tails$UE[1]),
  y.Upper.CI.two.tails = c(ki.list.BA$younger.ci.two.tails$LE[2],
                           ki.list.BA$younger.ci.two.tails$ME[2],
                           ki.list.BA$younger.ci.two.tails$UE[2]),
  y.P.value.upper = c(ki.list.BA$younger.p.value.upper$LE,
                      ki.list.BA$younger.p.value.upper$ME,
                      ki.list.BA$younger.p.value.upper$UE),
  y.P.value.lower = c(ki.list.BA$younger.p.value.lower$LE,
                      ki.list.BA$younger.p.value.lower$ME,
                      ki.list.BA$younger.p.value.lower$UE),
  
  o.Observed.value = c(ki.list.BA$older.obs.value$LE, 
                       ki.list.BA$older.obs.value$ME, 
                       ki.list.BA$older.obs.value$UE),
  o.Lower.CI = c(ki.list.BA$older.ci.one.tail$LE[1],
                 ki.list.BA$older.ci.one.tail$ME[1],
                 ki.list.BA$older.ci.one.tail$UE[1]),
  o.Upper.CI = c(ki.list.BA$older.ci.one.tail$LE[2],
                 ki.list.BA$older.ci.one.tail$ME[2],
                 ki.list.BA$older.ci.one.tail$UE[2]),
  o.Lower.CI.two.tails = c(ki.list.BA$older.ci.two.tails$LE[1],
                           ki.list.BA$older.ci.two.tails$ME[1],
                           ki.list.BA$older.ci.two.tails$UE[1]),
  o.Upper.CI.two.tails = c(ki.list.BA$older.ci.two.tails$LE[2],
                           ki.list.BA$older.ci.two.tails$ME[2],
                           ki.list.BA$older.ci.two.tails$UE[2]),
  o.P.value.upper = c(ki.list.BA$older.p.value.upper$LE,
                      ki.list.BA$older.p.value.upper$ME,
                      ki.list.BA$older.p.value.upper$UE),
  o.P.value.lower = c(ki.list.BA$older.p.value.lower$LE,
                      ki.list.BA$older.p.value.lower$ME,
                      ki.list.BA$older.p.value.lower$UE)
)

write.csv(killipii.results.BA.undisturbed, file = "killipii.results.BA.undisturbed.csv")


# Espeletia summapacis
su.list.BA <- results.BA[["E. summapacis"]]
summapacis.results.BA.undisturbed <- data.frame(
  Predictions = c("LE", "ME", "UE"),
  sp.Observed.value = c(su.list.BA$sp.obs.value$LE, 
                        su.list.BA$sp.obs.value$ME, 
                        su.list.BA$sp.obs.value$UE),
  sp.Lower.CI = c(su.list.BA$sp.ci.one.tail$LE[1],
                  su.list.BA$sp.ci.one.tail$ME[1],
                  su.list.BA$sp.ci.one.tail$UE[1]),
  sp.Upper.CI = c(su.list.BA$sp.ci.one.tail$LE[2],
                  su.list.BA$sp.ci.one.tail$ME[2],
                  su.list.BA$sp.ci.one.tail$UE[2]),
  sp.Lower.CI.two.tails = c(su.list.BA$sp.ci.two.tails$LE[1],
                            su.list.BA$sp.ci.two.tails$ME[1],
                            su.list.BA$sp.ci.two.tails$UE[1]),
  sp.Upper.CI.two.tails = c(su.list.BA$sp.ci.two.tails$LE[2],
                            su.list.BA$sp.ci.two.tails$ME[2],
                            su.list.BA$sp.ci.two.tails$UE[2]),
  sp.P.value.upper = c(su.list.BA$sp.p.values.upper$LE,
                       su.list.BA$sp.p.values.upper$ME,
                       su.list.BA$sp.p.values.upper$UE),
  sp.P.value.lower = c(su.list.BA$sp.p.values.lower$LE,
                       su.list.BA$sp.p.values.lower$ME,
                       su.list.BA$sp.p.values.lower$UE),
  
  y.Observed.value = c(su.list.BA$younger.obs.value$LE, 
                       su.list.BA$younger.obs.value$ME, 
                       su.list.BA$younger.obs.value$UE),
  y.Lower.CI = c(su.list.BA$younger.ci.one.tail$LE[1],
                 su.list.BA$younger.ci.one.tail$ME[1],
                 su.list.BA$younger.ci.one.tail$UE[1]),
  y.Upper.CI = c(su.list.BA$younger.ci.one.tail$LE[2],
                 su.list.BA$younger.ci.one.tail$ME[2],
                 su.list.BA$younger.ci.one.tail$UE[2]),
  y.Lower.CI.two.tails = c(su.list.BA$younger.ci.two.tails$LE[1],
                           su.list.BA$younger.ci.two.tails$ME[1],
                           su.list.BA$younger.ci.two.tails$UE[1]),
  y.Upper.CI.two.tails = c(su.list.BA$younger.ci.two.tails$LE[2],
                           su.list.BA$younger.ci.two.tails$ME[2],
                           su.list.BA$younger.ci.two.tails$UE[2]),
  y.P.value.upper = c(su.list.BA$younger.p.value.upper$LE,
                      su.list.BA$younger.p.value.upper$ME,
                      su.list.BA$younger.p.value.upper$UE),
  y.P.value.lower = c(su.list.BA$younger.p.value.lower$LE,
                      su.list.BA$younger.p.value.lower$ME,
                      su.list.BA$younger.p.value.lower$UE),
  
  o.Observed.value = c(su.list.BA$older.obs.value$LE, 
                       su.list.BA$older.obs.value$ME, 
                       su.list.BA$older.obs.value$UE),
  o.Lower.CI = c(su.list.BA$older.ci.one.tail$LE[1],
                 su.list.BA$older.ci.one.tail$ME[1],
                 su.list.BA$older.ci.one.tail$UE[1]),
  o.Upper.CI = c(su.list.BA$older.ci.one.tail$LE[2],
                 su.list.BA$older.ci.one.tail$ME[2],
                 su.list.BA$older.ci.one.tail$UE[2]),
  o.Lower.CI.two.tails = c(su.list.BA$older.ci.two.tails$LE[1],
                           su.list.BA$older.ci.two.tails$ME[1],
                           su.list.BA$older.ci.two.tails$UE[1]),
  o.Upper.CI.two.tails = c(su.list.BA$older.ci.two.tails$LE[2],
                           su.list.BA$older.ci.two.tails$ME[2],
                           su.list.BA$older.ci.two.tails$UE[2]),
  o.P.value.upper = c(su.list.BA$older.p.value.upper$LE,
                      su.list.BA$older.p.value.upper$ME,
                      su.list.BA$older.p.value.upper$UE),
  o.P.value.lower = c(su.list.BA$older.p.value.lower$LE,
                      su.list.BA$older.p.value.lower$ME,
                      su.list.BA$older.p.value.lower$UE)
)

write.csv(summapacis.results.BA.undisturbed, file = "summapacis.results.BA.undisturbed.csv")


# Espeletia "bractless" complex
br.list.BA <- results.BA[["complejo_sin_bracteas"]]
bractless.results.BA.undisturbed <- data.frame(
  Predictions = c("LE", "ME", "UE"),
  sp.Observed.value = c(br.list.BA$sp.obs.value$LE, 
                        br.list.BA$sp.obs.value$ME, 
                        br.list.BA$sp.obs.value$UE),
  sp.Lower.CI = c(br.list.BA$sp.ci.one.tail$LE[1],
                  br.list.BA$sp.ci.one.tail$ME[1],
                  br.list.BA$sp.ci.one.tail$UE[1]),
  sp.Upper.CI = c(br.list.BA$sp.ci.one.tail$LE[2],
                  br.list.BA$sp.ci.one.tail$ME[2],
                  br.list.BA$sp.ci.one.tail$UE[2]),
  sp.Lower.CI.two.tails = c(br.list.BA$sp.ci.two.tails$LE[1],
                            br.list.BA$sp.ci.two.tails$ME[1],
                            br.list.BA$sp.ci.two.tails$UE[1]),
  sp.Upper.CI.two.tails = c(br.list.BA$sp.ci.two.tails$LE[2],
                            br.list.BA$sp.ci.two.tails$ME[2],
                            br.list.BA$sp.ci.two.tails$UE[2]),
  sp.P.value.upper = c(br.list.BA$sp.p.values.upper$LE,
                       br.list.BA$sp.p.values.upper$ME,
                       br.list.BA$sp.p.values.upper$UE),
  sp.P.value.lower = c(br.list.BA$sp.p.values.lower$LE,
                       br.list.BA$sp.p.values.lower$ME,
                       br.list.BA$sp.p.values.lower$UE),
  
  y.Observed.value = c(br.list.BA$younger.obs.value$LE, 
                       br.list.BA$younger.obs.value$ME, 
                       br.list.BA$younger.obs.value$UE),
  y.Lower.CI = c(br.list.BA$younger.ci.one.tail$LE[1],
                 br.list.BA$younger.ci.one.tail$ME[1],
                 br.list.BA$younger.ci.one.tail$UE[1]),
  y.Upper.CI = c(br.list.BA$younger.ci.one.tail$LE[2],
                 br.list.BA$younger.ci.one.tail$ME[2],
                 br.list.BA$younger.ci.one.tail$UE[2]),
  y.Lower.CI.two.tails = c(br.list.BA$younger.ci.two.tails$LE[1],
                           br.list.BA$younger.ci.two.tails$ME[1],
                           br.list.BA$younger.ci.two.tails$UE[1]),
  y.Upper.CI.two.tails = c(br.list.BA$younger.ci.two.tails$LE[2],
                           br.list.BA$younger.ci.two.tails$ME[2],
                           br.list.BA$younger.ci.two.tails$UE[2]),
  y.P.value.upper = c(br.list.BA$younger.p.value.upper$LE,
                      br.list.BA$younger.p.value.upper$ME,
                      br.list.BA$younger.p.value.upper$UE),
  y.P.value.lower = c(br.list.BA$younger.p.value.lower$LE,
                      br.list.BA$younger.p.value.lower$ME,
                      br.list.BA$younger.p.value.lower$UE),
  
  o.Observed.value = c(br.list.BA$older.obs.value$LE, 
                       br.list.BA$older.obs.value$ME, 
                       br.list.BA$older.obs.value$UE),
  o.Lower.CI = c(br.list.BA$older.ci.one.tail$LE[1],
                 br.list.BA$older.ci.one.tail$ME[1],
                 br.list.BA$older.ci.one.tail$UE[1]),
  o.Upper.CI = c(br.list.BA$older.ci.one.tail$LE[2],
                 br.list.BA$older.ci.one.tail$ME[2],
                 br.list.BA$older.ci.one.tail$UE[2]),
  o.Lower.CI.two.tails = c(br.list.BA$older.ci.two.tails$LE[1],
                           br.list.BA$older.ci.two.tails$ME[1],
                           br.list.BA$older.ci.two.tails$UE[1]),
  o.Upper.CI.two.tails = c(br.list.BA$older.ci.two.tails$LE[2],
                           br.list.BA$older.ci.two.tails$ME[2],
                           br.list.BA$older.ci.two.tails$UE[2]),
  o.P.value.upper = c(br.list.BA$older.p.value.upper$LE,
                      br.list.BA$older.p.value.upper$ME,
                      br.list.BA$older.p.value.upper$UE),
  o.P.value.lower = c(br.list.BA$older.p.value.lower$LE,
                      br.list.BA$older.p.value.lower$ME,
                      br.list.BA$older.p.value.lower$UE)
)

write.csv(bractless.results.BA.undisturbed, file = "bractless.results.BA.undisturbed.csv")

