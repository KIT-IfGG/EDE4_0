################################################################################
# SDM and uncertainty for climate
################################################################################
require(raster)
require(sp)
require(raster)
require(rgdal)
require(dismo)
require(rJava)
require(maptools)
require(maxnet)

# CREATE A VIRTUAL SPECIES
# http://borisleroy.com/virtualspecies_tutorial/
require(virtualspecies)


bioclimData <-  getData("worldclim", var = "bio", res=2.5)
# bioclim temperature variables are ºC*10
bioTemperature <- c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11) # 3 is division; units not important
plot(bioclimData$bio1/10)

# Define the area of interest
# Not only climatic is important, but also assume that there geographical barriers
geographic.extent <- c(-20, 70, 10, 80) #lat (x), lon(y) From google maps(?)
bioclimData.aoi <- crop(bioclimData, geographic.extent)
plot(bioclimData.aoi$bio1/10)

# define response
bio1 <- as.data.frame (bioclimData.aoi$bio1, xy=TRUE)
bio1 <- bio1[!is.na(bio1$bio1),]
summary(bio1)
# Distribution of values in the area of interest
plot(density(bioclimData.aoi$bio1$bio1/10))
mean(bio1$bio1/10)
# bell-shaped functions / gaussian distributions functions
# Suitability of the environment for bio1 = 15 °C
dnorm(x = 15, mean = mean(bio1$bio1/10), sd = sd(bio1$bio1/10))
# Define the climate space for the species
# Check on Dorman's work for more ideas
# Now just broad
bioVar <- paste("bio",c(1,5,11,12,13,14,18),sep="")
for(i in 1:length(bioVar)){
  plot(bioclimData.aoi[[bioVar[i]]],  main = bioVar[i])
  density(bioclimData.aoi[[bioVar[i]]], main = bioVar[i])
  
  mtext(paste("sd:", sd(as.vector(bioclimData.aoi[[bioVar[i]]]), na.rm = T)), side = 3)
  
}
# Self define functions
myResponses <- formatFunctions(bio1 = c(fun = 'dnorm', mean = 110, sd = 80), # Annual Mean Temperature
                                 bio5 = c(fun = 'dnorm', mean = 250, sd = 90), # Max Temperature of Warmest Month
                                 bio11 = c(fun = 'dnorm', mean = 15, sd = 100), #  Mean Temperature of Coldest Quarter
                                 bio12 = c(fun = 'dnorm', mean = 800, sd = 330), #Annual Precipitation
                                 bio13 =  c(fun = 'dnorm', mean = 150, sd = 120), # Precipitation of Wettest Month
                                 bio14 = c(fun = 'dnorm', mean = 50, sd = 40),# Precipitation of Driest Month
                                 bio18 = c(fun = 'dnorm', mean = 175, sd = 90) #Precipitation of Warmest Quarter
                                 )

speciesA <-generateSpFromFun(raster.stack =bioclimData.aoi[[bioVar]],
                             parameters = myResponses, plot = TRUE)


# Integrated pca functionality
myStack <- bioclimData.aoi[[bioVar]]
speciesB <- generateSpFromPCA(raster.stack = myStack, sample.points = TRUE, nb.points = 5000) 
# Consider using sample.points = TRUE, nb.points = 5000, niche.breadth = "narrow")

# Random species
speciesC <- generateRandomSp(bioclimData.aoi[[c("bio1", "bio12", "bio14", "bio18")]], relations = c("gaussian", "logistic"),
                             convert.to.PA = FALSE)
# See reponses
plotResponse(speciesA)
plotResponse(speciesB)
plotResponse(speciesC)


# CONVERT VIRTUAL SPECIES INTO PRESENCE-ABSENCE DATA
# Do not use threshold, but use linear or logistic or else
# Run the conversion to presence-absence (Consider using prevalence)
speciesA
#pa1 <- convertToPA(speciesA, PA.method = "probability", prob.method = "linear",a = 1, b = 0, plot = TRUE)
speciesA.PA <- convertToPA(speciesA, PA.method = "probability",prob.method = "logistic",
                   beta = 0.65, alpha = -0.07, plot = TRUE)
speciesB.PA <- convertToPA(speciesB, PA.method = "probability",prob.method = "logistic",
                           beta = 0.65, alpha = -0.07, plot = TRUE)
speciesC.PA <- convertToPA(speciesC, PA.method = "probability",prob.method = "logistic",
                           beta = 0.65, alpha = -0.07, plot = TRUE)


# See reponses
plot(speciesA.PA)
speciesA.PA <- limitDistribution(speciesA.PA, geographical.limit = "continent",
                  area = "Eurasia", plot = TRUE)
plot(speciesB.PA)
speciesB.PA <- limitDistribution(speciesB.PA, geographical.limit = "continent",
                                 area = "Eurasia", plot = TRUE)

plot(speciesC.PA)
speciesC.PA <- limitDistribution(speciesC.PA, geographical.limit = "continent",
                                 area = "Eurasia", plot = TRUE)


saveRDS(speciesA.PA, file = "speciesA.PA.RDS")
saveRDS(speciesB.PA, file = "speciesB.PA.RDS")
saveRDS(speciesC.PA, file = "speciesC.PA.RDS")


# CREATE A PREDICTOR DATASET
# predictors
bioclimData.aoi
saveRDS(bioclimData.aoi, file = "bioclimData.aoi.RDS")

# Presence and absence points
P.points <- sampleOccurrences(speciesA.PA, n = 900, extract.probability = TRUE,
                              sampling.area = "Europe",
                              detection.probability = 0.9, # Sampling bias
                              correct.by.suitability = TRUE,
                              error.probability = 0,
                              type = "presence only")

PA.points <- sampleOccurrences(speciesA.PA, n = 1000, extract.probability = TRUE,
                               sampling.area = "Europe",
                               detection.probability = 0.9, # Sampling bias
                               correct.by.suitability = TRUE,
                               error.probability = 0,
                               type = "presence-absence")


head(PA.points$sample.points)
presence1 <- P.points$sample.points[P.points$sample.points$Observed==1, c("x", "y")]
presence2 <- PA.points$sample.points[PA.points$sample.points$Observed==1, c("x", "y")]
presence <- rbind(presence1, presence2)
absence <- PA.points$sample.points[PA.points$sample.points$Observed==0, c("x", "y")]

plot(speciesA.PA$probability.of.occurrence)
points(presence, col = "blue", cex= 0.5)
points(absence, col = "red", cex= 0.5)

### Extracting values from rasters
presvals <- extract(bioclimData.aoi, presence)
absvals <- extract(bioclimData.aoi, absence)
head(presvals)
# remove any NAs
presence <- presence[!is.na(rowSums(presvals)),]
absence <- absence[!is.na(rowSums(absvals)),]
presvals <- extract(bioclimData.aoi, presence)
absvals <- extract(bioclimData.aoi, absence)
any(is.na(rowSums(presvals)))
any(is.na(rowSums(absvals)))

### Create predictor dataset
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
head(sdmdata)
tail(sdmdata)
### Correlation among predictors
require(corrplot)
pairs(sdmdata[,2:12], cex=0.1, fig=TRUE)
corrplot(cor(sdmdata[,2:12]), type= "full", diag=FALSE, method="number")
# bio 4 and bio7 both reflect the seasonality, drop one
# bio 5 (month) and bio 10 (quarter) refer to warm periods,
# same applies for bio 6 and  bio 11
pairs(sdmdata[,13:ncol(sdmdata)], cex=0.1, fig=TRUE)
corrplot(cor(sdmdata[,13:ncol(sdmdata)], method = "pearson" ), type= "full", diag=FALSE,method="number")
# bio 13 (month) and bio16(quarter) refer to wet periods
# same applies for bio 14  and  bio 17
# Drop the month variable and
biovarDrop <- paste("bio", c(7, 5, 6, 13, 14), sep = "")
biovarKeep <- names(bioclimData.aoi)[!names(bioclimData.aoi)%in% biovarDrop]
predictors <- bioclimData.aoi[[biovarKeep]]
# Do PCA
require(RStoolbox)
pcaOut <- rasterPCA(predictors,  nComp = nlayers(predictors))
pcaOut
summary(pcaOut$model)
plot(pcaOut$map)
pcaOut$model

pcaStack <- pcaOut$map[[paste("PC",c(1:4), sep = "")]]
# Get Europe
europe <- lapply(ctry, FUN = function(x) getData("GADM", country = x, level= 1))
europeCoords <- do.call("rbind", europe)
                 
plot(europe)
# Member States of the European Union
ctry <- c("ALB", "AND", "ARE", "ARM", "AUT", "AZE", "BEL", "BGR", "BIH", "BLR", "CHE", 
          "CYP", "CZE", "DEU", "DNK", "DZA", "EGY", "ESH", "ESP", "EST", 
          "FIN", "FRA", "FRO", "GBR", "GEO", "GGY",  "GRC",  "GRL", "HRV", 
          "HUN", "IMN", "IRL", "IRN", "IRQ", "ISL", "ISR", "ITA", "JOR", "KAZ", 
          "LBN", "LBY", "LIE", "LTU", "LUX", "LVA", "MAR", "MCO", "MDA", "MKD", 
          "MLI", "MLT", "MNE", "MRT", "NER", "NLD", "NOR", "POL", "PRT", "QAT", 
          "ROU", "SAU", "SDN", "SEN", "SJM", "SMR", "SRB", "SVK", "SVN", "SWE",
          "SYR", "TCD", "TKM", "TUN", "TUR", "UKR", "UZB", "VAT", "XKO")
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom")
# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME%in%europeanUnion)
# Extract longitude and latitude border's coordinates of members states of E.U. 
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)
plot(europeCoords)

set.seed(123)
speciesD <- generateRandomSp(pcaStack, relations = c("gaussian"),
                 convert.to.PA = FALSE)



pcaOut$map

#BAses on 0.7 criteria
head(sdmdata)
head(sdmdata[,biovarKeep])

corrplot(cor(sdmdata[,biovarKeep], method = "pearson" ), type= "low", diag=FALSE,method="number")
biovarDrop <- c(biovarDrop, paste("bio", c(3, 9, 6, 13, 14))

### Climatic envelope model with presence-only data (P data)
bc <- bioclim(presvals[,biovarKeep])
bc
response(bc)

### Model evaluation and quality ####
ths <- seq(0,1, len=50)
e <- evaluate(p=presvals, a=absvals, bc, tr=ths)
plot(e,'ROC')
boxplot(e, col=c("blue", "red"), main="Probs")
density(e)
### Exkurs
dat <- sdmdata[,c("pb", biovarKeep)]
dat$pred <- predict(bc, dat)   ### Calculate modelled "probability" of occurrance. "Prediction".
names(dat)
dat[1:10,c("pb", "pred")]

par(mfrow=c(1,2))
boxplot(pred ~ pb, dat, col=c("red", "blue"))
grid()
#stripchart(pred ~ pb, dat)
pcol <- rgb(0,0,1,0.4)
acol <- rgb(1,0,0,0.6)
hist(dat$pred[dat$pb==0], col=acol, border="red", freq=F, main="")
hist(dat$pred[dat$pb==1], add=TRUE, col=pcol, border="white", freq=F)
legend("topright", legend=c("Presences", "Absences"), col=c(pcol, acol), pch=15, pt.cex=2)
grid()
box()
par(mfrow=c(1,1))

par(mfrow=c(1,2))
hist(dat$pred[dat$pb ==1])
hist(dat$pred[dat$pb ==0])
par(mfrow=c(1,1))
hlpr <- cbind.data.frame(FPR=e@FPR, TPR=e@TPR,  labels=ths)
hlpr <- round(hlpr, 2)
d <- duplicated(hlpr[,c("FPR", "TPR")])
table(d)
hlpr <- hlpr[!d,]

par(mfrow=c(1,1))
plot(e,'ROC', type="l")
text(hlpr$FPR, hlpr$TPR,  labels=hlpr$labels)
th <- 0.3   ### Not a nice example...
ths <- threshold(e); ths
th <- ths$kappa

mycolors <- c("lightgrey", "blue")
species_distribution <- predict(predictors, bc) 
species_distribution[species_distribution[]<th] <- 0
species_distribution[species_distribution[]>=th] <- 1

par(mfrow=c(1,2))
plot(species_distribution, main=paste0("Threshold = ", round(th,2)), col=mycolors, legend=FALSE)
legend("topright", legend=c("Species habitat", "No habitat"), col=mycolors, pch=15)
plot(speciesA.PA$probability.of.occurrence)

### Validation: k-fold data partitioning for BC ####
pres <- sdmdata[sdmdata[,1] == 1, 2:ncol(sdmdata)]
back <- sdmdata[sdmdata[,1] == 0, 2:ncol(sdmdata)]

k <- 5
group <- kfold(pres, k)

e <- vector(mode= "list", length = k)
for (i in 1:k) {
  train <- pres[group != i,]    # calibration dataset, training data
  test <- pres[group == i,]    # test dataset, validation dataset
  bc <- bioclim(train)
  e[[i]] <- evaluate(p=test, a=back, bc)
}

auc <- sapply(e, function(x) x@auc)
round(mean(auc),2)
round(sd(auc),2)
round(cv(auc),2)
round(median(auc),2)



# CREATE, SELECT VALIDATE MODEL MAXENT
ME.speciesA <- maxent(x=predictors, p=presence, a=absence)
response(ME.speciesA)
density(ME.speciesA)
# ME.speciesA in Browser

### Validation: k-fold data partitioning ####
k <- 5
group <- kfold(presence, k)

e <- vector(mode= "list", length = k)
maps <- vector(mode= "list", length = k)
for (i in 1:k) {
  train <- presence[group != i,]    # calibration dataset, training data
  test <- presence[group == i,]    # test dataset, validation dataset
  me  <- maxent(predictors, train, absence)
  e[[i]] <- evaluate(p=test, a=presence, model = me, x = predictors)
  maps[[i]] <-  predict(predictors, me)
}

auc <- sapply(e, function(x) x@auc)
round(mean(auc),2)
round(sd(auc),2)
round(cv(auc),2)
round(median(auc),2)

# ITERATION
### Validation: Repeated data partitioning ####
niter <- 10  ### Should be about 100, 30 is also ok.

maps <- vector(mode= "list", length = niter)
# Review sampling size
samplingSize <- 0.3
for (i in 1:niter){
  indexes <- sample.int(nrow(presence)*samplingSize, replace=TRUE)  ### 30% of the data are test/validation data.
  me <- maxent(predictors, presence[indexes,], absence)
  maps[[i]] <-  predict(predictors, me)
}

# PREDICT ON PREDICTORS (CURRENT SITUTATION) AND COMPARE TO VIRTUAL SPECIES DATA
predictions.map <- stack(maps)
cv.map <- calc(predictions.map, cv)
meanPrediction <- calc(predictions.map, mean)
medianPrediction <- calc(predictions.map, median)

require(rasterVis)
par(mfrow=c(1,2))
colors <- RColorBrewer::brewer.pal(8, "RdYlGn")
HScolors <- c("#e7e6e6", "#d2d2d2", colors)
myTheme <- rasterTheme(region=HScolors)
levelplot(medianPrediction, main="Median prediction", margin = NA, contour=F, par.settings = myTheme  )
colors <- RColorBrewer::brewer.pal(5, "OrRd")
myTheme <- rasterTheme(region=colors)
levelplot(cv.map, main="CV for predictions", contour=F, margin = NA, par.settings = myTheme)


# PREDICT ON PREDICTORS (CURRENT SITUTATION) AND COMPARE TO VIRTUAL SPECIES DATA
predictionCurrent <- predict(ME.speciesA, bioclimData.aoi, type= "response")




# PREDICTION (REFERENCE PREDICTION)

# use the mean values




# PREPARE DATA FOR PREDICTION


# COMPARE CLIMATE DATA 
# histogram, mean and sd






# ### Prediction uncertainty ####





# DEFINE METRIC FOR MEASURING THE DIFFERENCE


# CALCULATE YEARLY VARIABILITY


# CALCULATE ENSEMBLES VARIABILITY





