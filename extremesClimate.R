################################################################################
# Calculate extremes as Stewart et al 2021
################################################################################
# load require libraries
require(raster)
require(dismo)
require(ncdf4)
# load parallel package
require(parallel)
# set a different temp dir to avoid problesm with space or files being deleted
rasterOptions(tmpdir="F:/rTemp")

################################################################################


################################################################################
# Define the functions to calculate extrems
# Estrems larger than mean
maxExtremes <- function(x, returnPeriod = c(5, 10, 15, 20, 25, 30)) {
  if(any(is.na(x))) {
    return(rep(NA,2+(length(returnPeriod))))
  } else {
    u = mean(x)
    sd = sd(x)
    p = 1-(1/returnPeriod)
    q = quantile(x,probs = p)
    ex = as.numeric(q-u)
    return(c(u,sd,ex))
  }
}
# Extrems smaller than mean
minExtremes <- function(x, returnPeriod = c(5, 10, 15, 20, 25, 30)) {
  if(any(is.na(x))) {
    return(rep(NA,2+(length(returnPeriod))))
  } else {
    u = mean(x)
    sd = sd(x)
    p = 1/returnPeriod
    q = quantile(x,probs = p)
    ex = as.numeric(q-u)
    return(c(u,sd,ex))
  }
}
################################################################################
# BIOCLIM EXTREMS FROM WORLDCLIM MONTHLY DATA
################################################################################

################################################################################
# Calculate BIO6 = Min Temperature of Coldest Month
# load data
tminHist <- readRDS(file =  "E:/EDE_Daten/worldClimMonth/wc2.1_2.5m_tmin_1961_2018.rds" )
# Build index for calculation --> Dont break  winters
x <- names(tminHist)[1]
n_last <- 7
layerDates <- sapply(names(tminHist), function(x) paste(substr(x, nchar(x) - n_last + 1, nchar(x)), ".15", sep = "" ))
names(layerDates) <- NULL
index = data.frame(band = 1:nlayers(tminHist), date = as.Date(layerDates, format = "%Y.%m.%d"))
index$summer_yr <-  as.numeric(format(index$date,"%Y"))
index$winter_yr <- as.numeric(format(index$date,"%Y"))
head(index, 20)
index[which(format(index$date,"%m") %in% c("07","08","09","10","11","12")),
      "winter_yr"] <-  index[which(format(index$date,"%m") %in% c("07","08","09","10",
                                                                  "11","12")),"winter_yr"] + 1
head(index, 20)
# Create a data holder
years <- unique(index$winter_yr)
dummyTMNC <- vector(mode = "list", length = length(years))
names(dummyTMNC) <- years
cat("Calculating for...")
for (i in 1:length(years)){
  cat(years[i]);cat("...")
  tmp <- tminHist[[which(index$winter_yr==years[i])]] 
  #maxYear <- stackApply(tmp, indices = 1:nlayers(tmp), fun = max) Nope
  mintmp <- calc(tmp,  fun = min, na.rm = TRUE)
  dummyTMNC[[i]] <- mintmp
  tmp <- NULL
}
# Save just in case
saveRDS(dummyTMNC,file =  "E:/EDE_Daten/worldClimMonth/dummyBIO6.rds" )

# Write parallel function
writeParallel <- function(x){
  dummyName <- file.path("F:/worldClimMonth_proc/", paste("BIO6", "_",names(x) , ".tif", sep = "" ))
  writeRaster(x = x, filename = dummyName, format="GTiff")
}

for (i in 1:length(dummyTMNC)){
  names(dummyTMNC[[i]]) <- names(dummyTMNC)[i]
}
# create cluster object
cl <- makeCluster(4)
# test each number in sample_numbers for primality
results <- parSapply(cl, dummyTMNC, writeParallel)
# close
stopCluster(cl)
# Stack single files into one raster
TMNC <- stack(dummyTMNC[c(10:40)]) 
TMNC
plot(TMNC$X1970)
plot(TMNC$X2000)
# Calculate the extremes
exBIO6 <- calc(TMNC,maxExtremes)
# Save just in case
saveRDS(exBIO6,file =  "E:/EDE_Daten/worldClimMonth/exBIO6.rds" )
writeRaster(x = exBIO6, filename = "F:/worldClimMonth_proc/exBIO6.tif", format="GTiff")


################################################################################
# Calculate BIO5 = Max Temperature of Hottest Month
# load data
tmaxHist <- readRDS(file =  "E:/EDE_Daten/worldClimMonth/wc2.1_2.5m_tmax_1961_2018.rds" )
# Build index for calculation --> Dont break  winters
x <- names(tmaxHist)[1]
n_last <- 7
layerDates <- sapply(names(tmaxHist), function(x) paste(substr(x, nchar(x) - n_last + 1, nchar(x)), ".15", sep = "" ))
names(layerDates) <- NULL
index = data.frame(band = 1:nlayers(tmaxHist), date = as.Date(layerDates, format = "%Y.%m.%d"))
index$summer_yr <-  as.numeric(format(index$date,"%Y"))
index$winter_yr <- as.numeric(format(index$date,"%Y"))
head(index, 20)
index[which(format(index$date,"%m") %in% c("07","08","09","10","11","12")),
      "winter_yr"] <-  index[which(format(index$date,"%m") %in% c("07","08","09","10",
                                                                  "11","12")),"winter_yr"] + 1
head(index, 20)
# Create a data holder
years <- unique(index$summer_yr)
dummyBIO5 <- vector(mode = "list", length = length(years))
names(dummyBIO5) <- years
cat("Calculating for...")
for (i in 1:length(years)){
  cat(years[i]);cat("...")
  tmp <- tmaxHist[[which(index$summer_yr==years[i])]] 
  #maxYear <- stackApply(tmp, indices = 1:nlayers(tmp), fun = max) Nope
  maxtmp <- calc(tmp,  fun = max, na.rm = TRUE)
  dummyBIO5[[i]] <- maxtmp
  tmp <- NULL
}

# CRAZY CHECK
if(is.null(dummyBIO5[[length(dummyBIO5)]])) dummyBIO5[[length(dummyBIO5)]] <- NULL
# Save just in case
saveRDS(dummyBIO5,file =  "E:/EDE_Daten/worldClimMonth/dummyBIO5.rds" )



for (i in 1:length(dummyBIO5)){
  names(dummyBIO5[[i]]) <- names(dummyBIO5)[i]
}

# Write parallel function
writeParallel <- function(x){
  dummyName <- file.path("F:/worldClimMonth_proc/", paste("BIO5", "_",names(x) , ".tif", sep = "" ))
  writeRaster(x = x, filename = dummyName, format="GTiff", overwrite = TRUE)
}
# create cluster object
cl <- makeCluster(4)
# test each number in sample_numbers for primality
results <- parSapply(cl, dummyBIO5, writeParallel)
writeParallel(dummyBIO5[[1]])
# close
stopCluster(cl)
# Stack single files into one raster
BIO5 <- stack(dummyBIO5[c(10:40)]) 
BIO5
plot(BIO5$X1970)
plot(BIO5$X2000)
# Calculate the extremes
exBIO5 <- calc(BIO5, maxExtremes)
# Save just in case
saveRDS(exBIO5,file =  "E:/EDE_Daten/worldClimMonth/exBIO5.rds" )
writeRaster(x = exBIO5, filename = "F:/worldClimMonth_proc/exBIO5.tif", format="GTiff")

rm(tmaxHist)
rm(exBIO5)
rm(BIO5)
rm(dummyBIO5)
rm(results)

################################################################################
# Precipitation create rolling window for the period
# Load the data
precHist <- readRDS(file ="F:/worldClimMonth_proc/wc2.1_2.5m_prec_1961_2018.rds")


plot(precHist[[1]])
plot(precHist[[6]])



################################################################################
#  BIO17 = Precipitation of Driest Quarter 
#           AND
#  BIO16 = Precipitation of Wettest Quarter
# Build index for calculation --> Dont break  winters
x <- names(precHist)[1]
n_last <- 12
layerDates <- sapply(names(precHist), function(x) paste(substr(x, nchar(x) - n_last + 1, nchar(x)-5), ".15", sep = "" ))
names(layerDates) <- NULL
index = data.frame(band = 1:nlayers(precHist), date = as.Date(layerDates, format = "%Y.%m.%d"))
index$summer_yr <-  as.numeric(format(index$date,"%Y"))
index$winter_yr <- as.numeric(format(index$date,"%Y"))
head(index, 20)
index[which(format(index$date,"%m") %in% c("07","08","09","10","11","12")),
      "winter_yr"] <-  index[which(format(index$date,"%m") %in% c("07","08","09","10",
                                                                  "11","12")),"winter_yr"] + 1
head(index, 20)
# Create a data holder
years <- unique(index$summer_yr)
dummyBIO17 <- vector(mode = "list", length = length(years))
names(dummyBIO17) <- years
dummyBIO16 <- vector(mode = "list", length = length(years))
names(dummyBIO16) <- years

cat("Calculating for...")
for (i in 1:length(years)){
  cat(years[i]);cat("...")
  tmp <- precHist[[which(index$summer_yr==years[i])]] 
  #maxYear <- stackApply(tmp, indices = 1:nlayers(tmp), fun = max) Nope
  minPre <- calc(tmp,  fun = min, na.rm = TRUE)
  maxPre <- calc(tmp,  fun = max, na.rm = TRUE)
  dummyBIO17[[i]] <- minPre
  dummyBIO16[[i]] <- maxPre
  tmp <- NULL
}

# CRAZY CHECK
if(is.null(dummyBIO17[[length(dummyBIO17)]])) dummyBIO17[[length(dummyBIO17)]] <- NULL
# Save just in case
saveRDS(dummyBIO17,file =  "E:/EDE_Daten/worldClimMonth/dummyBIO17.rds" )
# CRAZY CHECK
if(is.null(dummyBIO16[[length(dummyBIO16)]])) dummyBIO17[[length(dummyBIO16)]] <- NULL
# Save just in case
saveRDS(dummyBIO16,file =  "E:/EDE_Daten/worldClimMonth/dummyBIO16.rds" )



for (i in 1:length(dummyBIO17)){
  names(dummyBIO17[[i]]) <- names(dummyBIO17)[i]
  names(dummyBIO16[[i]]) <- names(dummyBIO16)[i]
}


# Write parallel function
writeParallel <- function(x){
  dummyName <- file.path("F:/worldClimMonth_proc/", paste("BIO17", "_",names(x) , ".tif", sep = "" ))
  writeRaster(x = x, filename = dummyName, format="GTiff", overwrite = TRUE)
}
writeParallel(dummyBIO17[['1961']])
# create cluster object
cl <- makeCluster(4)
# test each number in sample_numbers for primality
results <- parSapply(cl, dummyBIO17, writeParallel)
# close
stopCluster(cl)
# Stack single files into one raster
BIO17 <- stack(dummyBIO17[c(10:40)]) 
BIO17
plot(BIO17$X1970)
plot(BIO17$X2000)
# Calculate the extremes
exBIO17 <- calc(BIO17,minExtremes)
# Save just in case
saveRDS(exBIO17,file =  "E:/EDE_Daten/worldClimMonth/exBIO17.rds" )
writeRaster(x = exBIO17, filename = "F:/worldClimMonth_proc/exBIO17.tif", format="GTiff")
################################################################################
# Write parallel function
writeParallel <- function(x){
  dummyName <- file.path("F:/worldClimMonth_proc/", paste("BIO16", "_",names(x) , ".tif", sep = "" ))
  writeRaster(x = x, filename = dummyName, format="GTiff")
}
writeParallel(dummyBIO16[['1961']])
# create cluster object
cl <- makeCluster(4)
# test each number in sample_numbers for primality
results <- parSapply(cl, dummyBIO16, writeParallel)
# close
stopCluster(cl)
# Stack single files into one raster
BIO16 <- stack(dummyBIO16[c(10:40)]) 
BIO16
plot(BIO16$X1970)
plot(BIO16$X2000)
# Calculate the extremes
exBIO16_min <- calc(BIO16,minExtremes)
# Save just in case
saveRDS(exBIO16_min,file =  "E:/EDE_Daten/worldClimMonth/exBIO16_min.rds" )
writeRaster(x = exBIO16_min, filename = "F:/worldClimMonth_proc/exBIO16_min.tif", format="GTiff")
# Calculate the extremes
exBIO16_max <- calc(BIO16,maxExtremes)
# Save just in case
saveRDS(exBIO16_max,file =  "E:/EDE_Daten/worldClimMonth/exBIO16_max.rds" )
writeRaster(x = exBIO16_max, filename = "F:/worldClimMonth_proc/exBIO16_max.tif", format="GTiff")

r1 <- raster("F:/worldClimMonth_proc/exBIO16_max.tif")
r2 <- raster("F:/worldClimMonth_proc/exBIO16_min.tif")
r <- r1[[1]]-r2[[1]]
plot(r)
################################################################################
# DRY DAYS from MIKLIP
################################################################################
# ccd _veg_per (vegation period)
# - longest dry period* in the vegetation period
# - number of dry periods* (periods >=5 days) in year 
# *rr < 1 mm

nc_data <- nc_open("E:/EDE_Daten/dry_days_VegPer/CDD_VegPer_1961_2020.nc")
cdd1 <- brick("E:/EDE_Daten/dry_days_VegPer/CDD_VegPer_1961_2020.nc",
             varname = "consecutive_dry_days_index_per_time_period")

cdd2 <- brick("E:/EDE_Daten/dry_days_VegPer/CDD_VegPer_1961_2020.nc",
              varname = "number_of_cdd_periods_with_more_than_5days_per_time_period")


plot(cdd1$X1961.09.30)
plot(cdd2$X1961.09.30)

dates <- as.Date(names(cdd1), format = "X%Y.%m.%d")
years <- as.numeric(format(dates,"%Y"))
dummyCDD <- stack(cdd1[[c(10:40)]])
exlongCDDVegPer <- calc(dummyCDD,maxExtremes)
saveRDS(exlongCDDVegPer,file =  "E:/EDE_Daten/worldClimMonth/exlongCDDVegPer.rds" )
writeRaster(x = exlongCDDVegPer, filename = "F:/worldClimMonth_proc/exlongCDDVegPer.tif", format="GTiff")

dates <- as.Date(names(cdd2), format = "X%Y.%m.%d")
years <- as.numeric(format(dates,"%Y"))
dummyCDD <- stack(cdd2[[c(10:40)]])
exnCDDyear <- calc(dummyCDD,maxExtremes)
saveRDS(exnCDDyear,file =  "E:/EDE_Daten/worldClimMonth/exnCDDyear.rds" )
writeRaster(x = exnCDDyear, filename = "F:/worldClimMonth_proc/exnCDDyear.tif", format="GTiff")

plot(exlongCDDVegPer$layer.8)
plot(exnCDDyear$layer.)
################################################################################
# dry_days (full year)
# -0 rr > 1 mm
# - 1  rr <= 1 m
# --> total number of dry days per year
# Read in the data
nc_data <- nc_open("E:/EDE_Daten/dry_days_VegPer/dry_days_EUR-10_1961-2020.nc")
dryDays <- raster("E:/EDE_Daten/dry_days_VegPer/dry_days_EUR-10_1961-2020.nc")
plot(dryDays)
# There is nothing to do with this!!
################################################################################
# SOIL WATER CONTENT from ERA5 (montly)
################################################################################
# all variablen in one datei
nc_data <- nc_open("E:/EDE_Daten/ERA5/ERA5_monthly_0.1_1978-042021.nc")

################################################################################
skinRes <- brick("E:/EDE_Daten/ERA5/ERA5_monthly_0.1_1978-042021.nc",
              varname = "src")
layerDates <- as.Date(names(skinRes), format = "X%Y.%m.%d")
years <- unique(as.numeric(format(dates,"%Y")))
index = data.frame(band = 1:nlayers(skinRes), date = layerDates)
index$summer_yr <-  as.numeric(format(index$date,"%Y"))
index$winter_yr <- as.numeric(format(index$date,"%Y"))
head(index, 20)
index[which(format(index$date,"%m") %in% c("07","08","09","10","11","12")),
      "winter_yr"] <-  index[which(format(index$date,"%m") %in% c("07","08","09","10",
                                                                  "11","12")),"winter_yr"] + 1


for (i in 1:nlayers(skinRes)){
  central <- i
  if (i == nlayers(skinRes)){
    lower <- i - 1
    upper <-  i - 2
  }else if (i == 1){
    lower <- i + 1
    upper <- i + 2
  }else{
    lower <- i - 1
    upper <- i + 1
  }
  rsum <- skinRes[[central]] + skinRes[[lower]]  + skinRes[[upper]] 
  skinRes[[i]] <- rsum
  rm(rsum)
}
names(skinRes) <- as.character(format(layerDates, "X%Y.%m.%d"))


dummySKINdriest <- vector(mode = "list", length = length(years))
names(dummySKINdriest) <- paste("X", years, sep = "")
dummySKINwettest <- vector(mode = "list", length = length(years))
names(dummySKINwettest) <- paste("X", years, sep="")




cat("Calculating for...")
for (i in 1:length(years)){
  cat(years[i]);cat("...")
  tmp <- skinRes[[which(index$summer_yr==years[i])]] 
  #maxYear <- stackApply(tmp, indices = 1:nlayers(tmp), fun = max) Nope
  minPre <- calc(tmp,  fun = min, na.rm = TRUE)
  maxPre <- calc(tmp,  fun = max, na.rm = TRUE)
  dummySKINdriest[[i]] <- minPre
  dummySKINwettest[[i]] <- maxPre
  tmp <- NULL
}

# Stack single files into one raster
skinDriest <- stack(dummySKINdriest[c(1:20)]) 
plot(skinDriest$X1981)
plot(skinDriest$X2000)
# Calculate the extremes
exSKINdriest <- calc(skinDriest,minExtremes)
# Save just in case
saveRDS(exSKINdriest,file =  "E:/EDE_Daten/worldClimMonth/exSKINdriest.rds" )
writeRaster(x = exSKINdriest, filename = "F:/worldClimMonth_proc/exSKINdriest.tif", format="GTiff")

# Stack single files into one raster
skinWettest <- stack(dummySKINwettest[c(1:20)]) 
plot(skinWettest$X1981)
plot(skinWettest$X2000)
# Calculate the extremes
exSKINwettest <- calc(skinWettest,maxExtremes)
# Save just in case
saveRDS(exSKINwettest,file =  "E:/EDE_Daten/worldClimMonth/exSKINwettest.rds" )
writeRaster(x = exSKINwettest, filename = "F:/worldClimMonth_proc/exSKINwettest.tif", format="GTiff")


################################################################################
swvl1 <- brick("E:/EDE_Daten/ERA5/ERA5_monthly_0.1_1978-042021.nc",
                 varname = "swvl1")
dummyswvl1MIN <- vector(mode = "list", length = nlayers(swvl1))
names(dummyswvl1MIN) <- paste("X", years, sep = "")
dummyswvl1MAX <- vector(mode = "list", length = nlayers(swvl1))
names(dummyswvl1MAX) <- paste("X", years, sep = "")
dummyswvl1MEAN <- vector(mode = "list", length = nlayers(swvl1))
names(dummyswvl1MEAN) <- paste("X", years, sep = "")


cat("Calculating for...")
for (i in 1:length(years)){
  cat(years[i]);cat("...")
  tmp <- swvl1[[which(index$summer_yr==years[i])]] 
  #maxYear <- stackApply(tmp, indices = 1:nlayers(tmp), fun = max) Nope
  meanPre <- calc(tmp,  fun = mean, na.rm = TRUE)
  minPre <- calc(tmp,  fun = min, na.rm = TRUE)
  maxPre <- calc(tmp,  fun = max, na.rm = TRUE)
  dummyswvl1MIN[[i]] <- minPre
  dummyswvl1MAX[[i]] <- maxPre
  dummyswvl1MEAN[[i]] <- meanPre
  tmp <- NULL
}
# MIN
# Stack single files into one raster
swvl1MIN <- stack(dummyswvl1MIN[c(1:20)]) 
plot(swvl1MIN$X1981)
plot(swvl1MIN$X2000)
exswvl1MIN <- calc(swvl1MIN, maxExtremes)
exswvl1MIN <- exswvl1MIN[[c(1:2)]]
# Save just in case
saveRDS(exswvl1MIN,file =  "E:/EDE_Daten/worldClimMonth/exswvl1MIN.rds" )
writeRaster(x = exswvl1MIN, filename = "F:/worldClimMonth_proc/exswvl1MIN.tif", format="GTiff")
# MAX
# Stack single files into one raster
swvl1MAX <- stack(dummyswvl1MAX[c(1:20)]) 
plot(swvl1MAX$X1981)
plot(swvl1MAX$X2000)
exswvl1MAX <- calc(swvl1MAX, maxExtremes)
exswvl1MAX <- exswvl1MAX[[c(1:2)]]
# Save just in case
saveRDS(exswvl1MAX,file =  "E:/EDE_Daten/worldClimMonth/exswvl1MAX.rds" )
writeRaster(x = exswvl1MAX, filename = "F:/worldClimMonth_proc/exswvl1MAX.tif", format="GTiff", overwrite = TRUE)
# MEAN
# Stack single files into one raster
swvl1MEAN <- stack(dummyswvl1MEAN[c(1:20)]) 
plot(swvl1MEAN$X1981)
plot(swvl1MEAN$X2000)
exswvl1MEAN <- calc(swvl1MEAN, maxExtremes)
exswvl1MEAN <- exswvl1MEAN[[c(1:2)]]
# Save just in case
saveRDS(exswvl1MEAN,file =  "E:/EDE_Daten/worldClimMonth/swvl1MEAN.rds" )
writeRaster(x = exswvl1MEAN, filename = "F:/worldClimMonth_proc/swvl1MEAN.tif", format="GTiff")

################################################################################
swvl2 <- brick("E:/EDE_Daten/ERA5/ERA5_monthly_0.1_1978-042021.nc",
               varname = "swvl2")
dummyswvl2MIN <- vector(mode = "list", length = nlayers(swvl2))
names(dummyswvl2MIN) <- paste("X", years, sep = "")
dummyswvl2MAX <- vector(mode = "list", length = nlayers(swvl2))
names(dummyswvl2MAX) <- paste("X", years, sep = "")
dummyswvl2MEAN <- vector(mode = "list", length = nlayers(swvl2))
names(dummyswvl2MEAN) <- paste("X", years, sep = "")


cat("Calculating for...")
for (i in 1:length(years)){
  cat(years[i]);cat("...")
  tmp <- swvl2[[which(index$summer_yr==years[i])]] 
  #maxYear <- stackApply(tmp, indices = 1:nlayers(tmp), fun = max) Nope
  meanPre <- calc(tmp,  fun = mean, na.rm = TRUE)
  minPre <- calc(tmp,  fun = min, na.rm = TRUE)
  maxPre <- calc(tmp,  fun = max, na.rm = TRUE)
  dummyswvl2MIN[[i]] <- minPre
  dummyswvl2MAX[[i]] <- maxPre
  dummyswvl2MEAN[[i]] <- meanPre
  tmp <- NULL
}
# MIN
# Stack single files into one raster
swvl2MIN <- stack(dummyswvl2MIN[c(1:20)]) 
plot(swvl2MIN$X1981)
plot(swvl2MIN$X2000)
exswvl2MIN <- calc(swvl2MIN, maxExtremes)
exswvl2MIN <- exswvl2MIN[[c(1:2)]]
# Save just in case
saveRDS(exswvl2MIN,file =  "E:/EDE_Daten/worldClimMonth/exswvl2MIN.rds" )
writeRaster(x = exswvl2MIN, filename = "F:/worldClimMonth_proc/exswvl2MIN.tif", format="GTiff")
# MAX
# Stack single files into one raster
swvl2MAX <- stack(dummyswvl2MAX[c(1:20)]) 
plot(swvl2MAX$X1981)
plot(swvl2MAX$X2000)
exswvl2MAX <- calc(swvl2MAX, maxExtremes)
exswvl2MAX <- exswvl2MAX[[c(1:2)]]
# Save just in case
saveRDS(exswvl2MAX,file =  "E:/EDE_Daten/worldClimMonth/exswvl2MAX.rds" )
writeRaster(x = exswvl2MAX, filename = "F:/worldClimMonth_proc/exswvl2MAX.tif", format="GTiff")
# MEAN
# Stack single files into one raster
swvl2MEAN <- stack(dummyswvl2MEAN[c(1:20)]) 
plot(swvl2MEAN$X1981)
plot(swvl2MEAN$X2000)
exswvl2MEAN <- calc(swvl2MEAN, maxExtremes)
exswvl2MEAN <- exswvl2MEAN[[c(1:2)]]
# Save just in case
saveRDS(exswvl2MEAN,file =  "E:/EDE_Daten/worldClimMonth/swvl2MEAN.rds" )
writeRaster(x = exswvl2MEAN, filename = "F:/worldClimMonth_proc/swvl2MEAN.tif", format="GTiff")

################################################################################
swvl3 <- brick("E:/EDE_Daten/ERA5/ERA5_monthly_0.1_1978-042021.nc",
               varname = "swvl3")
dummyswvl3MIN <- vector(mode = "list", length = nlayers(swvl3))
names(dummyswvl3MIN) <- paste("X", years, sep = "")
dummyswvl3MAX <- vector(mode = "list", length = nlayers(swvl3))
names(dummyswvl3MAX) <- paste("X", years, sep = "")
dummyswvl3MEAN <- vector(mode = "list", length = nlayers(swvl3))
names(dummyswvl3MEAN) <- paste("X", years, sep = "")


cat("Calculating for...")
for (i in 1:length(years)){
  cat(years[i]);cat("...")
  tmp <- swvl3[[which(index$summer_yr==years[i])]] 
  #maxYear <- stackApply(tmp, indices = 1:nlayers(tmp), fun = max) Nope
  meanPre <- calc(tmp,  fun = mean, na.rm = TRUE)
  minPre <- calc(tmp,  fun = min, na.rm = TRUE)
  maxPre <- calc(tmp,  fun = max, na.rm = TRUE)
  dummyswvl3MIN[[i]] <- minPre
  dummyswvl3MAX[[i]] <- maxPre
  dummyswvl3MEAN[[i]] <- meanPre
  tmp <- NULL
}
# MIN
# Stack single files into one raster
swvl3MIN <- stack(dummyswvl3MIN[c(1:20)]) 
plot(swvl3MIN$X1981)
plot(swvl3MIN$X2000)
exswvl3MIN <- calc(swvl3MIN, maxExtremes)
exswvl3MIN <- exswvl3MIN[[c(1:2)]]
# Save just in case
saveRDS(exswvl3MIN,file =  "E:/EDE_Daten/worldClimMonth/exswvl3MIN.rds" )
writeRaster(x = exswvl3MIN, filename = "F:/worldClimMonth_proc/exswvl3MIN.tif", format="GTiff")
# MAX
# Stack single files into one raster
swvl3MAX <- stack(dummyswvl3MAX[c(1:20)]) 
plot(swvl3MAX$X1981)
plot(swvl3MAX$X2000)
exswvl3MAX <- calc(swvl3MAX, maxExtremes)
exswvl3MAX <- exswvl3MAX[[c(1:2)]]
# Save just in case
saveRDS(exswvl3MAX,file =  "E:/EDE_Daten/worldClimMonth/exswvl3MAX.rds" )
writeRaster(x = exswvl3MAX, filename = "F:/worldClimMonth_proc/exswvl3MAX.tif", format="GTiff")
# MEAN
# Stack single files into one raster
swvl3MEAN <- stack(dummyswvl3MEAN[c(1:20)]) 
plot(swvl3MEAN$X1981)
plot(swvl3MEAN$X2000)
exswvl3MEAN <- calc(swvl3MEAN, maxExtremes)
exswvl3MEAN <- exswvl3MEAN[[c(1:2)]]
# Save just in case
saveRDS(exswvl3MEAN,file =  "E:/EDE_Daten/worldClimMonth/swvl3MEAN.rds" )
writeRaster(x = exswvl3MEAN, filename = "F:/worldClimMonth_proc/swvl3MEAN.tif", format="GTiff")

################################################################################
swvl4 <- brick("E:/EDE_Daten/ERA5/ERA5_monthly_0.1_1978-042021.nc",
               varname = "swvl4")
dummyswvl4MIN <- vector(mode = "list", length = nlayers(swvl4))
names(dummyswvl4MIN) <- paste("X", years, sep = "")
dummyswvl4MAX <- vector(mode = "list", length = nlayers(swvl4))
names(dummyswvl4MAX) <- paste("X", years, sep = "")
dummyswvl4MEAN <- vector(mode = "list", length = nlayers(swvl4))
names(dummyswvl4MEAN) <- paste("X", years, sep = "")


cat("Calculating for...")
for (i in 1:length(years)){
  cat(years[i]);cat("...")
  tmp <- swvl4[[which(index$summer_yr==years[i])]] 
  #maxYear <- stackApply(tmp, indices = 1:nlayers(tmp), fun = max) Nope
  meanPre <- calc(tmp,  fun = mean, na.rm = TRUE)
  minPre <- calc(tmp,  fun = min, na.rm = TRUE)
  maxPre <- calc(tmp,  fun = max, na.rm = TRUE)
  dummyswvl4MIN[[i]] <- minPre
  dummyswvl4MAX[[i]] <- maxPre
  dummyswvl4MEAN[[i]] <- meanPre
  tmp <- NULL
}
# MIN
# Stack single files into one raster
swvl4MIN <- stack(dummyswvl4MIN[c(1:20)]) 
plot(swvl4MIN$X1981)
plot(swvl4MIN$X2000)
exswvl4MIN <- calc(swvl4MIN, maxExtremes)
exswvl4MIN <- exswvl4MIN[[c(1:2)]]
# Save just in case
saveRDS(exswvl4MIN,file =  "E:/EDE_Daten/worldClimMonth/exswvl4MIN.rds" )
writeRaster(x = exswvl4MIN, filename = "F:/worldClimMonth_proc/exswvl4MIN.tif", format="GTiff")
# MAX
# Stack single files into one raster
swvl4MAX <- stack(dummyswvl4MAX[c(1:20)]) 
plot(swvl4MAX$X1981)
plot(swvl4MAX$X2000)
exswvl4MAX <- calc(swvl4MAX, maxExtremes)
exswvl4MAX <- exswvl4MAX[[c(1:2)]]
# Save just in case
saveRDS(exswvl4MAX,file =  "E:/EDE_Daten/worldClimMonth/exswvl4MAX.rds" )
writeRaster(x = exswvl4MAX, filename = "F:/worldClimMonth_proc/exswvl4MAX.tif", format="GTiff")
# MEAN
# Stack single files into one raster
swvl4MEAN <- stack(dummyswvl4MEAN[c(1:20)]) 
plot(swvl4MEAN$X1981)
plot(swvl4MEAN$X2000)
exswvl4MEAN <- calc(swvl4MEAN, maxExtremes)
exswvl4MEAN <- exswvl4MEAN[[c(1:2)]]
# Save just in case
saveRDS(exswvl4MEAN,file =  "E:/EDE_Daten/worldClimMonth/swvl4MEAN.rds" )
writeRaster(x = exswvl4MEAN, filename = "F:/worldClimMonth_proc/swvl4MEAN.tif", format="GTiff")

################################################################################
# TODO
################################################################################

# define extent and crop
# e <- drawExtent()
e <- extent(-24.17268, 67.936, 22.62975, 82.00946)
#tmaxHist <- crop(tmaxHist, e)
#tminHist <- crop(tminHist, e)
#precHist <- crop(precHist, e)
