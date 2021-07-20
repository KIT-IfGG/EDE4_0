###############################################################################
# Read netCDF data from IMk and turn into rasters
###############################################################################
require(ncdf4)
# Folder with all files
netcdfFilenames <-list.files("E:/EDE_Daten/klima_sims/klima_sims", full.names = T,
                             pattern = "nc")
netcdfFilenames <- netcdfFilenames[grepl(pattern = "2M",netcdfFilenames)]
climaNames <-list.files("E:/EDE_Daten/klima_sims/klima_sims", full.names = F, pattern = "nc")
climaNames <- climaNames[grepl(pattern = "2M",climaNames)]
climaNames <- gsub("_monmean.nc", "", climaNames)
climaNames <- gsub("_monsum.nc", "", climaNames)
climaNames <- gsub("_AV", "", climaNames)
climaNames


varNames <- c("T_2M_AV", "TMAX_2M", "TMIN_2M", "TOT_PREC",
              "T_2M_AV", "TMAX_2M", "TMIN_2M", "TOT_PREC",
              "T_2M_AV", "TMAX_2M", "TMIN_2M", "TOT_PREC",
              "T_2M_AV", "TMAX_2M", "TMIN_2M", "TOT_PREC")
              

# Read metadata and print it out
nc_data <- nc_open(netcdfFilenames[8])
r <- raster(netcdfFilenames[8])
# rotated pole stuff
#long_name: coordinates of the rotated North Pole
#grid_mapping_name: rotated_latitude_longitude
#grid_north_pole_latitude: 40
#grid_north_pole_longitude: -170

plot(r)
nc_data$var$lat



library(rgdal)
epsg <- make_EPSG()
c(epsg[epsg$code=="4236",]$prj4, "+proj=ob_tran +o_proj=longlat +o_lon_p=-162 +o_lat_p=39.25 +lon_0=175 +to_meter=0.01745329")
crs(r)<- CRS("+proj=ob_tran +o_proj=longlat +o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +to_meter=0.01745329")
plot(r)

x <- projectRaster(ro, crs =CRS("+proj=longlat +datum=WGS84 +no_defs +units=m"))

plot(ro)
ncd
x <- st_transform_proj(d, c(st_crs(4326)$proj4string, "+proj=ob_tran +o_proj=longlat +o_lon_p=-162 +o_lat_p=39.25 +lon_0=175 +to_meter=0.01745329"))
st_crs(x) = NA # ?! (!)
st_crs(x) = 4326

crs(r)




plot(DE, add=TRUE)

geom(empty.raster)
newCoord <- rasterToPoints(empty.raster)
head(newCoord)
table(newCoord[,1])
head(order(lon))

lat <- ncvar_get(nc_data, "lat")
lon <- ncvar_get(nc_data, "lon")
varName <- "TOT_PREC"
varValues <- ncvar_get(nc_data, varName)

head(lat)


dim(lat)
dim(lon)
dim(varValues)
range(lon)
range(lon[1,])
range(lon[,1])

range(lat)
min(lon[1,])
max(lon[1,])
min(lon[,1])
max(lon[,1])
head(lon)


plot(empty.raster)

library(rgdal)
epsg <- make_EPSG()


new.raster <- rasterize(cbind(lon, lat), empty.raster, z)
plot(new.raster)

DE <- getData('GADM', country='Germany', level=1)
plot(DE, add = TRUE)

plot(r)



sink('D:/Downloads/kliwa_monmeans.tar/kliwa_monmeans/KlimaStr.txt')
print(nc_data)
sink()


str(nc_data$var$T_2M_AV$dim,1)
nc_data <- nc_open(netcdfFilenames[4])

###############################################################################
# Function for all data
netToBrick.monthly <- function(nc_data, varName, mode = "kelvin"){
  fillvalue <- ncatt_get(nc_data, varName, "_FillValue")
  lat <- ncvar_get(nc_data, "lat")
  lon <- ncvar_get(nc_data, "lon")
  rowsRaster <-dim(lon)[2] # It might be inversed
  colsRaster <- dim(lon)[1] # it might be inversed
  lat <- as.vector(lat)
  lon <- as.vector(lon)
  varValues <- ncvar_get(nc_data, varName)
  cat("######################");cat("\n")
  cat("# ");cat(varName); cat("\n")
  cat("######################");cat("\n")
  # Loop over and read in the data
  years <- dim(varValues)[3]/12
  months <- c(1:12)
  monthsNames <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", 
                   "SEP", "OCT", "NOV", "DEC")
  dataHolder <- vector(mode = "list", length = years)
  # raster read function
  readRotate <- function(index, mode = "kelvin"){
      variable <-  as.vector(varValues[, ,index])
      if(mode == "kelvin"){
       # cat("Temperature conversion");cat("\n")
        variable <- variable - 273.15
      }
      empty.raster  <- raster(nrows=rowsRaster, ncols=colsRaster, 
                              xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
                              crs = CRS("+proj=longlat +datum=WGS84 +no_defs"), vals = 999)
 
      
      # a Lon column
      newLon <- seq(extent(empty.raster)[1], extent(empty.raster)[2], length.out = colsRaster)
      newLon <- matrix(rep(newLon, rowsRaster), ncol = rowsRaster)
      newLon <- as.vector(newLon)
      # a lat row
      newLat <- seq(extent(empty.raster)[3], extent(empty.raster)[4], length.out = rowsRaster)
      newLat <-  t(matrix(rep(newLat, colsRaster), ncol = colsRaster))
      newLat <- as.vector(newLat)
      
      #filled.raster <- rasterize(cbind(lon, lat), empty.raster, variable, fun = sum) # Empty points around
      filled.raster <- rasterize(cbind(newLon, newLat), empty.raster, variable)
      #r <- flip(r, direction='y')
     # if (mode == "kgm2"){
        #kg m-2 in 2km2 --> m3/1000 kg * 1000mm/1m *-->   mm in  2km2 but pixel size 
        # is different?!?! 
    #    r <- r*prod(res(r)
     # }
      return(filled.raster)
  }
  cat("Year: ")
  for (i in 1:years){
    indexes <- months * i
    cat("...");cat(i)
    yearList <- lapply(indexes,readRotate, mode = mode  )
    names(yearList) <- monthsNames
    yearList <- stack(yearList)
    #plot(yearList)
    #cat("\n\n");
    dataHolder[[i]] <- stack(yearList)
  }
  cat("\n")
  return(dataHolder)
}

###############################################################################
# Read and Format data
climateProj <- vector(mode = "list", length = length(varNames))
names(climateProj) <- climaNames
for (i in 1:length(varNames)){
  mode  <- "kelvin"  ###
  if(grepl(pattern = "PREC", varNames[i])) mode <- "kgm2"
  nc_data <- nc_open(netcdfFilenames[i])
  climateProj[[i]] <- netToBrick.monthly(nc_data, varNames[i], mode=mode)
}

saveRDS(climateProj, file = "climateProj.RDS")

###############################################################################
plot(climateProj$`CEH_T_2M_2021-2050`[[1]]$JAN)
plot(area(climateProj$`CEH_T_2M_2021-2050`[[1]]$JAN))
plot()
# Explore the data
# Put all color palettes together
library(RColorBrewer)
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# Set the colors
n <- 30
pie(rep(1,n), col=sample(col_vector, n))
myColors <- sample(col_vector, n)
#Plot density

# Compare density of tmax by year
calcYearExt <- function(x, returnPeriod = c(10,15,20), mode = "max", ...){
  u = mean(x, na.rm = TRUE)
  sd = sd(x, na.rm = TRUE)
  if (mode == "min"){
    p = 1/returnPeriod
  }else if (mode == "max"){
    p = 1 - (1/returnPeriod)
  }
  p = 1/returnPeriod
  q = quantile(x,probs = p, na.rm = TRUE )
  ex = as.numeric(q-u)
  out <- c(u, sd,q)
  names(out)<- c("mean", "sd", returnPeriod)
  return(c(u, sd,ex))
}
# Pack all data in a list
plotData <- vector(mode="list", length = 30)
for (i in 1:30){
  r1 <- climateProj$`CEH_TMAX_2M_2021-2050`[[i]][[C("JUN", "JUL")]]
  dummy <- calc(r1,fun =  calcYearExt)
  plot(dummy)
  values <- as.data.frame(dummy[[1]])
  values <- values[!is.na(values)]
  plot(density(values))
  plotData[[i]] <- dummy[!is.na(dummy)]
}


#☺ Compare one month/months within model across years (30 years)
mo <- c("JUN", "JUL", "AGO")
title <- paste("Density of TMEAN in", paste(mo, collapse="-"),  "CEH", sep = " ")
# Pack all data in a list
plotData <- vector(mode="list", length = 30)
for (i in 1:30){
  dummy <- as.data.frame(climateProj$`CEH_TMAX_2M_2021-2050`[[i]][[mo]], row.names = NULL)[,1]
  plotData[[i]] <- dummy[!is.na(dummy)]
}
# Define range
plot(density(unlist(plotData)), col = "red", lty = 4, lwd = 2)


densRange <- c(0, 0.5)
varRange <- range(unlist(sapply(plotData, range, na.rm=T)))
# Plot density
for (i in 1:30){
  lineColor <-  myColors[i]
  if (i==1){
    plot(density(plotData[[i]]), col= "black", xlim=varRange, ylim=densRange,
         main = title)
  }else{
    lines(density(plotData[[i]]), col = myColors[i])
  }
}
# Add density all years
lines(density(unlist(plotData)), col = "red", lty = 4, lwd = 2)


# Compare one month across models (10 years)
mo <- "JUL"
year <- 1
title <- "Density of TMAX in Juli - Models"
# Pack all data in a list
modelVar <- c("CEH_TMAX_2M_2021-2050", "EC_TMAX_2M_2021-2050", "HAD_TMAX_2M_2021-2050" )
plotData <- vector(mode="list", length = length(modelVar))
for (i in 1:length(modelVar)){
  dummy <- as.data.frame(climateProj[[modelVar[i]]][[year]][[mo]], row.names = NULL)[,1]
  plotData[[i]] <- dummy[!is.na(dummy)]
}
# Define range
densRange <- c(0, 0.5)
varRange <- range(unlist(sapply(plotData, range, na.rm=T)))
# Plot density
for (i in 1:length(modelVar)){
  lineColor <-  myColors[i]
  if (i==1){
    plot(density(plotData[[i]]), col= "#000000", xlim=varRange, ylim=densRange,
         main = title)
    legend("topleft", legend = modelVar,lty = 1, col = c("#000000", myColors[2:i]))
  }else{
    lines(density(plotData[[i]]), col = myColors[i])
  }
}

###############################################################################
# Geographic plots
plot(climateProj$`CEH_T_2M_2021-2050`[[1]])
plot(climateProj$`CEH_TOT_PREC_2021-2050`[[1]])

###############################################################################
# Calculate the biovars values
models <- c("CEH", "EC", "HAD")
period <- c("2021-2050")
varSuffix <- c("TOT_PREC", "TMIN_2M", "TMAX_2M" )
# Create the biovar for each model
biovarModel <- vector(mode = "list", length = length(models))
names(biovarModel) <- models
# loop and collect variables, and calculate biovars
require(dismo)
for (i in 1:length(biovarModel)){
  prec <- paste(models[i], varSuffix[1], period, sep="_")
  tmin <- paste(models[i], varSuffix[2], period, sep="_")
  tmax <- paste(models[i], varSuffix[3], period, sep="_")
  #loop over years
  monthYear <- vector(mode="list", length = 30)
  for (j in 1:12){
    plot(climateProj[[prec]][[j]]*100)
    str(climateProj, 1)
    
  }
}
area <- prod(res(climateProj[[prec]][[j]]))

plot(climateProj[[prec]][[j]])
plot(climateProj[[prec]][[j]]*area)

stackApply(climateProj[[prec]][[j]], fun = , filename='', na.rm=TRUE, ...) 

biovars(prec, tmin, tmax, ...) 

names(climateProj)






fillvalue <- ncatt_get(nc_data, "T_2M_AV", "_FillValue")
summary(T_2M_AV)
T_2M_AV[T_2M_AV == fillvalue$value] <- NA
head(lat)
head(lat[,2])
head(T_2M_AV)
-
# i is month (total 12*30)
singles <- vector(mode = "list", length = dim(T_2M_AV)[3])
head(lat)

names(singles) <- as.vector(outer(seq(2021, 2021+29, by = 1), c(1:12), paste, sep="_"))
for (i in 1:dim(T_2M_AV)[3]){
  # Concert to C degrees * 10
  temp <- T_2M_AV[, , i]-273.15
  r <- raster(t(temp), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  singles[[i]] <- flip(r, direction='y')
  r <- NULL
  
}






plot(maps[["2021_1"]])
plot(germany, add = TRUE)
plot(maps[["2021_6"]])
plot(germany, add = TRUE)

# Calulate the bioclim variables
tmn <- vector(mode ="list", length = 30)

months <- c(1:12)
for (i in 1:length(biovarsYear)){
  stack()
}


par(mfrow= c(3,3))
for (i in 1:9){
  plot(maps[[i]])
  mtext(paste("T_2M_AV (K): ",2020+i, sep=""  ), 3)
  plot(germany, add = TRUE)
}

r <- raster(t(T_2M_AV[, , i]), xmn=min(lon[,i]), xmx=max(lon[,i]), ymn=min(lat[i,]), ymx=max(lat[i,]),
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
plot(r)
plot(germany, add = TRUE)

head(lat[1,])
head(lon[,1])
summary(lat[1,])
summary(lon[,1])

summary()

head(T_2M_AV)


hola <- sapply(dummy, cbind)
head(hola)
hola

tmp_raster <- brick(netcdfFilenames[1])
tmp_raster

projection(tmp_raster)
extent(tmp_raster)
crs(tmp_raster)
par(mfrow=c(1,1))
germany <- getData('GADM', country='Germany', level=1)
mf
plot(germany)
plot(tmp_raster$X2021.07.02.12.00.00, main= "2021.07.02.12.00.00")
plot(germany, add = TRUE)
