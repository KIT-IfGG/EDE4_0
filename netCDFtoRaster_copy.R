###############################################################################
# Read netCDF data from IMk and turn into rasters
###############################################################################
require(ncdf4)
require(raster)
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
nc_data <- nc_open(netcdfFilenames[1])
r <- brick(netcdfFilenames[1])
# rotated pole stuff--> rotated spherical coordinate system
#long_name: coordinates of the rotated North Pole
# 5.49926,14.4018, 46.53,52.5693
#grid_mapping_name: rotated_latitude_longitude
#grid_north_pole_latitude: 40 # geographical coordinates of rotated north pole
#grid_north_pole_longitude: -170
# geographical coordinates of rotated north pole (or: angles of rotation for the domain)
# pollat and pollon give the coordinates of the rotated north pole in real geographical coordinates

# +proj=ob_tran +ellps=sphere +lat_0=40 +lon_0=-170
# ohter CORDEX +o_lon_p=-162 +o_lat_p=39.25 

# Fabian wiht snap 4.900963, 14.40644, 46.53172, 52.67489

# +o_lat_p= Latitude of the North pole of the unrotated source CRS, expressed in the rotated geographic CRS.
# +o_lon_p= Longitude of the North pole of the unrotated source CRS, expressed in the rotated geographic CRS.
# +lon_0 = Longitude of projection center. In this case norht pole
#crs(r) <- CRS("+proj=ob_tran +o_proj=latlon +ellps=sphere +lat_0=40 +lon_0=-170")
#crs(r) <- CRS("+proj=stere +lat_0=40 +lon=-170 +a=90")


r
plot(r, 1)
# +lon_0 <- 180+ (-170)
# +o_lon_p=0
# +o_lat_p=  40
crs(r) <- CRS("+proj=ob_tran  +o_proj=longlat  +lon_0=10 +o_lat_p=40 +o_lon_p=0")
rRepro <- projectRaster(r, crs = "+proj=longlat +datum=WGS84 +no_defs", method = "bilinear")
plot(rRepro, 1)
maps::map(add = TRUE)
writeRaster(rRepro, filename="E:/EDE_Daten/klima_sims/Rprojected_CEH_T_2M_2021-2050_monmean.tig",
            format="GTiff", overwrite=TRUE)

rSNAP <- brick("E:/EDE_Daten/klima_sims/reprojected/projected_CEH_T_2M_2021-2050_monmean.tif")
mask <- extent(4.8, 13.98, 47, 53)

snapPic <- crop(rSNAP[[3]], mask)
reproPic <- crop(rRepro[[3]], mask)
test <- overlay(snapPic, reproPic, fun=function(r1, r2){return(r1-r2)})


DEU <- getData("GADM", country = "DEU", level = 1)
plot(DEU, add = T)

x<- lwgeom::st_transform_proj(r, c(CRS(4326)$proj4string, "+proj=ob_tran +o_proj=longlat +o_lon_p=-170 +o_lat_p=40 +lon_0=180 +to_meter=0.01745329"))
sf:st_crs(x) = NA # ?! This is mandatory for some reason
sf::st_crs(x) <- NA # ?! This is mandatory for some reason
sf::st_crs(x) <- 4326


values <- raster::raster(netcdfFilenames[1], varname = "T_2M_AV")
coords <- raster::brick(raster::raster(netcdfFilenames[1], varname = "lon"), 
                        raster::raster(netcdfFilenames[1], varname = "lat"))

extent(coords)
r_brick <- brick(values, xmn=extent(coords)[1], xmx=extent(coords)[2],
                 ymn=extent(coords)[3], ymx=extent(coords)[4],
                 crs=CRS("+proj=ob_tran +o_proj=longlat +o_lon_p=-162.0 +o_lat_p=39.25 +lon_0=180 +lat_0=0"))
plot(r_brick)
crs(r_brick) <- CRS("+proj=ob_tran +o_proj=longlat +o_lon_p=-170 +o_lat_p=40 +lon_0=180  +to_meter=0.01745329")
rRepro <- projectRaster(r_brick, crs = "+proj=longlat +datum=WGS84 +no_defs", method = "bilinear")
rRepro
plot(rRepro)

plot(coords)

library(quadmesh)
a <- mesh_plot(values, coords = coords) 
maps::map(add = TRUE)

DEU <- getData("GADM", country = "DEU", level = 1)

library(maptools)
library(rasterVis)
rOwn <- netToBrick.monthly(nc_data,varName = "T_2M_AV" )
plot(rOwn)
p <- levelplot(rOwn[[8]]$JAN, margin =NA, par.settings = RdBuTheme)
p + layer(sp.lines(DEU, lwd=0.8, col='darkgray'))

rMiro <- rOwn[[1]]$JAN # # With logical system and original coordinates
rMiro2 <- rOwn[[1]]$JAN # # With inverse rows and cols original coordinates
plot(rMiro2)
# Do a raster brick with coordinates and plotted

# Fill using Fabians as empty raster


rMiro
plot(rMiro)
plot(rFabian)

hola <- projectRaster(r, rFabian, method="ngb")

plot(hola)

st_crs(r) = NA # ?! (!)
st_crs(x) = 4326


#+to_meter=0.01745329"
plot(r)
dim(r)

lat <- ncvar_get(nc_data, "lat")
lon <- ncvar_get(nc_data, "lon")
empty.raster  <- raster(nrows=dim(r)[1], ncols=dim(r)[2], 
                        xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
                        crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), vals = 999)

hola <- projectRaster(r, rFabian, method="bilinear")
plot(hola)
nc_data$var$lat





lat <- ncvar_get(nc_data, "lat")
lon <- ncvar_get(nc_data, "lon")
varName <- "TOT_PREC"
varValues <- ncvar_get(nc_data, varName)



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



sink('D:/Downloads/KlimaStr.txt')
print(nc_data)
sink()



###############################################################################
# Function for all data
netToBrick.monthly <- function(nc_data, varName, mode = "kelvin"){
  fillvalue <- ncatt_get(nc_data, varName, "_FillValue")
  lat <- ncvar_get(nc_data, "lat")
  lon <- ncvar_get(nc_data, "lon")
  rowsRaster <-dim(lon)[2] # It should  be inversed?
  colsRaster <- dim(lon)[1] # it should be inversed?
  lat <- as.vector(lat)
  lon <- as.vector(lon)
  varValues <- ncvar_get(nc_data, varName)
  cat("######################");cat("\n")
  cat("# ");cat(varName); cat("\n")
  cat("######################");cat("\n")
  # Loop over and read in the data by years
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
                              crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), vals = 999)
      empty.raster <- raster( res = 0.02528052, 
                              xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
                              crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), vals = 999)
      # a Lon column
      #newLon <- seq(extent(empty.raster)[1], extent(empty.raster)[2], length.out = colsRaster)
      #newLon <- matrix(rep(newLon, rowsRaster), ncol = rowsRaster)
      #newLon <- as.vector(newLon)
      # a lat row
      #newLat <- seq(extent(empty.raster)[3], extent(empty.raster)[4], length.out = rowsRaster)
      #newLat <-  t(matrix(rep(newLat, colsRaster), ncol = colsRaster))
      #newLat <- as.vector(newLat)
        
    
      filled.raster <- rasterize(cbind(lon, lat), empty.raster, variable,fun= mean,  na.rm =TRUE) # Aint good --> Empty points around
      filled.raster.filter <- focal(filled.raster, 
                                    w = matrix(1,3,3), NAonly = TRUE,
                                    fun = mean, pad = TRUE, na.rm = TRUE )
      #r <- flip(r, direction='y')
     # if (mode == "kgm2"){
        #kg m-2 in 2km2 --> m3/1000 kg * 1000mm/1m *-->   mm in  2km2 but pixel size 
        # is different?!?! 
    #    r <- r*prod(res(r)
     # }
      return(filled.raster.filter)
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
# Fabians reprojeziertes TIFF
rFabian <- raster("E:/EDE_Daten/klima_sims/CEH_T_2M_2021-2050_monmean_reprojected.tif", band = 3)
plot(rFabian)
rSNAP <- raster("E:/EDE_Daten/klima_sims/reprojected/projected_CEH_T_2M_2021-2050_monmean.tif", band=3)
plot(rSNAP)
plot(rSNAP[[4]])
plot(rSNAP)
rGSI <- brick("E:/EDE_Daten/klima_sims/klima_sims/CEH_T_2M_2021-2050_monmean_resampled_gis.tif")
rGSI
plot(rGSI[[4]])

rSNAP3 <- raster("E:/EDE_Daten/klima_sims/reprojected/projected_HSURF.tif", band=1)
plot(rSNAP3)

test <- raster("E:/EDE_Daten/klima_sims/testDifference_R_snap.tif")
plot(test)
table(test[])
hist(test)
density(test)
  
