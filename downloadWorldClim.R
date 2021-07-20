################################################################################
# Download the monthly worldclim data
################################################################################
# Links to the files
links <- c("https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmin_1960-1969.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmin_1970-1979.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmin_1980-1989.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmin_1990-1999.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmin_2000-2009.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmin_2010-2018.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmax_1960-1969.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmax_1970-1979.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmax_1980-1989.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmax_1990-1999.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmax_2000-2009.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmax_2010-2018.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_prec_1960-1969.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_prec_1970-1979.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_prec_1980-1989.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_prec_1990-1999.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_prec_2000-2009.zip",
           "https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_prec_2010-2018.zip")

# Loop over file links and download the file at specified folder
for (i in 1:length(links)){
  download.file(links[i], destfile = file.path("E:/EDE_Daten/worldClimMonth/", basename(links[i])))
}

# Find all downloaded zip files in the folder
zipFiles <- list.files("E:/EDE_Daten/worldClimMonth/", pattern = "zip", full.names = TRUE)

# Loop over zip files and unzip the file at specified folder (in this case, same folder)
for (i in 1:length(zipFiles)){
  unzip(zipFiles[i], exdir = "E:/EDE_Daten/worldClimMonth/.", overwrite = FALSE)
}

# Find all unzipped files 
unzipFiles <- list.files("E:/EDE_Daten/worldClimMonth/", pattern = ".tif", full.names = TRUE)
# load required library for reading the spatial data
require(raster)
  #Select the tmax files
unzipSel <- unzipFiles[grepl("tmax", unzipFiles)]
  # create names
layerNames <- sapply(unzipSel, function(x) gsub(".tif", "", basename(x)))
names(unzipSel) <- layerNames[]
  # create the spatial data and save
r <- stack(unzipSel)
saveRDS(r,file =  "E:/EDE_Daten/worldClimMonth/wc2.1_2.5m_tmax_1961_2018.rds" )

  #Select the tmin files
unzipSel <- unzipFiles[grepl("tmin", unzipFiles)]
# create names
layerNames <- sapply(unzipSel, function(x) gsub(".tif", "", basename(x)))
names(unzipSel) <- layerNames[]
# create the spatial data and save
r <- stack(unzipSel)
saveRDS(r,file =  "E:/EDE_Daten/worldClimMonth/wc2.1_2.5m_tmin_1961_2018.rds" )

#Select the prech files
unzipSel <- unzipFiles[grepl("prec", unzipFiles)]
# create names
layerNames <- sapply(unzipSel, function(x) gsub(".tif", "", basename(x)))
names(unzipSel) <- layerNames[]
# make roll sum and write the name data
library(raster)
rasterOptions(tmpdir="F:/rTemp")
pb <- txtProgressBar(min = 0, max = length(unzipSel), style = 3)
for (i in 1:length(unzipSel)){
  central <- i
  setTxtProgressBar(pb, i)
  if (i == length(unzipSel)){
    lower <- i - 1
    upper <-  i - 2
  }else if (i == 1){
    lower <- i + 1
    upper <- i + 2
  }else{
    lower <- i - 1
    upper <- i + 1
  }
  r.central <- raster(unzipSel[central])
  r.lower <- raster(unzipSel[lower])
  r.upper <- raster(unzipSel[upper])
  rsum <- r.central + r.lower + r.upper
  
  dummyName <- file.path("F:/worldClimMonth_proc/", gsub(".tif", "_rsum.tif",basename(unzipSel[central])))
  writeRaster(x = rsum, filename = dummyName, format="GTiff")
  rm(r.central, rsum, r.upper, r.lower, dummyName)
}
close(pb)

 
# Find all unzipped files 
unzipSel <- list.files("F:/worldClimMonth_proc/", pattern = "_rsum.tif", full.names = TRUE)
# create the spatial data and save
r <- stack(unzipSel)
saveRDS(r,file =  "F:/worldClimMonth_proc/wc2.1_2.5m_prec_1961_2018.rds" )


