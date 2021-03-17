# load require libraries
require(raster)
require(sp)
require(sf)
require(rgdal)


# get the bioclim data from Worldclim
  #  2.5-minute spatial resolution (this is about 4.5 km at the equator)
bioclimData <- getData("worldclim",var="bio",res=2.5)
str(bioclimData, max.level = 2)

# get the files with the Chorological maps
  # see here https://www.sciencedirect.com/science/article/pii/S2352340917301981#ec0005
  # in my case already downloeaded and zipped
filesDir <- "E:/chorological_maps_dataset_20200527/chorological_maps_dataset/"
fileNames <- list.files(filesDir, pattern = "shp", full.names = T, recursive = T)
# sort out the files
  # polygon features (name suffix “plg”), which define continuous areas of occupancy of the species range 
  # point features (name suffix “pnt”), which identify more fragmented and isolated populations
  # species with reported synanthropic occurrences outside the natural range point and/or polygon shapefile “syn”)

shpOccupancy <- fileNames[grepl("plg", fileNames)]
shpOccupancy <- shpOccupancy[!grepl("syn", shpOccupancy)]
  # remove clip files. ?What do they mean
shpOccupancy <- shpOccupancy[!grepl("clip", shpOccupancy)]
  # little check
sppNames <- sapply(shpOccupancy, basename)
names(sppNames) <- NULL
sppNames<- gsub("_plg.shp", "", sppNames)


outDir <- "E:/chorological_maps_dataset_20200527/"

# A function to read a shapefile, extract the data of bioclim
extractAll <- function(sppSHP, sppName){
  # read the shp
  sppDistribution <- readOGR( dsn= sppSHP)
  # reduce the area
 # sppBioclim <- crop(bioclimData, sppDistribution)  
  # extract the data of the spp
  dfList <- extract(bioclimData, sppDistribution)
  # bind all data
  df <- do.call(rbind, dfList)
  # write the ouput
  write.table(df, paste(file.path(outDir, sppName), "_bioclim.txt", sep = ""),sep = "\t",
              col.names = T, row.names = F)

}

length(sppNames) == length(shpOccupancy)
head(sppNames)
head(shpOccupancy)

# loop and write

for (i in 94:length(sppNames)){
  cat("\n---------------------------------------------\n")
  cat( sppNames[i])
  cat("\n")
  extractAll(shpOccupancy[i], sppNames[i])
}


#------------------------------------------------------------------------------#
# get the files with the data
fileNames <- list.files(outDir, pattern = "_bioclim.txt", full.names = T, recursive = T)

# read the data
df <- read.csv(fileNames[58], sep = "\t")
head(df)
summary(df)


summary(df[,1], na.rm = T)

suffixes <- c("min", "1stQ", "median", "mean", "3rdQ", "max")
variables <- names(df)


summaryBIO <- function(aFile){
  df <- read.csv(aFile, sep = "\t")
  colTemperature <- c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11)
  for (i in 1:length(colTemperature)){
    df[,colTemperature[i]] <-  df[,colTemperature[i]]/10
  }
  holder <- c()
  for (i in 1:length(variables)){
    dummy <- df[, variables[i]]
    summaryDummy <- summary(dummy)
    summaryDummy <- summaryDummy[1:6]
    names(summaryDummy) <- paste(variables[i],suffixes, sep = "_" )
    holder <-c(holder, summaryDummy)
  }
  return(holder)
}

listSummaries <- lapply(fileNames, summaryBIO)

names(listSummaries) <- sppNames

df <- do.call(rbind, listSummaries)
head(df)
df <- cbind (rownames(df), df)
colnames(df)[1] <- "baumart"
df <- as.data.frame(df)
write.table(df, paste(file.path(outDir,"summary_allspp"), "_bioclim.txt"),sep = "\t",
            col.names = T, row.names = F)


# Please note that the temperature data are in °C * 10. 
# Species of Interest
filename <- "E:/EDE_Daten/Baumarten - SpeciesOfInterest.tsv"
sppInterest <- read.csv(filename, sep = "\t", header = T)

unique(df$baumart)

sppInterest_pattern <- gsub(" ", "_", sppInterest$baumart_wiss_2)


df_sppInterest <- df[grepl( paste(sppInterest_pattern, collapse = "|"),df$baumart),]


write.table(df_sppInterest, paste(file.path(outDir,"summary_sppInterest"), "_bioclim.txt"),sep = "\t",
            col.names = T, row.names = F)


