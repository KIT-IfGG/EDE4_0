
# Species of Interest
filename <- "E:/EDE_Daten/Baumarten - SpeciesOfInterest.tsv"
sppInterest <- read.csv(filename, sep = "\t", header = T)

# read in the raw files
filenames <- list.files("txt", path = "E:/chorological_maps_dataset_20200527/",
                        full.names = T)

sppInterest_pattern <- gsub(" ", "_", sppInterest$baumart_wiss_2)


matches <- filenames[grepl(paste(paste("\\b", sppInterest_pattern, "_bioclim.txt", sep =""),
                                  collapse = "|"), 
                         filenames)]
sppNames <- gsub(" _bioclim.txt", "", sapply(matches, basename))
names(sppNames) <- NULL

names(matches) <- sppNames

cbind(matches, sppInterest_pattern)

BaumartKlimaSpace <-lapply(matches, function(x){
  df <- read.csv(x, sep = "\t", header = T)
  colTemperature <- c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11)
  for (i in 1:length(colTemperature)){
    df[,colTemperature[i]] <-  df[,colTemperature[i]]/10
  }
  return(df)
})


saveRDS(BaumartKlimaSpace, file = "E:/EDE_Daten/BaumartKlimaSpace.rds")
