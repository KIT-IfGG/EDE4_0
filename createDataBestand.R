# libraries
#install.packages("ProfoundData")
require(ProfoundData)
#?ProfoundData
#vignette("ProfoundData", package="ProfoundData")

citation("ProfoundData")

#downloadDatabase("E:/")
dbFilepath <- "E:/ProfoundData/ProfoundData.sqlite"

rawPath <-"/e/ProfoundData/ProfoundData.sqlite"

Sys.info()["sysname"]

setDB(dbFilepath)

browseData()

bestaende <- getData("SITES")

bestaende <-bestaende[-10, ]

sites <- bestaende$site
names(sites) <- paste("Bestand_", c(1:length(sites)),sep="")

# subset to german sites
germanSites <- sites[ sites %in% c("peitz", "solling_beech", "solling_spruce")]



bestandNanmes <- paste("bestand",c("A", "B", "C"), sep = "_")

createDataBestand <- function(i){
  dummyBestand <- vector(mode= "list", length = 6)
  names(dummyBestand) <- c("Beschreibung", "Boden", "Bestand", "Baum",
                           "Klima_Hist", "Klima_Pro")
  df <- bestaende[bestaende$site==germanSites[i],] 
  df$site[df$site==germanSites[i]] <- bestandNanmes[i]
  dummyBestand$Beschreibung <- df
  
  df <- getData(dataset = "SOIL", site = germanSites[i])
  df$site[df$site==germanSites[i]] <- bestandNanmes[i]
  dummyBestand$Boden <- df
  
  df <- getData(dataset = "STAND", site = germanSites[i])
  df$site[df$site==germanSites[i]] <- bestandNanmes[i]
  dummyBestand$Bestand <- df
  
  df <- getData(dataset = "TREE", site = germanSites[i])
  df$site[df$site==germanSites[i]] <- bestandNanmes[i]
  dummyBestand$Baum <- df
  
  df <- getData(dataset = "CLIMATE_LOCAL", site = germanSites[i])
  df$site[df$site==germanSites[1]] <- bestandNanmes[i]
  dummyBestand$Klima_Hist <- df
  
  df <- getData(dataset = "CLIMATE_ISIMIP2BLBC", site = germanSites[i],collapse = FALSE )
  for (k in 1:length(df)){
    for (j in 1:length(df[[i]])){
      df[[k]][[j]]$site[df[[k]][[j]]$site==germanSites[1]] <- bestandNanmes[i]
      
    }
  }
  dummyBestand$Klima_Pro <- df
  
  return(dummyBestand)
  
}


hola <-  df <- getData(dataset = "CLIMATE_LOCAL", site = germanSites[1], )
?summarizeData

Bestand_A <- createDataBestand(1)
Bestand_B <- createDataBestand(2)
Bestand_C <- createDataBestand(3)



saveRDS(Bestand_A, file = "E:/EDE_Daten/Bestand_A.rds")
saveRDS(Bestand_B, file = "E:/EDE_Daten/Bestand_B.rds")
saveRDS(Bestand_C, file = "E:/EDE_Daten/Bestand_C.rds")


######## BIOCLIM DATA #################################

require(dismo)

calculateBioVarHist <- function(BestandObject){
  dummy <- BestandObject[["Klima_Hist"]]
  years <- unique(dummy$year)
  holder <- vector(mode = "list", length = length(years))
  for (i in 1:length(years)){
    df <- dummy[dummy$year== years[i], ]
    head(df)
    temp <- aggregate(df[, c("mo", "tmax_degC", "tmean_degC", "tmin_degC")],
                      FUN="mean", by = list(df$mo))
    prec <- aggregate(df[, c("mo", "p_mm")],
                      FUN="sum", by = list(df$mo))
    biodf <- biovars(prec$p_mm, temp$tmin_degC, temp$tmax_degC)
    bioRow <-  c(years[i], biodf)
    names(bioRow) <- c("year", paste("bio", c(1:length(biodf))))
    holder[[i]] <- c(years[i], biodf)
    
  }
  bioclimBestand <- do.call("rbind", holder)
  colnames(bioclimBestand) <- c("year", paste("bio",
                                           c(1:19), sep= ""))
  meanbioclimBestand <- apply(bioclimBestand,MARGIN = 2, mean)
  meanbioclimBestand <- meanbioclimBestand[2:length(meanbioclimBestand)]
  BestandObject[["Bioclim_full_hist"]] <- as.data.frame(bioclimBestand)
  BestandObject[["Bioclim_mean_hist"]] <- as.data.frame(meanbioclimBestand)
  return(BestandObject)
}


Bestand_A <- readRDS("E:/EDE_Daten/Bestand_A.rds")
Bestand_B <- readRDS("E:/EDE_Daten/Bestand_B.rds")
Bestand_C <- readRDS("E:/EDE_Daten/Bestand_C.rds")

Bestand_A <- calculateBioVarHist(Bestand_A)
Bestand_B <- calculateBioVarHist(Bestand_B)
Bestand_C <- calculateBioVarHist(Bestand_C)



saveRDS(Bestand_A, file = "E:/EDE_Daten/Bestand_A.rds")
saveRDS(Bestand_B, file = "E:/EDE_Daten/Bestand_B.rds")
saveRDS(Bestand_C, file = "E:/EDE_Daten/Bestand_C.rds")




calculateBioVarHist.simple <- function(df_clima){
 
  years <- unique(df_clima$year)
  holder <- vector(mode = "list", length = length(years))
  for (i in 1:length(years)){
    df <- df_clima[df_clima$year== years[i], ]

    temp <- aggregate(df[, c("mo", "tmax_degC", "tmean_degC", "tmin_degC")],
                      FUN="mean", by = list(df$mo))
    prec <- aggregate(df[, c("mo", "p_mm")],
                      FUN="sum", by = list(df$mo))
    biodf <- biovars(prec$p_mm, temp$tmin_degC, temp$tmax_degC)
    bioRow <-  c(years[i], biodf)
    names(bioRow) <- c("year", paste("bio", c(1:length(biodf))))
    holder[[i]] <- c(years[i], biodf)
  }
  bioclimBestand <- do.call("rbind", holder)
  colnames(bioclimBestand) <- c("year", paste("bio",c(1:19), sep= ""))
    
  return(bioclimBestand)
}


calculateBioVarHist.pro <- function(BestandObject){
  dummy <- BestandObject[["Klima_Pro"]]
  dummyholder <- BestandObject[["Klima_Pro"]]
  
  forcingCondition <- names(dummy)
  for (i in  1:length(forcingCondition)){
    rcp <- names(dummy[[i]])
    for (j in 1:length(rcp)){
      dummyholder[[i]][[j]] <-calculateBioVarHist.simple( dummy[[i]][[j]])
      
    }
  }
  
  BestandObject[["Bioclim_full_pro"]] <- dummyholder
  return(BestandObject)

}


Bestand_A <- readRDS("E:/EDE_Daten/Bestand_A.rds")
Bestand_B <- readRDS("E:/EDE_Daten/Bestand_B.rds")
Bestand_C <- readRDS("E:/EDE_Daten/Bestand_C.rds")

Bestand_A <- calculateBioVarHist.pro(Bestand_A)

Bestand_B <- calculateBioVarHist.pro(Bestand_B)
Bestand_C <- calculateBioVarHist.pro(Bestand_C)



saveRDS(Bestand_A, file = "E:/EDE_Daten/Bestand_A.rds")
saveRDS(Bestand_B, file = "E:/EDE_Daten/Bestand_B.rds")
saveRDS(Bestand_C, file = "E:/EDE_Daten/Bestand_C.rds")



