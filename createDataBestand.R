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
  
  df <- getData(dataset = "CLIMATE_ISIMIP2B", site = germanSites[i])
  df <- df[df$forcingCondition=="rcp2p6",]
  df$site[df$site==germanSites[i]] <- bestandNanmes[i]
  dummyBestand$Klima_Pro <- df
  
  return(dummyBestand)
  
}




Bestand_A <- createDataBestand(1)
Bestand_B <- createDataBestand(2)
Bestand_C <- createDataBestand(3)



saveRDS(Bestand_A, file = "E:/EDE_Daten/Bestand_A.rds")
saveRDS(Bestand_B, file = "E:/EDE_Daten/Bestand_B.rds")
saveRDS(Bestand_C, file = "E:/EDE_Daten/Bestand_C.rds")





