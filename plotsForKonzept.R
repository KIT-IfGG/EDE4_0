source("E:/GitHub/EDE40/processData/plotSppClimate.R")
source("E:/GitHub/EDE40/processData/calculateStandVariables.R")
source("E:/GitHub/EDE40/processData/climateSpaceAnalysis.R")


BaumartKlimaSpace <- readRDS(file = "E:/EDE_Daten/BaumartKlimaSpace.rds")

names(BaumartKlimaSpace)

sppOfInterest.names <- names(BaumartKlimaSpace)[c(1,13,14,15,17)]



temperature <- paste("bio",c(1, 5, 6, 1, 1,  8,9, 10, 11), sep = "")
temperatureLegend <- c("Annual Mean Temperature", "Max Temperature of Warmest Month",
                       "Min Temperature of Coldest Month", "Annual Mean Temperature",
                       "Annual Mean Temperature", "Mean Temperature of Wettest Quarter",
                       "Mean Temperature of Driest Quarter", "Mean Temperature of Warmest Quarter",
                       " Mean Temperature of Coldest Quarter")

precipation <- paste("bio", c(12, 12, 12,  13, 14, 16, 17, 18, 19), sep = "")
precipationLegend <- c("Annual Precipitation", "Annual Precipitation", "Annual Precipitation",
                       "Precipitation of Wettest Month",
                       " Precipitation of Driest Month", "Precipitation of Wettest Quarter",
                       "Precipitation of Driest Quarter", "Precipitation of Warmest Quarter",
                       "Precipitation of Coldest Quarter")

# Loop over variables
for (i in 1:length(temperature)){
  
  # Obtain range of variable for each spp
  xaxis <- range(sapply(sppOfInterest.names, function(x)range(BaumartKlimaSpace[[x]][, temperature[i]], na.rm = T)))
  yaxis <- range(sapply(sppOfInterest.names, function(x)range(BaumartKlimaSpace[[x]][, precipation[i]], na.rm = T)))
  
  # Make plot together with all species
  for (j in 1:length(sppOfInterest.names)){
    
    dummy <-  BaumartKlimaSpace[[sppOfInterest.names[j]]]
    
    jpeg(file.path("E:/EDE_Daten/", paste(precipation[i],"_",  temperature[i], "_", sppOfInterest.names[j],
                                          ".jpg", sep = "")), 
         width = 330, height = 330, units = "mm",
         res = 300)
    
    plotClimateSpaceHull(x =dummy[, precipation[i]],
                         y =dummy[, temperature[i]],
                         ylab=temperatureLegend[i],
                         xlab=precipationLegend[i], 
                         main = sppOfInterest.names[j],
                         alpha = 1, lwd=1,
                         pch = 15, cex = 0.5, res = 10)
    
    
  dev.off()
  }
  
}
  
plotClimateSpaceHull.Bestand(x = BaumartKlimaSpace[[sppOfInterest.names[j]]][, precipation[i]],
                             y =BaumartKlimaSpace[[sppOfInterest.names[j]]][, temperature[i]],
                             x_bestand = dummyBestand[,precipation[i]],
                             y_bestand = dummyBestand[, temperature[i]],
                             ylab=temperatureLegend[i],
                             xlab=precipationLegend[i], 
                             main = "Eignung Fichte - Bestand A",
                             alpha = 1, xaxis = xaxis, yaxis = yaxis,
                             pch = 15, cex = 0.5,  tolerance = c(0.5,0.5), lwd= 1,
                             spp = "Fichte")



# Simplyfy range




# 


dummy <- BaumartKlimaSpace[["Picea_abies"]]
head(dummy)
dummyBestand <- Bestand$Bioclim_full_hist



xaxis <- range(sapply(precipation, function(x)range(dummy[, x], na.rm = T)),
               sapply(precipation, function(x)range(dummyBestand[, x], na.rm = T)))
yaxis <- range(sapply(temperature, function(x)range(dummy[, x], na.rm = T)),
               sapply(temperature, function(x)range(dummyBestand[, x], na.rm = T)))



riskAnalysis <- vector(mode = "list", length = length(temperature))


for (i in 1:length(temperature)){
  plotClimateSpaceHull.Bestand(x = dummy[, precipation[i]], y = dummy[, temperature[i]], 
                               x_bestand = dummyBestand[,precipation[i]],
                               y_bestand = dummyBestand[, temperature[i]],
                               ylab=temperatureLegend[i],
                               xlab=precipationLegend[i], 
                               main = "Eignung Fichte - Bestand A",
                               alpha = 1, xaxis = xaxis, yaxis = yaxis,
                               pch = 15, cex = 0.5,  tolerance = c(0.5,0.5), lwd= 1,
                               spp = "Fichte")
  hi <- climateSpaceComparison.full(x = dummy[, precipation[i]], y = dummy[, temperature[i]], 
                                    x_bestand = dummyBestand[,precipation[i]],
                                    y_bestand = dummyBestand[, temperature[i]],
                                    tolerance = c(0.5,0.5), verbose = F)
  riskAnalysis[[i]] <- hi$report$overall$wert
}
i <- 1
hi$report$overall[1,2]
