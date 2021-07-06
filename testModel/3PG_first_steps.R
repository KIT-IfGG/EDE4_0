################################################################################
# 3PG stuff for meeting with Florian
################################################################################
options(encoding="utf-8")
# libraries
library(r3PG)
library(dplyr)
# set working dir 
syncSharePath <- "c:/Users/Ramiro/bwSyncShare/EDE4.0_Hiwi/3PG"
# load data from the package r3pg
load(file.path(syncSharePath, "solling.rda"))
load(file.path(syncSharePath, "grid_input.rda"))
# climate data
library(ProfoundData)
dbFilepath <- "E:/ProfoundData/ProfoundData.sqlite" # Big stuff, not in syncShare
setDB(dbFilepath)

################################################################################
# Compare the Parameters
################################################################################
# Read the parameters from Foresters 2021 Switzerland
paramaterSchweiz <- read.csv(file.path(syncSharePath, "Schweiz_3PG.tsv"), sep = "\t")
head(paramaterSchweiz)
# Tidy up the parameters
piceAbies <- paramaterSchweiz[, c("Parameter", "Picea.abies")]
head(piceAbies)
names(piceAbies)<- c("parameter", "piab")
meanVal <- piceAbies$piab
meanVal <- gsub( "- ", "-", meanVal)
meanVal <- as.numeric(gsub("\\(.*","",meanVal))
piceAbies$piab <- meanVal
# It seems some parameters have names not supported by the package
'%!in%' <- function(x,y)!('%in%'(x,y))
piceAbies[piceAbies$parameter %!in% i_parameters$parameter, ]

paramMistmach <- merge(i_parameters, piceAbies, by.x = "parameter", by.y = "parameter",
                        all = T)
# Need to understand it better
View(paramMistmach)
# Select only accepted parameters
piceAbies <- piceAbies[piceAbies$parameter %in% i_parameters$parameter, ]

# Compare parameters from Solling and Swiss
paramComparison <- merge(param_solling, piceAbies, by.x = "param_name", by.y= "parameter", all = TRUE,
                         suffixes = c("soll", "ch"))

head(paramComparison)
View(paramComparison)
# There are big differences, settings when running the model are important!!!
diffValues<- c("nHB", "aH", "fullCanAge")
diffValues.legend <- c("Power of d in the stem height relationship", "Constant in the stem height relationship",
                        "Age at canopy closure")
diffValues.source <- c("regression", "regression", "default (previous publications)")

cbind (paramComparison[paramComparison$param_name %in%  diffValues, ], diffValues.legend, diffValues.source)
# Differences have to do with allometry

################################################################################
# Compare with Solling
################################################################################
# Run with r3PG data
parameters.Solling <- param_solling[,c(1:2)]
names(parameters.Solling) <-  c("parameter", "piab")
parameters.Solling <- parameters.Solling[parameters.Solling$parameter %in% i_parameters$parameter, ]

out_3PG.solling <- run_3PG(
    site        = site_solling, 
    species     = species_solling, 
    climate     = climate_solling, 
    thinning    = thinn_solling,
    parameters  = parameters.Solling, 
    #size_dist   = meineVerteilung,
    settings    = list(light_model = 1, transp_model = 1, phys_model = 1, 
                         height_model = 1, correct_bias = 0, calculate_d13c = 0),
    check_input = TRUE, df_out = TRUE)
  # save output in the list

# Settings like in Foresters 2021, but correct_bias can be set to 1?!!
# settings = list(light_model = 2, transp_model = 2,
# phys_model = 2, height_model = 2, correct_bias = 1, calculate_d13c=0)
out_3PG.swiss <- run_3PG(
  site        = site_solling, 
  species     = species_solling, 
  climate     = climate_solling, 
  thinning    = thinn_solling,
  parameters  = piceAbies, 
  #size_dist   = meineVerteilung,
  settings    = list(light_model = 1, transp_model =1 , phys_model = 1,
                   height_model = 2, correct_bias = 0, calculate_d13c = 0),
  check_input = TRUE, df_out = TRUE)
# save output in the list
outComparsion <- list(solling = out_3PG.solling, swiss = out_3PG.swiss)

# Plot results
i_var <- c('stems_n',  'dbh', 'height', 'volume_mai', 'volume', 'volume_cum',
           "basal_area")
colors <- c("black", "red")
rows <- 3
par(mfrow =c(rows, ceiling(length(i_var)/rows)))
for (j in 1:length(i_var)){
  for (i in 1:length(outComparsion)){
    dummy <-  outComparsion[[i]][outComparsion[[i]]$variable ==i_var[j], ]
    if (length(dummy[,1]) != 0){
      if(i == 1){
        plot(dummy$date, dummy$value, main= i_var[j], type = "l", col = colors[i],
             ylim = c(0, max(dummy$value, na.rm = TRUE)*1.25))
      }else{
        lines(dummy$date, dummy$value, col = colors[i] )
      }
      legend("bottomleft", legend = names(outComparsion), col = colors, lty=1 )
    }
  }
}
par(mfrow=c(1,1))

################################################################################
# See how climate influences
################################################################################
# Site: set to the future
site_solling
mySite <- site_solling
mySite$from <- "2012-01"
mySite$to <-  "2082-01" # End of climate data

# species data
species_solling
mySpecies <-   species.grid
mySpecies$planted <- "2010-03"
#mySpecies$biom_stem <- NA
#mySpecies$biom_root <- NA
#mySpecies$biom_foliage <- NA

# thinning
thinn_solling
thinn.grid
age <- c(10,13, 17,23, 26, 30)
stems_n <- c(1500, 250, 200, 30, 20, 20 )

2500 - c(stems_n[1], sapply(2:6, FUN= function(x) sum(stems_n[1:x])))

sum(stems_n)
myThinning <- data.frame(list(species="piab", age= age, stems_n = stems_n),
                                 stem=rep(0.8, length(age)), root = rep(0.8, length(age)),
                                 foliage = rep(0.8, length(age)))
# parameter
myParameter <- param_solling[,c(1:2)]
names(myParameter) <-  c("parameter", "piab")



climateData <-  getData("CLIMATE_ISIMIP2BLBC", site = "solling_spruce", forcingDataset = "GFDLESM2M",
                                          collapse = FALSE)

names(climateData)
str(climateData)
myClimate <- climateData[c("rcp2p6", "rcp4p5", "rcp6p0", "rcp8p5")]
   # Fucntion to calculate variables for model and prepare outputs
# Data has daily resolution, convert it to monthly resolution
summarizeMonthly <- function(data, by = "total"){
  # think of having variables
  summaryCLIMATE <- function(subData){
    #site <- unique(subData$site)
    year <-unique(subData$year)
    month <- unique(subData$mo)
    tmp_max  <- ifelse("tmax_degC" %in% colnames(subData), mean(subData$tmax_degC, na.rm = T),NA)
    tmp_ave   <- ifelse("tmean_degC" %in% colnames(subData),mean(subData$tmean_degC, na.rm = T),NA)
    tmp_min  <- ifelse("tmin_degC" %in% colnames(subData),mean(subData$tmin_degC, na.rm = T),NA)
    prcp   <- ifelse("p_mm" %in% colnames(subData),sum(subData$p_mm, na.rm = T),NA)
    frost_days    <- ifelse("tmin_degC" %in% colnames(subData), sum(subData$tmin_degC < 0, na.rm = T),NA)
    srad  <-  ifelse("rad_Jcm2day" %in% colnames(subData), sum(subData$rad_Jcm2day, na.rm = T)* (1/1000)*(10000/1),
                     NA) # MJ m-2 d-1
    subSummary <- cbind(year, month, tmp_min , tmp_max, tmp_ave, prcp, srad,
                        frost_days)
    return(subSummary)
  }
  # Summarize
  data$foobar <- paste(data$year, data$mo, sep = " ")
  foobar <- unique(data$foobar)
  summaryObject <- lapply(1:length(foobar), function(x){
    subData <- data[data$foobar == foobar[x], ]
    subData <- summaryCLIMATE(subData)
    return(subData)
  })
  summaryObject <- as.data.frame(Reduce(f = function(...)rbind(...),x = summaryObject))
  summaryObject[,2:ncol(summaryObject)] <- apply(summaryObject[,2:ncol(summaryObject)], 2, function(x) as.numeric(as.character(x)))
  row.names(summaryObject) <- NULL
  return(summaryObject)
}
  # Prepare the data
for (i in 1:length(myClimate)){
  myClimate[[i]] <- summarizeMonthly(myClimate[[i]])
}
  # Add CO2 data
co2Data <- getData("CO2_ISIMIP", site = "solling_spruce", collapse = FALSE)
myCO2 <- co2Data[c("rcp2p6", "rcp4p5", "rcp6p0", "rcp8p5")]
head(myCO2$rcp2p6)

for (i in 1:length(myClimate)){
  dummy <- myCO2[[i]][myCO2[[i]]$year<2100,]
  dummy <- dummy[, c("year", "co2_ppm")]
  colnames(dummy) <- c("year", "co2")
  df <- merge(myClimate[[i]], dummy, by.x = "year", by.y = "year", all.x = TRUE)
  myClimate[[i]] <- df
}

# Run the model for each and compare
modelRuns <- vector(mode = "list", length = length(myClimate))

for (i in 1:length(myClimate)){
  # Run the model with the climate data
  out_3PG <- run_3PG(
    site        = mySite, 
    species     = mySpecies, 
    climate     = myClimate[[i]], 
    thinning    = thinn.grid, # thinn.grid
    parameters  = piceAbies, 
    #size_dist   = meineVerteilung,
   # settings    = list(light_model = 1, transp_model = 1, phys_model = 1, 
  #                     height_model = 1, correct_bias = 0, calculate_d13c = 0),
  # if swiss stuff other settings
    settings    = list(light_model = 2, transp_model = 2, phys_model = 2,
                     height_model = 2, correct_bias = 0, calculate_d13c = 0),
    check_input = TRUE, df_out = TRUE)
  # save output in the list
  modelRuns[[i]] <- out_3PG
  summary(myClimate[[i]])
}


# Plot stand variables 
rcps <- c("rcp2p6", "rcp4p5", "rcp6p0", "rcp8p5")
colors <- RColorBrewer::brewer.pal(4, "RdYlGn")
rows <- 3
par(mfrow =c(rows, ceiling(length(i_var)/rows)))
for (j in 1:length(i_var)){
  for (i in 1:length(modelRuns)){
    dummy <-  modelRuns[[i]][modelRuns[[i]]$variable ==i_var[j], ]
    if (length(dummy[,1]) != 0){
      if(i == 1){
        plot(dummy$date, dummy$value, main= i_var[j], type = "l", col = colors[i])
      }else{
        lines(dummy$date, dummy$value, col = colors[i] )
      }
      legend("topright", legend = rcps, col = colors, lty=1 )
    }
  }
}

par(mfrow = c(1,1))
