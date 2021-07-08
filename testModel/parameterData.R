################################################################################
# Prepare Parameters
################################################################################
options(encoding="utf-8")
# set working dir 
syncSharePath <- "c:/Users/Ramiro/bwSyncShare/EDE4.0_Hiwi/3PG"

################################################################################
# clean and tidy up the parameters
paramaterSchweiz <- read.csv(file.path(syncSharePath, "Schweiz_3PG.tsv"), sep = "\t")
head(paramaterSchweiz)
names(paramaterSchweiz)

species <- names(paramaterSchweiz)[4:ncol(paramaterSchweiz)]

dataHolder <- vector(mode = "list", length = length(species))

for (i in 1:length(species)){
  # Extract parameters
  dummy <- paramaterSchweiz[, species[i]]
  # tidy up minus stuff
  dummy <- gsub( "- ", "-", dummy)
  # get everything before brackets
  default <- as.numeric(gsub("\\(.*","",dummy))
  # get everythin between brackets
  # extract min and max and clean up
  parRange <- gsub(".*\\((.*)\\).*","\\1",dummy)
  min <- gsub("\\–.*","",parRange)
  max <- gsub(".*\\–","",parRange)
  min <- ifelse(min==default, NA, min)
  max <- ifelse(min==default, NA, max)
  # pack in df and name it
  df <- data.frame(cbind(dummy, default, min, max))
  colnames(df) <- paste(species[i], c("_orig", "", "_min", "_max"), sep = "")
  dataHolder[[i]] <- df
}

cleanTable <- do.call(cbind, dataHolder)

View(cleanTable)                             

write.table(cleanTable, file = file.path(syncSharePath, "parameterSchweiz.txt"),
          sep = "\t", row.names = FALSE)

################################################################################
# clean and tidy up the prior ranges
paramaterSchweiz <- read.csv(file.path(syncSharePath, "PriorRanges_Table2.tsv"), sep = "\t")
head(paramaterSchweiz)
names(paramaterSchweiz)

species <- names(paramaterSchweiz)[2:ncol(paramaterSchweiz)]
dataHolder <- vector(mode = "list", length = length(species))

for (i in 1:length(species)){
  # Extract parameters
  dummy <- paramaterSchweiz[, species[i]]
  # tidy up minus stuff
  dummy <- gsub( "- ", "-", dummy)
  # extract min and max 
  min <- gsub("\\–.*","",dummy)
  max <- gsub(".*\\–","",dummy)
  # pack in df and name it
  df <- data.frame(cbind(dummy, min, max))
  colnames(df) <- paste(species[i], c("_orig", "_min", "_max"), sep = "")
  dataHolder[[i]] <- df
}

cleanTable <- do.call(cbind, dataHolder)

View(cleanTable)                             

write.table(cleanTable, file = file.path(syncSharePath, "parameterSchweiz_prior.txt"),
            sep = "\t", row.names = FALSE)

################################################################################
# clean and tidy up the posterior ranges
paramaterSchweiz <- read.csv(file.path(syncSharePath, "PosteriorRanges_Table3.tsv"), sep = "\t")
head(paramaterSchweiz)
names(paramaterSchweiz)

species <- names(paramaterSchweiz)[2:ncol(paramaterSchweiz)]

dataHolder <- vector(mode = "list", length = length(species))

for (i in 1:length(species)){
  # Extract parameters
  dummy <- paramaterSchweiz[, species[i]]
  # tidy up minus stuff
  dummy <- gsub( "- ", "-", dummy)
  # get everything before brackets
  default <- as.numeric(gsub("\\(.*","",dummy))
  # get everythin between brackets
  # extract min and max and clean up
  parRange <- gsub(".*\\((.*)\\).*","\\1",dummy)
  min <- gsub("([0-9])\\–.*","\\1",parRange)
  max <- gsub(".*\\–","",parRange)
  # pack in df and name it
  df <- data.frame(cbind(dummy, default, min, max))
  colnames(df) <- paste(species[i], c("_orig", "", "_min", "_max"), sep = "")
  dataHolder[[i]] <- df
}

cleanTable <- do.call(cbind, dataHolder)

View(cleanTable)                             

write.table(cleanTable, file = file.path(syncSharePath, "parameterSchweiz_posterior.txt"),
            sep = "\t", row.names = FALSE)