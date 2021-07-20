################################################################################
# Prepare Equations Biomass for 3PG
################################################################################
options(encoding="utf-8")
# set working dir 
syncSharePath <- "c:/Users/Ramiro/bwSyncShare/EDE4.0_Hiwi/3PG"
################################################################################
# clean and tidy up the coefficients
bioEquations <- read.csv(file.path(syncSharePath, "TableA5.csv"))
head(bioEquations)


targetVariables <- c("ln.ß0.", "ß.for.ln.d.", "ß.for.ln.Age.", "ß.for.BA",
                     "ß.for.ln.TPH.",  "ß.for.ln.Latitude.", "ß.for.Precipitation",
                     "ß.for.Temperature")

cleanCoeff <- function(coeffName){
  raw <- bioEquations[[coeffName]]
  value <- as.numeric(gsub("\\(.*","",raw))
  se <- gsub(".*\\((.*)\\).*","\\1",raw)
  return(cbind(value, se, raw))
}

coeffs <- lapply(targetVariables, cleanCoeff)
# Names
namesCoeffs <- paste("coeff", c("0", "_lnd", "_lnAge", "_BA", "_lnTPH", "_lnLAT", "_PP", "_Tmean"),
      sep ="")

length(coeffs) == length(namesCoeffs)

for (i in 1:length(coeffs)){
  colnames(coeffs[[i]]) <- paste(namesCoeffs[i], colnames(coeffs[[i]]), sep="_")
}


View(coeffs[[4]])
head(coeffs[[6]])
cleaned <- do.call("cbind", coeffs)
head(cleaned)
################################################################################
# Clean and tidy up the ranges
targetVariables <- c("d.range..cm.", "BA.range..m2.per.ha.",  "Age.range..years.",
                     "TPH.range..trees.per.ha.", "Latitude.range..degrees.",
                     "Mean.annual.precipitation..mm.", "Mean.annual.temperature..degrees.Celcius.")

newNames <- c("d_cm", "BA_m2ha.", "Age_y", "TPH_treesha",  "LAT_deg", "PP_mm", "Tmean_degC")

varName <- targetVariables[[1]]
cleanRange <- function(varName){
  raw <- bioEquations[[varName]]
  lower <- gsub("(.*)\\/(.*)\\/(.*)", "\\1", raw)
  mean <- gsub("(.*)\\/(.*)\\/(.*)", "\\2", raw)
  upper <- gsub("(.*)\\/(.*)\\/(.*)", "\\3", raw)
  return(cbind(lower, mean, upper, raw))
}

ranges <- lapply(targetVariables, cleanRange)


# Names
for (i in 1:length(ranges)){
  colnames(ranges[[i]]) <- paste(newNames[i], colnames(ranges[[i]]), sep="_")
}

cleanedRange <- do.call("cbind", ranges)
head(cleanedRange)

newBioEq <- data.frame(cbind(bioEquations[, 1:3], cleaned, bioEquations[, 12:46], cleanedRange))

head(newBioEq)

write.table(newBioEq, file = file.path(syncSharePath, "biomassEquations_2017.txt"),
            sep = "\t", row.names = FALSE)
