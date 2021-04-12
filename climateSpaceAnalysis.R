

# already clustered with groups


myrange <- function(df){
  y = range(df$y)
  x = range(df$x)
  return(list(x = x, y= y))
}

checkRange <- function(xy, rangeXY){
  check <- FALSE
  if (xy[1] <= max(rangeXY$x) && xy[1] >= min(rangeXY$x) ) {
    check <- TRUE
  }else{
    check<- FALSE
    }
  if (xy[2] <= max(rangeXY$y) && xy[2] >= min(rangeXY$y)) {
    check <- TRUE
  }else{
    check<- FALSE
  }   
  return(check)
  }
  
climateSpaceComparison <- function(df, df_bestand, tolerance = c(1, 0.5), verbose = F){
  groups <- as.numeric(unique(df$group))
  climateSpace_tree_range <- vector(mode = "list", length = length(groups))
  for (i in 1:length(groups)){
    climateSpace_tree_range[[i]] <- myrange(df[df$group==i,])
  }
  
  climateSpace_stand <- as.matrix(df_bestand)
  df_bestand$group <- max(groups)+1
  
  climateSpace_tree <- as.matrix(df[, c("x", "y")])
  
  # Check to which group it belongs
  
  for (i in 1:length(climateSpace_stand[,1])){
    if(verbose) {cat("\n");cat(rep("-", 10));cat("\n")}
    if(verbose) {cat("Checking group for "); cat(climateSpace_stand[i,], sep = ", ");cat("\n")}
    distance <- colSums((t(climateSpace_tree) - climateSpace_stand[i,])^2)
    nearest.idx <- unique(which.min(distance))
    group <- as.numeric(df[nearest.idx,]$group)
    difference <- abs(abs(climateSpace_tree[nearest.idx, ] - climateSpace_stand[i,]) - tolerance)
    if(verbose) {cat("The difference is  "); cat(difference);cat("\n")}
    
    if (!any(tolerance < difference )){
      # Actually not needed because of tolerance
      if(verbose) {cat("Acceptable difference. The group is "); cat(group);cat("\n")}
      #check <- checkRange( climateSpace_stand[i,], climateSpace_tree_range[[group]])
      #if(check){ df_bestand[i,"group"] <- group}
    }else{
      if(verbose){cat("*Unacceptable difference!! The group is ")} 
      group <- group +1
      if(verbose) {cat(group);cat("\n")}
      
    }
    df_bestand[i,"group"] <- group
  }
  
  
  # check mean value
  meanTempPrec <- c(mean(df_bestand[,"x"]), mean(df_bestand[,"y"]))
  
  distance <- colSums((t(climateSpace_tree) - meanTempPrec)^2)
  nearest.idx <- unique(which.min(distance))
  group <- as.numeric(df[nearest.idx,]$group)
  difference <- abs(abs(climateSpace_tree[nearest.idx, ] - meanTempPrec) - tolerance)
    
  if(verbose) {cat("The difference is  "); cat(difference);cat("\n")}
    
  if (!any(tolerance < difference )){
      # Actually not needed because of tolerance
    if(verbose) {cat("Acceptable difference. The group is "); cat(group);cat("\n")}
      #check <- checkRange( climateSpace_stand[i,], climateSpace_tree_range[[group]])
      #if(check){ df_bestand[i,"group"] <- group}
  }else{
    if(verbose){cat("*Unacceptable difference!! The group is ")} 
    group <- group +1
    if(verbose) {cat(group);cat("\n")}
    }
  meanTempPrec <- c(meanTempPrec, group)
  
  
  #Analyze risk
  riskLegend <- c("sehr gering", "gering", "erhöht", "hoch", "sehr hoch")
  riskValues <- c(1:5)
  riskPercentage <-rep(0, 5)
  riskMatrix <- data.frame(riskLegend, riskValues, riskPercentage)

  # fill the matrix
  risk <- table(df_bestand[, "group"])
  risk <- round(risk / length(df_bestand[, "group"]), digits = 2)
  for (i in 1:length(riskValues)){
    if (riskValues[i] %in% names(risk)){
      riskMatrix[i, 3] <-risk[as.character(riskValues[i])]
    }

  }
  riskMatrix[riskMatrix$riskValues < 3, "riskValues"] <- 1
  riskMatrix[riskMatrix$riskValues == 3, "riskValues"] <- 2
  riskMatrix[riskMatrix$riskValues == 4, "riskValues"] <- 2
  riskMatrix$riskLegend <- sapply(riskMatrix$riskValues, function(x) riskLegend[x])
  
  if(meanTempPrec[3] < 3) meanTempPrec[3] <- 1
  if(meanTempPrec[3] == 3) meanTempPrec[3] <- 2
  if(meanTempPrec[3] == 4) meanTempPrec[3] <- 2
  
  overallRisk <- sum(riskMatrix[,2]*riskMatrix[,3])
 
  
  
  overall <- matrix(c("gesamtRisikowert", round(overallRisk,digits = 1), names(riskValues)[ceiling(overallRisk)],
                      "Durschnitt T-P ist im",meanTempPrec[3], names(riskValues)[ceiling(meanTempPrec[3])] ),
                    ncol = 3, byrow = T)
  colnames(overall) <-  c("variabel", "wert", "stufe")
  
  report <- list(percentage = riskMatrix,
                 overall = as.data.frame(overall,  row.names = NULL))
  
  
  # result
  result <- list(bestand = df_bestand, report = report)
  
return(result)
}

climateSpaceComparison.full <-  function(x, y, x_bestand, y_bestand,tolerance = c(1, 0.5),
                                         k=min(25,length(x)),n=4, verbose = F){
  require (tidyverse)
  require(viridis)
  require(RANN)
  require(RColorBrewer)
  
  df <- data.frame(x=x,y=y)
  df <- na.omit(df)
  
  # Estimate density by k-nearest neighbours
  # (a kernel density estimate might be used instead)
  result <- nn2(scale(df), k=k)
  df$kdist <- result$nn.dists[,k] # why taking last column
  
  # Divide points into n groups by density
  df$group <- 
    ceiling(rank(df$kdist, ties.method="random") *(n/nrow(df))) %>% 
    factor(levels=seq_len(n))
  
  # Add bestand as overlay
  df_bestand <- data.frame(x=x_bestand,y=y_bestand)
  df_bestand <- na.omit(df_bestand)
  
  result <- climateSpaceComparison(df,  df_bestand, tolerance=tolerance, verbose = verbose)
  return(result)
  
}
  

