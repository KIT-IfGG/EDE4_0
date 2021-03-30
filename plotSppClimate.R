# scatter plot with density http://www.logarithmic.net/pfh-files/blog/01509162940/scatter.html


plotClimateSpace <- function(x, y,k=min(25,length(x)),n=4, alpha = 0.5, pch = 20,
                             cex =0.4, ...){
  
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
  unique(df$group)
  
  # Assign colors
  my.cols <- rev(brewer.pal(n, "RdYlBu"))
  df$color <- sapply(df$group, function(x){ my.cols[x]})
  
  # plot 
  plot(df$x, df$y, pch=pch , cex=cex, col = alpha(df$color, alpha), ...)
  legend("topright",title= paste("Fraction of" , length(df[,1]), "points", sep = " "),
         legend = rep(" 25 %", 4), pch = pch, col = my.cols, cex = 0.75, bty = "n")
  
  #calculate hull
  hpts <- chull(df[df$group == 4, c("x","y")])
  hpts <- c(hpts, hpts[1])
  lines(df[hpts, c("x","y") ], col = my.cols[4], lwd= lwd)
  
  
}


plotClimateSpace.Bestand <- function(x, y, x_bestand, y_bestand,k=min(25,length(x)),n=4, alpha = 0.5, pch = 20,
                                     cex =0.4, verbose = F, ...){
 
  require (tidyverse)
  require(viridis)
  require(RANN)
  require(RColorBrewer)

  # calculate range for plot
  yaxis <- range(c(y, y_bestand), na.rm = T)
  xaxis <- range(c(x, x_bestand), na.rm = T)
  
  
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
  unique(df$group)
  
  # Assign colors
  my.cols <- rev(brewer.pal(n, "RdYlBu"))
  df$color <- sapply(df$group, function(x){ my.cols[x]})
  
  # plot 
  plot(df$x, df$y, pch=pch , cex=cex, col = alpha(df$color, alpha),
       ylim = yaxis, xlim= xaxis,...)
  legend("topright",title= paste("Fraction of" , length(df[,1]), "points", sep = " "),
         legend = rep(" 25 %", 4), pch = pch, col = my.cols, cex = 0.75, bty = "n")
  #calculate hull
  hpts <- chull(df[df$group == 4, c("x","y")])
  hpts <- c(hpts, hpts[1])
  lines(df[hpts, c("x","y") ], col = my.cols[4], lwd=lwd)
  
  
  

  # Add bestand as overlay
  df_bestand <- data.frame(x=x_bestand,y=y_bestand)
  df_bestand <- na.omit(df_bestand)
  
  #calculate hull
  hpts <- chull(df_bestand)
  hpts <- c(hpts, hpts[1])
  lines(df_bestand[hpts, ], col = "darkgreen", lwd=lwd)
  
  bestand.cols <- rev(brewer.pal(n, "Greens"))
  #z <- MASS::kde2d(df_bestand[,1], df_bestand[,2], n=50)
  #contour(z, drawlabels=TRUE, nlevels=n, col=bestand.cols, lwd= 2, add=TRUE)
  abline(h=mean(df_bestand[,2]), v=mean(df_bestand[,1]), lwd=lwd, col = "darkgreen")
  points(df_bestand$x, df_bestand$y, pch="x", col= "darkolivegreen", cex = 1)
  legend("bottomleft",title= "Bestand Klima (aktuell) ", 
         legend = c("mittelwert", "einzelwert", "klimahülle"),
         pch=c("+", "X", "-"), 
         col = c("darkgreen", "darkolivegreen", "darkgreen"), cex = 0.75, bty = "n")
  
  #calculate risk
  report <- climateSpaceComparison(df,  df_bestand, tolerance=tolerance)
  legend("bottomright", title = paste("Risikowert: ",
                                      report$report$overall[1,3], " (",
                                      report$report$overall[1,2], ")",
                                      "\n", "Werteverteilung:" , sep=""),
         legend = paste(round(as.numeric(report$report$percentage$riskPercentage)*100, digits=1), "% ",  "- ",
                        report$report$percentage$riskLegend, sep =""),
         col = c(my.cols, "white"),  pch = pch, cex = 0.75, bty = "n")
  

  
}




plotClimateSpaceHull <- function(x, y,k=min(25,length(x)),n=4, alpha = 0.5, pch = 20,
                             cex =0.4, xaxis = NULL, yaxis=NULL, lwd = 2,...){
  
  require (tidyverse)
  require(viridis)
  require(RANN)
  require(RColorBrewer)
  require(raster)
  
  # calculate range for plot
  if(is.null(yaxis)) yaxis <- range(y, na.rm = T)
  if(is.null(xaxis)) xaxis <- range(x, na.rm = T)
  
  atx <- seq(0, ceiling(max(xaxis)/1000)*1000,by=500)
  aty <- seq(ceiling(min(yaxis)/10)*1000, ceiling(max(yaxis)/10)*1000,by=500)
  xlabels <- seq(0, ceiling(max(xaxis)/1000)*1000,by=500)
  ylabels <- seq(ceiling(min(yaxis)/10)*10, ceiling(max(yaxis)/10)*10,by=5)
  
  
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
  
  
  # Assign colors
  my.cols <- rev(brewer.pal(n, "RdYlBu"))
  df$color <- sapply(df$group, function(x){ my.cols[x]})
  
  groups <- as.numeric(unique(df$group))
  
  
  empty.raster  <- raster(nrows=max(yaxis)*100-min(yaxis)*100, ncols=max(xaxis)-min(xaxis), 
                          xmn=min(xaxis), xmx=max(xaxis), ymn=min(yaxis)*100, ymx=max(yaxis)*100,
                          resolution = 50)
  
  new.raster <- rasterize(x=cbind(df$x, df$y*100), y= empty.raster, field =as.numeric(df[,4]), fun= min)
  
  
  plot(new.raster, col = my.cols, axes = FALSE, legend = FALSE, ...)


  legend("topright",title= paste("Fraction of" , length(df[,1]), "points", sep = " "),
         legend = paste(seq(100/n, 100, by= 100/n), "%"), pch = 15, col = my.cols, cex = 0.75, bty = "n")
  
  axis(1, at=atx, labels=xlabels, las=0)
  
  axis(2, at=aty, labels=ylabels,  las=0)
  
  
}


plotClimateSpaceHull.Bestand <- function(x, y, x_bestand, y_bestand,k=min(25,length(x)),n=4, alpha = 0.5, pch = 20,
                                     cex =0.4, verbose = F,  xaxis = NULL, yaxis=NULL,
                                     lwd = 2, tolerance = c(0.5,0.5), ...){
  require (tidyverse)
  require(viridis)
  require(RANN)
  require(RColorBrewer)
  require(raster)
  
  # calculate range for plot
  if(is.null(yaxis)) yaxis <- range(c(y, y_bestand), na.rm = T)
  if(is.null(xaxis)) xaxis <- range(c(x, x_bestand), na.rm = T)
  
  atx <- seq(0, ceiling(max(xaxis)/1000)*1000,by=500)
  aty <- seq(ceiling(min(yaxis)/10)*1000, ceiling(max(yaxis)/10)*1000,by=500)
  xlabels <- seq(0, ceiling(max(xaxis)/1000)*1000,by=500)
  ylabels <- seq(ceiling(min(yaxis)/10)*10, ceiling(max(yaxis)/10)*10,by=5)
  
  
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
  
  
  # Assign colors
  my.cols <- rev(brewer.pal(n, "Blues"))
  df$color <- sapply(df$group, function(x){ my.cols[x]})
  
  groups <- as.numeric(unique(df$group))
  
  
  empty.raster  <- raster(nrows=max(yaxis)*100-min(yaxis)*100, ncols=max(xaxis)-min(xaxis), 
                          xmn=min(xaxis), xmx=max(xaxis), ymn=min(yaxis)*100, ymx=max(yaxis)*100,
                          resolution = 50)
  
  new.raster <- rasterize(x=cbind(df$x, df$y*100), y= empty.raster, field =as.numeric(df[,4]), fun= min)
  
  
  plot(new.raster, col = my.cols, axes = FALSE, legend = FALSE, ...)
  
  
  legend("topright",title= paste("Fraction of" , length(df[,1]), "points", sep = " "),
         legend = paste(seq(100/n, 100, by= 100/n), "%"), pch = 15, col = my.cols, cex = 0.75, bty = "n")
  
  axis(1, at=atx, labels=xlabels, las=0)
  
  axis(2, at=aty, labels=ylabels,  las=0)
  
  
  # Add bestand as overlay
  df_bestand <- data.frame(x=x_bestand,y=y_bestand)
  df_bestand <- na.omit(df_bestand)
  
  #calculate hull
  hpts <- chull(df_bestand)
  hpts <- c(hpts, hpts[1])
  df_bestand_100 <- df_bestand
  df_bestand_100$y <- df_bestand_100$y *100
  
  lines(df_bestand_100[hpts, ], col = "darkgreen", lwd=lwd)
  
  bestand.cols <- rev(brewer.pal(n, "Greens"))
  #z <- MASS::kde2d(df_bestand[,1], df_bestand[,2], n=50)
  #contour(z, drawlabels=TRUE, nlevels=n, col=bestand.cols, lwd= 2, add=TRUE)
  abline(h=mean(df_bestand_100[,2]), v=mean(df_bestand[,1]), lwd=lwd, col = "darkgreen")
  points(df_bestand$x, df_bestand_100$y, pch="x", col= "darkolivegreen", cex = 0.5)
  
  legend("bottomleft",title= "Bestand Klima (aktuell) ", 
         legend = c("mittelwert", "einzelwert", "klimahülle"),
         pch=c("+", "X", "-"), 
         col = c("darkgreen", "darkolivegreen", "darkgreen"), cex = 0.75, bty = "n")
  
  #calculate risk
  report <- climateSpaceComparison(df,  df_bestand, tolerance=tolerance)
  legend("bottomright", title = paste("Risikowert: ",
                                      report$report$overall[1,3], " (",
                                      report$report$overall[1,2], ")",
                                      "\n", "Werteverteilung:" , sep=""),
         legend = paste(round(as.numeric(report$report$percentage$riskPercentage)*100, digits=1), "% ",  "- ",
                        report$report$percentage$riskLegend, sep =""),
         col = c(my.cols, "white"),  pch = pch, cex = 0.75, bty = "n")
  
  
}