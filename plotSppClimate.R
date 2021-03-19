# scatter plot with density http://www.logarithmic.net/pfh-files/blog/01509162940/scatter.html
require (tidyverse)
require(viridis)
require(RANN)


plotClimateSpace <- function(x, y,k=min(25,length(x)),n=4, alpha = 0.5, pch = 20,
                             cex =0.4, ...){
  df <- data.frame(x=x,y=y)
  df <- na.omit(df)
  
  # Estimate density by k-nearest neighbours
  # (a kernel density estimate might be used instead)
  result <- nn2(scale(df), k=k)
  df$kdist <- result$nn.dists[,k] # why taking last column
  
  # Divide points into n groups by density
  df$group <- 
    ceiling(rank(-df$kdist, ties.method="random") *(n/nrow(df))) %>% 
    factor(levels=seq_len(n))
  unique(df$group)
  
  # Assign colors
  my.cols <- brewer.pal(n, "RdYlBu")
  df$color <- sapply(df$group, function(x){ my.cols[x]})
  
  # plot 
  plot(df$x, df$y, pch=pch , cex=cex, col = alpha(df$color, alpha), ...)
  legend("topright",title= paste("Fraction of" , length(df[,1]), "points", sep = " "),
         legend = rep(" 25 %", 4), pch = pch, col = my.cols, cex = 0.75, bty = "n")
  
}


plotClimateSpace.Bestand <- function(x, y, x_bestand, y_bestand,k=min(25,length(x)),n=4, alpha = 0.5, pch = 20,
                                     cex =0.4, ...){
 
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
    ceiling(rank(-df$kdist, ties.method="random") *(n/nrow(df))) %>% 
    factor(levels=seq_len(n))
  unique(df$group)
  
  # Assign colors
  my.cols <- brewer.pal(n, "RdYlBu")
  df$color <- sapply(df$group, function(x){ my.cols[x]})
  
  # plot 
  plot(df$x, df$y, pch=pch , cex=cex, col = alpha(df$color, alpha),
       ylim = yaxis, xlim= xaxis,...)
  legend("topright",title= paste("Fraction of" , length(df[,1]), "points", sep = " "),
         legend = rep(" 25 %", 4), pch = pch, col = my.cols, cex = 0.75, bty = "n")
  
  # Add bestand as overlay
  df_bestand <- data.frame(x=x_bestand,y=y_bestand)
  df_bestand <- na.omit(df_bestand)
  
  bestand.cols <- rev(brewer.pal(n, "Greens"))
  z <- kde2d(df_bestand[,1], df_bestand[,2], n=50)
  contour(z, drawlabels=FALSE, nlevels=n, col=bestand.cols, lwd= 2, add=TRUE)
  abline(h=mean(df_bestand[,2]), v=mean(df_bestand[,1]), lwd=1, col = "darkgreen")
  legend("bottomleft",title= "Bestand Klima (aktuell) ",
         legend = rep(" 25 %", 4), lty=2, col = bestand.cols, cex = 0.75, bty = "n")
  
}




