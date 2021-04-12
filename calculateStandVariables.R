#calculate_basal_area <- function(tree) {
#ba <- as.data.frame(aggregate(tree$dbh1_cm, by=list(year=tree$year, species=tree$species), function(x) sum(0.25 *pi * x^2)))
#  names(ba)[names(ba) =="x"] <- "basal_area_cm2"
#  if (length(unique(tree$size_m2)) == 1) ba$basal_area_m2_per_ha <- (ba$basal_area_cm2/(100*100))/(unique(tree$size_m2)/(100*100))
#  ba
#}

# DBH
# arithmetischer Mitteldurchmesser ` d
calcDBH.meanArit100 <- function(dbh){
  if(length(dbh[!is.na(dbh)])==0){
    dbh_cm <- NA
  }else{
    dbh_cm <- mean(dbh, na.rm = T)
  }
  return(dbh_cm)
}

# Durchmesser des Grundflächenmittelstammes dg
calcDBH.meanSquared <- function(dbh){
  if(length(dbh[!is.na(dbh)])==0){
    dq_cm <- NA
  }else{
    dq_cm <- sqrt(sum(dbh**2, na.rm = T)/length(dbh[!is.na(dbh)]))
  }
  return(dq_cm)
}

# Oberdurchmesser ddom 
calcDBH.meanArit100 <- function(dbh){
  if(length(dbh[!is.na(dbh)])==0){
    dq_cm <- NA
  }else{
    dbh <- dbh[order(dbh, decreasing = T)]
    if (length(dbh)>= 100){
      dbh <- dbh[1:100]
    }
    dbh_cm <- mean(dbh, na.rm = T)
  }
  return(dbh_cm)
}


calcDBH.meanSquared100 <- function(dbh){
  if(length(dbh[!is.na(dbh)])==0){
    dq_cm <- NA
  }else{
    dbh <- dbh[order(dbh, decreasing = T)]
    if (length(dbh)>= 100){
      dbh <- dbh[1:100]
    }
    dq_cm <- sqrt(sum(dbh**2, na.rm = T)/length(dbh[!is.na(dbh)]))
  }
  return(dq_cm)
}


# mean squared dbh--> Lorey's mean dbh weights

calcDBH.LoreysMean <- function(dbh){
  ba_cm2 <- sapply(dbh, function(x){pi * (x/2)**2})
  dbh_cm <- sum((ba_cm2 * dbh), na.rm = T)/sum(ba_cm2, na.rm = T)
  #height_m <- sqrt(sum(height**2, na.rm = T)/length(height[!is.na(height)]))
  return(dbh_cm)
}

calcDBH.LoreysMean100  <- function(dbh){
  dbh <- dbh[order(dbh, decreasing = T)]
  if (length(dbh)>= 100){
    dbh <- dbh[1:100]
  }
  ba_cm2 <- sapply(dbh, function(x){pi * (x/2)**2})
  dbh_cm <- sum((ba_cm2 * dbh), na.rm = T)/sum(ba_cm2, na.rm = T)
  #height_m <- sqrt(sum(height**2, na.rm = T)/length(height[!is.na(height)]))
  return(dbh_cm)
}



### Height ######

# arithmetische Mittelhöhe 
calcHeight.meanArit  <- function(height){
  height <- height[!is.na(height)]
  height <- height /length(height)
  return(height)
} 

# Oberhöhe hdom
calcHeight.meanArit100 <- function(dbh, height){
  if(length(dbh[!is.na(dbh)])==0 | length(height[!is.na(height)])==0 ){
    warning("Height cant be calculated!")
    warning("Not enough data!")
    height <- NA
  }else{
    dbh <- dbh[order(dbh, decreasing = T)]
    height <- height[order(dbh, decreasing = T)]
    if (length(height)>= 100){
      height <- height[1:100]
    }
    height <- mean(height, na.rm = T)
  }
  return(height)
}

# Logarithmic y = 10.745ln(x) - 13.095Collelongo Works!!
calcHeiht.log <- function(dbh, a =1, b=1){
  height <- a + b*log(dbh)
  return(height)
}
# Petterson KRoof
calcHeight.peterson <- function(dbh, a =1, b=1){
  height <- 1.3 + (dbh/(a + b*dbh))**3
  return(height)
}
# Michailoff  Peitz
calcHeight.michailoff  <- function(dbh, a =1, b=1){
  height <- 1.3 + a*exp(b/dbh)
  return(height)
}

# Oliveira cited from Gadow &   Bredenkamp
calcHeight.oliveira  <- function(dbh, a =1, b=1){
  height <- exp(a + b/dbh)
  return(height)
}

# BERTALANFFY: Need to be checked --> a = horizontale Asymptote (maximale Höhe)
calcHeight.bertalanffy  <- function(a =1, b=1, c=1, t=1, m= 1){
  height <- a * (1 - b*exp(c*t))^(1/(1-m))
  return(height)
}
# SCHUMACHER: Need to be checked --> (horizontale Tangente im Ursprung, 
# Asymptote bei h = a, Wendepunkt bei t = b/2
calcHeight.schumacher  <- function(a =1, b=1, t=1){
  height <- a * exp(-b/t)
  return(height)
}
# LUNDQUIST: Need to be checked --> (horizontale Tangente im Ursprung,
# Asymptote bei h = a, Wendepunkt bei t = (b .c)1/c / (c+1) )
calcHeight.lundquist  <- function(a =1, b=1, t=1, c= 1){
  height <- a * exp((-b/t) * c)
  return(height)
}
# DECOURT:  Need to be checked --> (horizontale Asymptote bei h = 1/a).
calcHeight.decourt  <- function(a =1, b=1, t=1, c= 1){
  height <- (t^2 /a) * t^2 + b*t + c
  return(height)
}




# mean squared height--> Lorey's mean height weights
calcHeight.LoreysMean100  <- function(height, dbh){
  if(length(height[!is.na(height)])==0){
    height_m <- NA
  }else{
    dbh <- dbh[order(dbh, decreasing = T)]
    height <- height[order(dbh, decreasing = T)]
    if (length(dbh)>= 100){
      dbh <- dbh[1:100]
      height <- height[1:100]
    }
    
    ba_m2 <- sapply(dbh/100, function(x){pi * (x/2)**2})
    ba_m2 <- ba_m2[!is.na(height)]
    height <- height[!is.na(height)]
    height_m <- sum((ba_m2 * height), na.rm = T)/sum(ba_m2, na.rm = T)
    #height_m <- sqrt(sum(height**2, na.rm = T)/length(height[!is.na(height)]))
  }
  return(height_m)
}
# Loreyhöhe hL
calcHeight.LoreysMean <- function(height, dbh){
  if(length(height[!is.na(height)])==0){
    height_m <- NA
  }else{
    ba_m2 <- sapply(dbh/100, function(x){pi * (x/2)**2})
    ba_m2 <- ba_m2[!is.na(height)]
    height <- height[!is.na(height)]
    height_m <- sum((ba_m2 * height), na.rm = T)/sum(ba_m2, na.rm = T)
    #height_m <- sqrt(sum(height**2, na.rm = T)/length(height[!is.na(height)]))
  }
  return(height_m)
}




# Höhe des Grundflächenmittelstammes hg?
#The height of the average tree may be measured.
calcHeight.meanSquared <- function(height, dbh){
  if(length(height[!is.na(height)])==0){
    height_m <- NA
  }else{
    meanDBH <- meanSquaredDiameter(dbh)
    dbh <- dbh[!is.na(height)]
    height <- height[!is.na(height)]
    height_m <- height[which.min(abs(dbh - meanDBH))]
  }
  
  return(height_m)
}

calcHeight.meanSquared100 <- function(height, dbh){
  if(length(height[!is.na(height)])==0){
    height_m <- NA
  }else{
    dbh <- dbh[order(dbh, decreasing = T)]
    height <- height[order(dbh, decreasing = T)]
    if (length(dbh)>= 100){
      dbh <- dbh[1:100]
      height <- height[1:100]
    }
    if(length(height[!is.na(height)])==0){
      height_m <- NA
    }else{
      meanDBH <- meanSquaredDiameter(dbh)
      dbh <- dbh[!is.na(height)]
      height <- height[!is.na(height)]
      height_m <- height[which.min(abs(dbh - meanDBH))]
    }
  }
  return(height_m)
}


# Gleichförmige BestÄnde

# h = a . db
# h = a (1 - e(-b . d) )
# h = a + b . ln d
# h = 1,3 + d2 / (a + bd)2 #(Näselund)
# h = 1,3 + a( d / (1 + d) )2
# h = 1,3 + a e(-b / d)
# h = 1,3 + (a + bd + cd2)

# Plenterbestand 
# h - 1,3 = d2 / (a + bd + cd2)





### G Grundfläche ###


# basal area of stand
standBA <- function(dbh, plotSize){
  if(length(dbh[!is.na(dbh)])==0){
    ba_m2ha <- NA
  }else{
    ba_m2 <- sapply(dbh/100, function(x){pi * (x/2)**2})
    A_ha <-  unique(plotSize)* (1/10000)
    ba_m2ha <- sum(ba_m2, na.rm = T) / A_ha
  }
  return(ba_m2ha)
}
#### H/D  ####
calcHD <- function(dbh, height){
  if(length(dbh[!is.na(dbh)])==0 | length(height[!is.na(height)])==0){
    warning("HD cant be calculated")
    hd <- NA
  }else{
    hd <- height/dbh
  }  
  return(hd)
}


### Volum ###

# Bestandesformzahl F = V / (G * H) ; F * (G*H) V= Vorrat
  # F from table or hg/dg or hdom / ddom
calcVol <- function(dbh, height, ba, f = NULL){
  if(length(dbh[!is.na(dbh)])==0 | length(height[!is.na(height)])==0 | length(ba[!is.na(ba)])==0){
    warning("Volum cant be calculated")
    volum <- NA
  }else{
    if(is.null(f))f <- height/dbh
    
    volum <- f * ba * height
  }  
  return(volum)
}


# zuwachs # Volumenzuwachs 
calcGrowth.abs <- function(volum){
  if(length(volum[!is.na(volum)])==0){
    warning("Growth cant be calculated")
    growth <- NA
  }else{
    growth <- c(0, volum[2:length(volum)] - volum[1:(length(volum)-1)])
  }  
  return(growth)
}

calcGrowth.rel <- function(volum){
  if(length(volum[!is.na(volum)])==0){
    warning("Growth cant be calculated")
    growth <- NA
  }else{
    growth <- c(0, volum[2:length(volum)] - volum[1:(length(volum)-1)])
    growth <- growth / c(0,volum[1:(length(volum)-1)]) * 100
  }  
  return(growth)
}




# standortsgüte (siehe https://www.wsl.ch/forest/waldman/vorlesung/ww_tk524.ehtml)
# Y = b0 + b1 X1 + b2 X2 .... bn Xn
# Y  = Zielgrösse (abhängige Variable)
# Xi = Standortsfaktoren (unabhängige Variablen)
# bi = zu bestimmende Regressionskoeffizienten (Konstanten)




