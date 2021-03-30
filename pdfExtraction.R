# lib
require(pdftools)
options(encoding="utf-8")

###### FVA Steckbrief ###################################################
pdfObject <- pdf_text("D:/Downloads/180201steckbrief.pdf")

str(pdfObject, 1)

head(pdfObject)


# index for species
index <- read.csv("E:/EDE_Daten/Baumarten - FVA_Steckbrief_Index.tsv", header = F,
                  sep = "\t", encoding="utf-8")
  # one page delyaed
index[, 2] <- index[, 2]+1
baumarten <- index[1:(nrow(index)-1), 1]

# What information to collect
  # read in variables
variablen <- read.csv("E:/EDE_Daten/Baumarten - FVA_Variablen.tsv", header = F,
                  sep = "\t", encoding="utf-8")[,1]
columns <- c("baumart", variablen)

# create a dataframe to hold the date
steckbrief <- as.data.frame(matrix(rep(NA, length(columns)*length(baumarten)), ncol = length(columns)))
colnames(steckbrief) <- columns

for ( i in 1:length(index[,1])){
  cat("\n\n");cat("Steckbrief von"); cat(index[i, 1]); cat("\n")
  
  steckbrief[i, "baumart"]<- index[i, 1]
  
  if (steckbrief[i, "baumart"] != "END"){
    subsetPDF <- pdfObject[index[i, 2]:(index[i+1, 2]-1)]
    subsetPDF <- unlist(strsplit(subsetPDF, split="\r\n"))
    
    for (j in 2:length(columns)){
      # there is a line break sometimes, better modify the pattern
      if (columns[j]=="Erfahrung in Baden-Württemberg und Deutschland" 
          || columns[j]=="Zusätzliche Information"){
        begin <- grep(gsub("und Deutschland","", columns[j]), subsetPDF)
      }else if (columns[j]=="Literatur"){
      # The word is in many places. So better mark the begin ^ and the exact match
        begin <- grep(paste("^", columns[j], "\\b", sep= ""), subsetPDF)
        
      }else{
      # Include : to find the right word
        begin <- grep(paste(columns[j], ":", sep= ""), subsetPDF)  
         }
     
      
      if (length(begin)!=0){
        cat("\n\n");cat("Variable: "); cat(columns[j]); cat("\n")
        
        if(j==length(columns)){
          end <- length(subsetPDF)
        }else{
          if (columns[j+1]=="Erfahrung in Baden-Württemberg und Deutschland" 
              || columns[j+1]=="Zusätzliche Information"){
            end <- grep(gsub("und Deutschland","", columns[j+1]), subsetPDF)-1
          }else if (columns[j+1]=="Literatur"){
            end <- grep(paste("^", columns[j+1], "\\b", sep= ""), subsetPDF)-1
            
          }else{
            end <- grep(paste(columns[j+1], ":", sep= ""), subsetPDF)-1
          }
  
        }
  
        
        if(length(end)==0){
          cat("Couldnt find an end. Asumming is only one line more"); cat("\n")
          end <- begin+1
          }
        
        
        dummy <- subsetPDF[c(begin:end)]
        if (columns[j]=="Erfahrung in Baden-Württemberg und Deutschland" 
            || columns[j]=="Zusätzliche Information"
            || columns[j]=="Literatur"){
          # Ensure exact match with \\> for literature
          info <- gsub(paste(".*", columns[j], "\\>", sep = ""),"", paste( dummy, collapse = " "))
        }else{
          info <- gsub(paste(".*", columns[j],  ":",sep = ""),"", paste( dummy, collapse = " "))
          
        }
        
        
        steckbrief[i, columns[j]] <- info
        
      }else{
        cat("Variable not present!"); cat("\n")
      }
    }
  }else{
    cat("\n\n"); cat("ENDE");cat("\n")
  }
}

write.table(steckbrief, "E:/EDE_Daten/FVA_steckbrief_table.txt",sep = "\t",
            col.names = T, row.names = F)
saveRDS(steckbrief, file = "E:/EDE_Daten/FVA_steckbrief.rds")



###### LWF Steckbrief ###################################################

LWF_band <- c("D:/Downloads/praxishilfe_baumarten_lwf_band1.pdf",
                   "D:/Downloads/praxishilfe_baumarten_lwf_band2.pdf")
                   
LWF_tabelle <- vector(mode = "list", length = 2)
names(LWF_tabelle) <- paste("band_", c(1:2), sep = "")

m = 1
pdfObject <- pdf_text(LWF_band[m])

str(pdfObject, 1)

head(pdfObject)

# What information to collect
# read in variables
variablen <- read.csv("E:/EDE_Daten/Baumarten - LWF_Variablen_Steckbrief.tsv", header = T,
                      sep = "\t", encoding="utf-8")
hauptVariablen <- unique(variablen$Variable)
unterVariablen <- variablen$Subvariable
unterVariablen <- gsub("^na$", NA, unterVariablen)
unterVariablen <- unterVariablen[!is.na(unterVariablen)]

columns <- c("baumart", hauptVariablen, unterVariablen)

names(unterVariablen) <- c(rep("Holzverwendung", 3),rep("Waldbau", 3) )

# index for species
index <- read.csv("E:/EDE_Daten/Baumarten - LWF_Index.tsv", header = F,
                  sep = "\t", encoding="utf-8")
# index must be corrected two pages
index <- index[index[,3]=="band_1",]
index[, 2] <- index[, 2]-2


index <- index[index[,3]=="band_2",]

baumarten <- index[1:(nrow(index)-1), 1]


# create a dataframe to hold the date
steckbrief <- as.data.frame(matrix(rep(NA, length(columns)*length(baumarten)), ncol = length(columns)))
colnames(steckbrief) <- columns

doubleCol <- c("Holzverwendung", "Waldschutz", "Artenvielfalt")

for ( i in 1:length(index[,1])){
  cat("\n\n");cat("Steckbrief von "); cat(index[i, 1]); cat("\n")
  
  steckbrief[i, "baumart"]<- index[i, 1]
  
  if (steckbrief[i, "baumart"] != "END"){
  
    subsetPDF <- pdfObject[index[i, 2]:(index[i+1, 2]-1)]
    subsetPDF <- unlist(strsplit(subsetPDF, split="\r\n"))
      # remove all spaces > 1  gsub(" +"," ", subsetPDF)
      
      lastbegin <- 1
      for (j in 1:length(hauptVariablen)){
        
        begin <- grep(paste("\\b",hauptVariablen[j], "$", sep= ""), subsetPDF)
        if (hauptVariablen[j] == "Wasser und Boden" ){
          begin <- grep(paste("\\b",hauptVariablen[j], "", sep= ""), subsetPDF)
        }
        if (length(begin)==0){
          begin <-grep(paste("\\b",hauptVariablen[j], "\\>", sep= ""), subsetPDF)
        }
        
        if (j == length(hauptVariablen)){
          end <- length(subsetPDF)
        }else{
          end <- grep(paste("\\b",hauptVariablen[j+1], "$", sep= ""), subsetPDF)-1
        }
        if (length(end)==0){
          end <-grep(paste("\\b",hauptVariablen[j+1], "\\>", sep= ""), subsetPDF)-1
        }
        
        
        if (columns[j] == "Wasser und Boden" ){
          end <- grep(paste("\\b",hauptVariablen[j+1], "", sep= ""), subsetPDF)-1
          
          veryend <- grep(paste("\\b",hauptVariablen[j+2], "$", sep= ""), subsetPDF)-1
          
          end <- end[end<veryend]
        }
   
        
        if (length(begin)!=0){
          cat("\n\n");cat("Variable: "); cat(columns[j]); cat("\n")
          if(max(begin, na.rm = T)> lastbegin){
            begin <- begin[begin>lastbegin]
          }else{
            begin <- lastbegin +2
          }
          
          if(length(end)==0){
            cat("Couldnt find an end. Asumming is only one line more"); cat("\n")
            end <- begin+1
          }
          end <- end[end>begin[1]]
          end <- max(end, na.rm = T)
          if (is.na(begin) | is.na(end)){
            cat("Nothing to be found. Already told so!");cat("\n")
          }else{
            lastbegin <- begin<- begin[1]
           
            
            dummy <- subsetPDF[c(begin:end)]
            dummy <- trimws(dummy, which =  "left", whitespace = "[ \t\r\n]")
            
            if (hauptVariablen[j] %in% doubleCol){
              # split the text in columns
              dummy <- gsub("*  +", "\t", dummy)
              dummy <- unlist(strsplit(dummy, "\t"))
              # if is hauptvariable
              dummy <- c(dummy[c(FALSE, TRUE)], dummy[c(TRUE, FALSE)])  
            }
            
            # Prepare the info 
            if(hauptVariablen[j] %in% names(unterVariablen)){
              subvar <- unterVariablen[names(unterVariablen)==hauptVariablen[j]]
              for (k in 1:length(subvar)){
                subBegin <- grep(paste(subvar[k], ":", sep= ""), dummy)
                if (k < length(subvar) ){
                  subEnd <- grep(paste(subvar[k+1], ":", sep= ""), dummy)-1
                }else{
                  subEnd <- length(dummy)
                }
                subDummy <- dummy[subBegin:subEnd]
                
                subInfo <- sub(paste(".*", subvar[k], ":", sep = ""),"", paste( subDummy, collapse = " "))
                steckbrief[i, subvar[k]] <- subInfo
                subInfo <- NA
              }
              
            }
            
            info <- sub(paste("\\b", hauptVariablen[j], "\\>", sep = ""),"", paste( dummy, collapse = " "))
           
            steckbrief[i, hauptVariablen[j]] <- info
          }
          info <- NA
          
        }else{
          cat("Variable not present!"); cat("\n")
        }

      }
      
  }else{
    cat("\n\n"); cat("ENDE");cat("\n")
  }
}
        
        
     

LWF_tabelle[[m]] <- steckbrief

steckbrief <-do.call("rbind", LWF_tabelle)


write.table(steckbrief, "E:/EDE_Daten/LWF_steckbrief_table.txt",sep = "\t",
            col.names = T, row.names = F)
saveRDS(steckbrief, file = "E:/EDE_Daten/LWF_steckbrief.rds")


