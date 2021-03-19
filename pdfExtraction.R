# lib
require(pdftools)


###### FVA Steckbrief ###################################################
pdfObject <- pdf_text("D:/Downloads/180201steckbrief.pdf")

str(pdfObject, 1)

head(pdfObject)
options(encoding="utf-8")

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
  
  if (baumart != "END"){
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
        iterator = 1
        
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




