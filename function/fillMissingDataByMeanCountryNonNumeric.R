source("./function/unletter.R")
source("./function/normalize.R")
fillMissingDataByMeanCountryNonNumeric <- function(myData){
  myData <- subset(myData, select = -id)
  
  textDataShort <- read.csv("./eurosat-w2020/codebook_compact.csv", header = T, stringsAsFactors = F)
  myData[myData == ".a"] <- NA
  myData[myData == ".b"] <- NA
  myData[myData == ".c"] <- NA
  myData[myData == ""] <- NA
  
  delete <- vector()
  for(i in 1:nrow(textDataShort)){
    if(textDataShort[i, "Obs"] < "100"){
      delete <- c(delete, i)
    }else if(textDataShort[i, "Mean"] != "."){
      myData[i] <- as.numeric(unlist(myData[i]))
      myData[is.na(myData[i]), i] <- textDataShort$Mean[i]
      myData[i] <- as.numeric(unlist(myData[i]))
    }else{
      myData[i] <- as.factor(unlist(myData[i]))
    }
  }
  myData <- subset(myData, select = -delete)
  return (myData)
}