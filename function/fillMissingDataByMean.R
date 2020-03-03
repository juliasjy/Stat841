source("./function/unletter.R")
source("./function/normalize.R")
fillMissingDataByMean <- function(myData){
  textDataShort <- read.csv("./eurosat-w2020/codebook_compact.csv", header = T, stringsAsFactors = F)
  myData[myData == ".a"] <- NA
  myData[myData == ".b"] <- NA
  myData[myData == ".c"] <- NA
  myData[myData == ""] <- NA
  
  nonNumeric <- vector()
  for(i in 1:nrow(textDataShort)){
    if(textDataShort[i, "Mean"] != "."){
      myData[i+1] <- as.numeric(unlist(myData[i+1]))
      myData[is.na(myData[i+1]), i+1] <- textDataShort$Mean[i]
      myData[i+1] <- as.numeric(unlist(myData[i+1]))
    }else{
      myData[i+1] <- as.numeric(unlist(lapply(as.vector(unlist(myData[i+1])), unletter)))
      myData[i+1] <- normalize(myData[i+1])
      nonNumeric <- c(nonNumeric, i+1)
    }
  }
  # myData <- subset(myData, select = -nonNumeric)
  return (myData)
}