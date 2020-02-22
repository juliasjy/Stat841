convertDataType <- function(myData){
  myData <- fillMissingDataByMean(getData("train.csv"))
  textDataShort <- read.csv("./eurosat-w2020/codebook_compact.csv", header = T, stringsAsFactors = F)
  for(i in 1:nrow(textDataShort)){
    if(textDataShort[i, "Mean"] != "."){
      myData[i+1] <- as.numeric(unlist(myData[i+1]))
    }
  }
  return (myData)
}