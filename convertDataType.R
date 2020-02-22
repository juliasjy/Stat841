convertDataType <- function(myData){
  textDataShort <- read.csv("./eurosat-w2020/codebook_compact.csv", header = T, stringsAsFactors = F)
  idx <- c()
  for(i in 1:nrow(textDataShort)){
    if(textDataShort[i, "Mean"] != "."){
      myData[i+1] <- as.numeric(unlist(myData[i+1]))
    }else{
      idx <- c(idx, i+1)
    }
  }
  myData <- myData[,-idx]
  return (myData)
}