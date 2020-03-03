library(bnstruct)
fillMissingDataByKnn <- function(myData){
  textDataShort <- read.csv("./eurosat-w2020/codebook_compact.csv", header = T, stringsAsFactors = F)
  for(i in 1:nrow(textDataShort)){
    if(textDataShort[i, "Mean"] != "."){
      myData[i+1] <- as.numeric(unlist(myData[i+1]))
    }else{
      myData[i+1] <- as.factor(unlist(myData[i+1]))
    }
  }
  myData <- subset(myData, select = -id)
  myData <- knn.impute(as.matrix(myData), k = 13)
  return (myData)
}