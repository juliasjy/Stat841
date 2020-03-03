setUpMissingData <- function(myData){
  textDataShort <- read.csv("./eurosat-w2020/codebook_compact.csv", header = T, stringsAsFactors = F)
  myData[myData == ".a"] <- NA
  myData[myData == ".b"] <- NA
  myData[myData == ".c"] <- NA
  myData[myData == ""] <- NA
  return (myData)
}