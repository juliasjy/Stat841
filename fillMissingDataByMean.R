# # Section One ---------------------------------
# 
# library(tidyr)
# 
# textData <- read.delim("./eurosat-w2020/codebook_long.txt", header = F, stringsAsFactors = F)
# 
# #re_matches(data = textData, pattern = rex(capture("type:")))
# 
# type <- textData[grep("type:", textData$V1),]
# 
# 
# textDataShort <- read.delim("./eurosat-w2020/codebook_compact.txt", header = F, stringsAsFactors = F)
# textDataShort <- textDataShort[10:280,]
# colnames(textDataShort) <- c("temp")
# 
# textDataShortDF <- data.frame(temp = textDataShort)
# DF <- separate(data = textDataShortDF$temp, col = c("Var", "Obs", "Unique", "Mean", "Min", "Max", "Label"))
# 
# DF <- textDataShortDF %>% separate(temp, c("Var", "Obs", "Unique", "Mean", "Min", "Max", "Label"), sep=" ")
# 
# textDataShortVector <- as.vector(textDataShort)

# Section Two =================================
# myData <- data.frame(read.csv(paste0("./eurosat-w2020/", "train.csv"), stringsAsFactors=FALSE), stringsAsFactors = FALSE)

fillMissingDataByMean <- function(myData){
  textDataShort <- read.csv("./eurosat-w2020/codebook_compact.csv", header = T, stringsAsFactors = F)
  myData[myData == ".a"] <- NA
  myData[myData == ".b"] <- NA
  myData[myData == ".c"] <- NA
  myData[myData == ""] <- NA
  
  noMissing <- vector()
  for(i in 1:nrow(textDataShort)){
      myData[is.na(myData[i+1]), i+1] <- textDataShort$Mean[i]
  }
  return (myData)
}