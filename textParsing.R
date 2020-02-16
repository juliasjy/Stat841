library(tidyr)

textData <- read.delim("./eurosat-w2020/codebook_long.txt", header = F, stringsAsFactors = F)

#re_matches(data = textData, pattern = rex(capture("type:")))

type <- textData[grep("type:", textData$V1),]


textDataShort <- read.delim("./eurosat-w2020/codebook_compact.txt", header = F, stringsAsFactors = F)
textDataShort <- textDataShort[10:280,]
colnames(textDataShort) <- c("temp")

textDataShortDF <- data.frame(temp = textDataShort)
DF <- separate(data = textDataShortDF$temp, col = c("Var", "Obs", "Unique", "Mean", "Min", "Max", "Label"))

DF <- textDataShortDF %>% separate(temp, c("Var", "Obs", "Unique", "Mean", "Min", "Max", "Label"), sep=" ")

textDataShortVector <- as.vector(textDataShort)

###########################################################
textDataShort <- read.csv("./eurosat-w2020/codebook_compact.csv", header = T, stringsAsFactors = F)

