getData <- function(name){
  data <- data.frame(read.csv(paste0("./eurosat-w2020/", name), stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  return (data)
}