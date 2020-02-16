getData <- function(name){
  data <- read.csv(paste0("./eurosat-w2020/", name))
  return (data)
}

train_data <- getData("train.csv")
test_data <- getData("test.csv")

