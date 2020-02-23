unletter <- function(word) {
  paste(sprintf("%02d",utf8ToInt(tolower(word))),collapse='')
}
# output <- vector()
# for(i in 1:length(v)){
#   word <- v[i]
#   output <- c(output, paste(sprintf("%02d",utf8ToInt(tolower(word))),collapse=''))
# }
# return (output)