normalize <- function(x) {
  return (10 * (x - min(x)) / (max(x) - min(x)))
}