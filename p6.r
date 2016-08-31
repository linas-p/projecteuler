sq_diff <- function(n){
  sq1 <- sum((1:n)^2)
  sq2 <- sum(1:n)^2
  return(sq2 -sq1)
}