f <- function(N){
  x <- 1:N
  set1 <- which((x %% 3) == 0)
  set2 <- which((x %% 5) == 0)
  finalset <- unique(c(set1, set2))
  result <- sum(finalset)
  return(result)
}

f(9) #23

f(999) #233168