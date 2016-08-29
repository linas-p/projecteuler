fib <- function(N){
  if(N < 1){
    stop("N must be positive")
  } else if (N == 1) {
    return(1)
  } else if (N == 2){
    return(2)
  } else {
    return(fib(N-1) + fib(N-2))
  }
}

get_n <- function(condition){
  max_val <- 0
  N <- 1
  while(max_val < condition){
    N <- N + 1
    max_val <- fib(N)
  }
  return(N-1)
}


fib_mod <- function(N){
  
  if(N < 1){
    stop("N must be positive")
  } else if (N == 1) {
    return(c(1,1))
  } else if (N == 2){
    return(c(2,2))
  } else {
    seq1 <- fib(N-1)
    seq2 <- fib(N-2)
    return(c(seq1[1] + seq2[1], unique(c(seq1, seq2))))
  }
}

get_result <- function(condition){
  N <- get_n(condition)
  seq <- fib_mod(N)
  result <- sum(seq[(seq %% 2) == 0])
  return(result)
}

get_result(4000000) #4613732
