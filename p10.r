prime <- function(n, prime_list = NULL){
  if(n < 3){
    return(TRUE)
  }
  index <- c()
  if(is.null(prime_list)){
    index <- 2:(n-1)
  }else{
    index <- prime_list
  }
  result <- TRUE;
  for(k in index){
    if ((n %% k) == 0){
      result <- FALSE;
      return(result)
    }
  }
  return(TRUE);
}


find_prime_up_to_n <- function(n){
  k <- 2;
  primes <- c()
  while(k < (n - 1)){
    if(prime(k, primes)){
      primes <- c(primes, k)
    }
    if((k %% 10000) == 0){
      print(paste("found", k))
    }
    k <- k + 1
  }
  return(primes)
}

res <- find_prime_up_to_n(2000000)
sum(res)