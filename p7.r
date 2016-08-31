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


find_nth_prime <- function(n){
  k <- 2;
  counter <- 0
  primes <- c()
  while(counter != n){
    if(prime(k, primes)){
      counter = counter + 1
      primes <- c(primes, k)
      if((counter %% 100) == 0){
        print(paste("found", counter))
      }
    }
    k <- k + 1
  }
  return(primes)
}

all_n_primes <- find_nth_prime(10001);
max(all_n_primes)#104743
