prime <- function(n){
  if(n < 3){
    return(TRUE)
  }
  result <- TRUE;
  for(k in 2:(n-1)){
    if ((n %% k) == 0){
      result <- FALSE;
      return(result)
    }
  }
  return(TRUE);
}


find_div <- function(n){
    result <- c()
    tmp <- n;
    k <- 2;
    while(k != n){
      if(k > tmp){ # It's a tricky one :)
        break
      } else if ((tmp %% k) == 0){
        result <- c(result, k)
        tmp <- tmp / k
      }
      k = k + 1
    }
    return(result)
}
  
find_div(600851475143) # 71  839 1471 6857
prime(max(find_div(600851475143)))
