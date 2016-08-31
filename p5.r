divisible <- function(n, m = n){
  result <- m
  found <- FALSE
  while(!found){
    all_good <- TRUE
    for(k in n:1){
      if((result %% k) != 0){
        all_good <- FALSE
        break;
      }
    }
    if(all_good){
      return(result)
    }else{
      result <- result + 1
    }
  }
}

divisible(20)
