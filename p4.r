numbs <- function(n){
  if((n %/% 10) != 0){
    return(c(n %% 10, numbs(n %/% 10)))
  } else {
    return(n %% 10)
  }
}

palindrom <- function(n){
  nums <- numbs(n);
  result <- TRUE
  for(k in 1:length(nums)){
    if(nums[k] != nums[length(nums)-k+1]){
      result <- FALSE;
      return(result)
    }
  }
  return(result)
}

which_paindrom <- function(){
  result <- c()
  for(k in 100:999){
    for(j in k:999){
      if(palindrom(k*j)){
        result <- rbind(result, c(k, j))
      }
    }
  }
  index <- which(p[,1]* p[,2] == max(p[,1]* p[,2]));
  return(result[index,])
}

palindrom(913*993)