find_triplet <- function(n){
  for(k in (n-2):(n/3)){
    for(j in (n-k):1){
      i = n - k - j
      if(k > j & j > i){
        if(i^2 + j^2 == k^2){
          print(paste("Triplet found:", k, j, i, sep = ","))
          return(c(i,j,k, i*j*k))
        }
      }
    }
  }
}