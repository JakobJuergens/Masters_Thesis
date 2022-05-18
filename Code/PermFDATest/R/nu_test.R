nu_realizations <- function(sample1, sample2, full = FALSE, approxQ, approx_grid){
  if(full == TRUE){
    return(nu_real_full(sample1, sample2, approx_grid))
  } else{
    return(nu_real_approx(sample1, sample2, approxQ, approx_grid))
  }

}

nu_real_full <- function(sample1, sample2, approx_grid){

}

nu_real_approx <- function(){

}
