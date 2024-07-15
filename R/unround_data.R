#' Unrounding method
#'
#' @param input numeric vector of measurement values
#' @param depth the accuracy, analogous to smallest significant digit
#'
#' @return the unrounded numeric vector (random number b/w -1/2 depth and 1/2 depth)
#' @export
#'
unround_data = function(input, depth=0.01) {
  unroundedVector = input
  i = 1
  while(i<length(input)+1){
    unroundedVector[i] = input[i] +  runif(1, -depth/2, depth/2)
    i = i+1
  }
  return(unroundedVector)
}
