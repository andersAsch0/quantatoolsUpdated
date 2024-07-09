#' Unrounding method
#'
#' @param input a vector of measurements
#' @param depth the accuracy, analogous to smallest significant digit
#'
#' @return the vector input, unrounded (random number b/w -1/2 depth and 1/2 depth)
#' @export
#'
#' @examples data = c(1,2,3) unroundedData = unround_data(data)
unround_data = function(input, depth=0.01) {
  unroundedVector = input
  i = 1
  while(i<length(dataVector)+1){
    unroundedVector[i] = input[i] +  runif(1, -depth/2, depth/2)
    i = i+1
  }
  return(unroundedVector)
}
