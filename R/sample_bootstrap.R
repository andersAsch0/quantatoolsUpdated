#' Get bootstrap sample
#'
#' @param x vector of data to sample
#' @param decrease_by size of sample = length of x - decrease_by
#'
#' @return the vector of samples 
#' @export
#'
#' @examples SW1 = c(10.8, 11.2, 12.7, 16.4, 16.9, 20.5) SWsample = sample_bootstrap(SW1, decrease_by = 2)
sample_bootstrap = function(x, decrease_by=0) {
  sample(size = length(x) - decrease_by,
         x = x,
         replace = T)
}
