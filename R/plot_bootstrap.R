#' plot bootstrap produced by compute_bootstrap (if something else is passed in, it will just print() whatever it is)
#' 
#' @param bootstrap expecting a list from compute_bootstrap 
#' 
#' @return nothing
#' @export
#' 
plot_bootstrap <- function(bootstrap){
  if(is.list(bootstrap) && ("p" %in% names(bootstrap))){
    print(bootstrap$p)
  }
  else{
    print(bootstrap)
  }
}