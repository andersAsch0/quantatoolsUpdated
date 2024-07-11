#' update the constants used when calculating the cosine quantogram (and some other functions)
#'
#' @param rngStart where the quantum search starts
#' @param rngEnd where the search ends
#' @param stepSize the increment size of the calculation
#' @param Qmin I have no clue what this is
#'
#' @export
update_constants = function(rngStart, rngEnd, stepSize, Qmin){
  newConstants = list(rngStart, rngEnd, stepSize, Qmin)
  names(newConstants) = c("RNG_START", "RNG_END", "STEP", "Q_MIN")
  options(CONSTANTS_QUANTOGRAM = newConstants)
  getOption("CONSTANTS_QUANTOGRAM")
}
