#' update the constants used when calculating the cosine quantogram (and some other functions)
#'
#' @param rngStart where the quantum search starts
#' @param rngEnd where the search ends
#' @param stepSize the increment size of the calculation
#' @param Qmin smallest allowed quanta (calculations will skip it even its included in the RNG range)
#'
#' @export
update_constants_quantogram = function(rngStart, rngEnd, stepSize, Qmin){
  newConstants = list(rngStart, rngEnd, stepSize, Qmin)
  names(newConstants) = c("RNG_START", "RNG_END", "STEP", "Q_MIN")
  options(CONSTANTS_QUANTOGRAM = newConstants)
  getOption("CONSTANTS_QUANTOGRAM")
}

#' update the constants used when calculating the SLS 
#'
#' @param slsrngStart where the sls search starts
#' @param slsrngEnd where the sls search ends
#' @param stepSize the increment size of the calculation 
#' @param slsMaxNumQuanta how many quanta should be looked for max
#'
#' @export
update_constants_sls = function(slsrngStart, slsrngEnd, stepSize, slsMaxNumQuanta){
  newConstants = list(slsrngStart, slsrngEnd, stepSize, slsMaxNumQuanta)
  names(newConstants) = c("SLS_RNG_START", "SLS_RNG_END", "SLS_STEP", "SLS_MAX_N_QUANTA")
  options(CONSTANTS_SLS = newConstants)
  getOption("CONSTANTS_SLS")
}
