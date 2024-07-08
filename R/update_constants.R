#' update the constants used when calculating the cosine quantogram (and some other functions)
#'
#' @param rngStart where the quantum search starts
#' @param rngEnd where the search ends
#' @param stepSize the increment size of the calculation
#' @param Qmin I have no clue what this is
#'
update_constants = function(rngStart, rngEnd, stepSize, Qmin){
  CONSTANTS_QUANTOGRAM=list(
    RNG_START = rngStart,   # minimal threshold for search
    RNG_END = rngEnd,       # maximal threshold for search
    STEP = stepSize,        # single step size
    Q_MIN = Qmin)
}