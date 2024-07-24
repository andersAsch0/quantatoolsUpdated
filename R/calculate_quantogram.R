#' Calculate Kendall's cosine quantogram
#'
#' @param x numeric vector of measurement values
#' @param params cosine quantogram parameters including "RNG_START", "RNG_END", "STEP", "Q_MIN"
#'
#' @return two columned dataframe of quanta (q) and their scores (f_q)
#' @export
#'
calculate_quantogram = function(x,
                                params=getOption("CONSTANTS_QUANTOGRAM")) {

  if (4 != sum(c("RNG_START", "RNG_END", "STEP", "Q_MIN") %in% names(params))) {
    print("Incorrect number of parameters")
    return(NULL)
  }

  n = x %>% length
  A = sqrt(2 / n)
  results = NULL

  rng <- seq(params$RNG_START, params$RNG_END, by = params$STEP)
  for (q in rng) {
    if (q < params$Q_MIN)
      next
    sum = 0
    for (i in 1:n) {
      e = x[i] %% q
      cosVal = 2.0 * pi * e / q
      sum = sum + cos(cosVal)
    }
    f_q = A * sum # quanta
    results = rbind(results, data.frame(q, f_q))
  }
  colnames(results) <- c('q', 'f_q')
  return(results)
}

#' the cosine quantogram but with sines, used to calculate Kendall's modular quantogram
#'
#' @param x numeric vector of measurement values
#' @param params cosine quantogram parameters including "RNG_START", "RNG_END", "STEP", "Q_MIN"
#'
#' @return two columned dataframe of quanta (q) and their scores (f_q)
#'
calculate_sine_quantogram = function(x,
                                params=getOption("CONSTANTS_QUANTOGRAM")) {
  
  if (4 != sum(c("RNG_START", "RNG_END", "STEP", "Q_MIN") %in% names(params))) {
    print("Incorrect number of parameters")
    return(NULL)
  }
  
  n = x %>% length
  A = sqrt(2 / n)
  results = NULL
  
  rng <- seq(params$RNG_START, params$RNG_END, by = params$STEP)
  for (q in rng) {
    if (q < params$Q_MIN)
      next
    sum = 0
    for (i in 1:n) {
      e = x[i] %% q
      sinVal = 2.0 * pi * e / q
      sum = sum + sin(sinVal)
    }
    f_q = A * sum # quanta
    results = rbind(results, data.frame(q, f_q))
  }
  colnames(results) <- c('q', 'f_q')
  return(results)
}


#' Simply calculate a quantum (using cosine quantogram)
#'
#' @param x
#'
#' @export
#'
get_quantum = function(x) {
  calculate_quantogram(x) %>% filter(f_q==max(f_q)) %>% pull(q)
}

#'  calculate Kendall's modular quantogram (cosine quantogram but accounting for the possibility of an offset
#'
#' @param x numeric vector of measurement values
#' @param params cosine quantogram parameters including "RNG_START", "RNG_END", "STEP", "Q_MIN"
#'
#' @return two columned dataframe of quanta (q) and their scores (f_q)
#' @export
calculate_modular_quantogram = function(x,
                                        mqparams=getOption("CONSTANTS_QUANTOGRAM")){
  CQ = calculate_quantogram(x, params = mqparams)
  SQ = calculate_sine_quantogram(x, params = mqparams)
  results = NULL
  
  for (n in 1:length(CQ$q)) {
    f_q = 1/2 * (CQ[n, 2]^2+SQ[n, 2]^2)
    results = rbind(results, data.frame(CQ[n, 1], f_q))
  }
  colnames(results) <- c('q', 'f_q')
  return(results)
}