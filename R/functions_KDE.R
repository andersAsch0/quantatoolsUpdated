#' Get KDE distribution
#'
#' @param data_input numeric vector of measurement values
#' @param band kernel bandwidth smooth param. higher is smoother
#' @param  grid_size the number of equally spaced points at which to estimate the density.
#'
#' @return dataframe of x and y coordinates
#' @export
#'
calculate_KDE = function(data_input, band = 1, grid_size = 1000) {
  bkde(data_input, kernel = "normal", bandwidth = band, gridsize = grid_size) %>%
    as.data.frame() %>%
    filter(x > 0 & y > 0) -> kde
  return(kde)
}

#' Sample from KDE distribution
#'
#' @param kde dataframe of kde
#' @param size number of samples
#'
#' @return vector of samples
#' @export
#'
sample_KDE = function(kde, size = 1000) {
  sample <- sample(size = size, x = kde$x, replace = T, prob = kde$y/sum(kde$y))
  return(sample)
}

#' plot kde
#'
#' @param kde dataframe of two columns, the x and y coords of points along the KDE
#'
#' @export
#'
plot_KDE = function(kde) {
  ggplot(data = kde, mapping = aes(x = x, y = y)) +
    geom_point() 
}



