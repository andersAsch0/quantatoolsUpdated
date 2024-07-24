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


#' sample the (non quantal) KDE repeatedly and take cosine quantograms of the samples, in order to determine the significance of quantogram scores
#'
#' @param data_input numeric vector of measurement values (unrounded)
#' @param kde the kde generated for the data (use calculate KDE)
#' @param num_simulations how many Monte Carlo simulations are run
#' @param peakHeight specific quantogram peak height to find the significance of (optional)
#' @param significance specific significance to find the minimum peak height required for (optional)
#' 
#' @return a list of 4 containing: dataframe of all max peak heights in simulations, table of significances, and the values values associated with peakHeight and significance respectively
#' @export
#' 
monte_carlo_test <- function(data_input, kde, num_simulations, peakHeight = 0, significance = 0.95){
  peaks_all = NULL
  for (n in 1:num_simulations) {
    
    # 1. Sample from kde (sample is same size as dataset)
    sample = sample_KDE(kde, length(data_input))
    
    # 2. Estimate quantum and peak of the (non quantal) sample 
    results = calculate_quantogram(sample)
    # colnames(results) <- c('q', 'f_q')
    
    # 3. Collect results
    win_score = results %>% group_by %>% filter(f_q == max(f_q)) #get best quantum from sample
    if(length(win_score) > 1){ # if there are >1 tied peaks choose one (I think its unlikely enough to not matter)
      win_score = win_score[1,]
    }
    q_hat = win_score$q #best quantum
    peak_hat = win_score$f_q #best quantum's score (height of peak)
    
    peaks_all[[n]] = data.frame(q_hat, peak_hat) #adding onto peaks_all: a list of dataframes, each containing the best quantum and its score for each sample
    
  }
  peaks_all = do.call(rbind.data.frame, peaks_all) #turn it into a dataframe of 2 cols (q_hat, and peak_hat)
  #this is a dataframe of all the peak quanta scores from each of the simulated quantograms
  
  #calculate significence of the input peak
  peaks_taller = peaks_all %>% filter(peak_hat >= peakHeight)
  percent_peaks_taller = length(peaks_taller$q_hat) / length(peaks_all$q_hat)
  
  #calculate corresponding peaks for all significances in steps of 5
  significances_all = data.frame((0:19)*5/100, quantile(peaks_all$peak_hat, (0:19)*5/100)) #vectorized 0-95%
  significances_all <- rbind(significances_all, list(0.99, quantile(peaks_all$peak_hat, 0.99))) #added in 99 %
  significances_all <- rbind(significances_all, list(1, quantile(peaks_all$peak_hat, 1)))
  percs = paste(as.character((0:19)*5), "%")
  percs[length(percs)+1] = "99 %"
  percs[length(percs)+1] = "100 %" 
  row.names(significances_all) <- percs #renaming the rows of the dataframe
  colnames(significances_all) <- c("significance", "minimum peak height")
  
  #calculate corresponding peak (quantum score) of input significance
  if((significance >= 0 ) && (significance <= 1)) {sig = quantile(peaks_all$peak_hat, significance)}
  else { sig = "Error: invalid significance parameter"}
  
  return(list("peaks" = peaks_all, "significance table" = significances_all,  "proportion of simulations that exceeded input peak" = percent_peaks_taller, "cut-off of input significance" = sig))
}
#' sample the (non quantal) KDE repeatedly and take modular quantograms of the samples, in order to determine the significance of quantogram scores
#'
#' @param data_input numeric vector of measurement values (unrounded)
#' @param kde the kde generated for the data (use calculate KDE)
#' @param num_simulations how many Monte Carlo simulations are run
#' @param peakHeight specific quantogram peak height to find the significance of (optional)
#' @param significance specific significance to find the minimum peak height required for (optional)
#' 
#' @return a list of 4 containing: dataframe of all max peak heights in simulations, table of significances, and the values values associated with peakHeight and significance respectively
#' @export
#' 
monte_carlo_test_modular <- function(data_input, kde, num_simulations, peakHeight = 0, significance = 0.95){
  peaks_all = NULL
  for (n in 1:num_simulations) {
    
    # 1. Sample from kde (sample is same size as dataset)
    sample = sample_KDE(kde, length(data_input))
    
    # 2. Estimate quantum and peak of the (non quantal) sample 
    results = calculate_modular_quantogram(sample)
    # colnames(results) <- c('q', 'f_q')
    
    # 3. Collect results
    win_score = results %>% group_by %>% filter(f_q == max(f_q)) #get best quantum from sample
    if(length(win_score) > 1){ # if there are >1 tied peaks choose one (I think its unlikely enough to not matter)
      win_score = win_score[1,]
    }
    q_hat = win_score$q #best quantum
    peak_hat = win_score$f_q #best quantum's score (height of peak)
    
    peaks_all[[n]] = data.frame(q_hat, peak_hat) #adding onto peaks_all: a list of dataframes, each containing the best quantum and its score for each sample
    
  }
  peaks_all = do.call(rbind.data.frame, peaks_all) #turn it into a dataframe of 2 cols (q_hat, and peak_hat)
  #this is a dataframe of all the peak quanta scores from each of the simulated quantograms
  
  #calculate significence of the input peak
  peaks_taller = peaks_all %>% filter(peak_hat >= peakHeight)
  percent_peaks_taller = length(peaks_taller$q_hat) / length(peaks_all$q_hat)
  
  #calculate corresponding peaks for all significances in steps of 5
  significances_all = data.frame((0:19)*5/100, quantile(peaks_all$peak_hat, (0:19)*5/100)) #vectorized 0-95%
  significances_all <- rbind(significances_all, list(0.99, quantile(peaks_all$peak_hat, 0.99))) #added in 99 %
  significances_all <- rbind(significances_all, list(1, quantile(peaks_all$peak_hat, 1)))
  percs = paste(as.character((0:19)*5), "%")
  percs[length(percs)+1] = "99 %"
  percs[length(percs)+1] = "100 %" 
  row.names(significances_all) <- percs #renaming the rows of the dataframe
  colnames(significances_all) <- c("significance", "minimum peak height")
  
  #calculate corresponding peak (quantum score) of input significance
  if((significance >= 0 ) && (significance <= 1)) {sig = quantile(peaks_all$peak_hat, significance)}
  else { sig = "Error: invalid significance parameter"}
  
  return(list("peaks" = peaks_all, "significance table" = significances_all,  "proportion of simulations that exceeded input peak" = percent_peaks_taller, "cut-off of input significance" = sig))
}

