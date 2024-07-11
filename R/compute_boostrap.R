#' Compute boostrap confidence interval
#'
#' @param set measurements vector
#' @param label (optional) label of dataset
#' @param conf confidence level, 0.05 by default
#' @param n_bootstrap_samples number of bootstrap samples
#'
#' @return dataframe with lots of info and ggplot
#' @export
#'
compute_bootstrap <- function(set,
                              label,
                              conf=0.05,
                              n_bootstrap_samples=500
                              ) {

  results = calculate_quantogram(set) #results is the normal quantogram (aka list of quantums and their scores)
  colnames(results) <- c('q', 'f_q')
  # TODO: if n(best_score)>1, take max
  best_score = results %>% group_by %>% filter(f_q == max(f_q)) #highest f(q)
  original_quanta = data.frame(label=label, #dataframe w three columns: label, quantum, and its score (above)
                               q_hat = best_score$q,
                               peak_hat = best_score$f_q)

  # Bootstrap confidence interval procedure
  results_all = NULL
  results_sector = NULL
  for (n in 1:n_bootstrap_samples) {

    # 1. Sample from original dataset (sample is same size as dataset)
    sample = sample_bootstrap(set)

    # 2. Estimate quantum and peak of the sample (acting as if the original set was the true makeup of the pop and seeing how far off we get from a sample)
    results = calculate_quantogram(sample)
    
    # 3. Collect results
    # TODO: if >1 maximum
    win_score = results %>% group_by %>% filter(f_q == max(f_q)) #get best quantum from sample
    q_hat = win_score$q #best quantum
    peak_hat = win_score$f_q #best quantum's score (height of peak)
    
    results_sector[[n]] = data.frame(q_hat, peak_hat) #adding onto results_sector: a list of dataframes, each containing the best quantum and its score for each sample
    
  }
  results_sector = do.call(rbind.data.frame, results_sector) #turn it into a dataframe of 3 cols (sample number, q_hat, and peak_hat)
  df_results_all = data.frame(label=label, results_sector) #same as above but with "label" instead of sample num
  
  # Plot adjustment
  borders = quantile(df_results_all$q_hat, c(0+conf/2, 1-conf/2)) # finds upper and lower percentiles to contain 1-confidence % of the data
  df_results_all %>% group_by() %>% summarise(min = min(q_hat), max = max(q_hat)) -> limits
  eps = (limits$max - limits$min) * 1.5 / 2           # scale up x axis range
  center = limits$min + (limits$max - limits$min) / 2  # get center position
  
  # Plot boostrap confidence interval for quantum
  p <- ggplot(df_results_all,
              aes(q_hat)) +
    geom_histogram(binwidth = (limits$max - limits$min)/50,
                   fill = "white",
                   color = "black")+ #plot histogram
    geom_vline( #original quantum is shown as a black dashed line
      data = original_quanta,
      aes(xintercept = q_hat),
      linetype = "dashed",
      linewidth = 1) +
    geom_vline( #upper and lower bounds are shown as red dashed line
      xintercept = borders,
      colour = "red",
      linetype = "dashed",
      linewidth = 1) +
    xlab(label = "quantum estimation") +
    theme_minimal()
  
  print(p)
  
  l <- list("p"=p, "df_results_all"=df_results_all, "borders"=borders)
  return(l)
}
