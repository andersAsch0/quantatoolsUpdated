#' Compute boostrap confidence interval
#'
#' @param set measurements vector
#' @param label (optional) label of dataset
#' @param conf confidence level, 0.05 by default
#' @param n_bootstrap_samples number of bootstrap samples
#'
#' @return
#' @export
#'
#' @examples
compute_bootstrap <- function(set,
                              label,
                              conf=0.05,
                              n_bootstrap_samples=500
                              ) {

  results = calculate_quantogram(set)
  # TODO: if n(best_score)>1, take max
  best_score = results %>% group_by %>% filter(f_q == max(f_q)) #highest f(q)
  original_quanta = data.frame(label=label,
                               q_hat = best_score$q,
                               peak_hat = best_score$f_q)

  # Bootstrap confidence interval procedure
  results_all = NULL
  results_sector = NULL
  for (n in 1:n_bootstrap_samples) {

    # 1. Sample from original dataset
    sample = sample_bootstrap(set)

    # 2. Estimate quantum and peak
    results = calculate_quantogram(sample)

    # 3. Collect results
    # TODO: if >1 maximum
    win_score = results %>% group_by %>% filter(f_q == max(f_q))
    q_hat = win_score$q
    peak_hat = win_score$f_q

    results_sector[[n]] = data.frame(q_hat, peak_hat)

  }
  results_sector = do.call(rbind.data.frame, results_sector)
  df_results_all = data.frame(label=label, results_sector)

  # Plot adjustment
  borders = quantile(df_results_all$q_hat, c(0+conf/2, 1-conf/2))
  df_results_all %>% group_by() %>% summarise(min = min(q_hat), max = max(q_hat)) -> limits
  eps = (limits$max - limits$min) * 1.5 / 2           # scale up x axis range
  center = limits$min + (limits$max - limits$min) / 2  # get center position

  # Plot boostrap confidence interval for quantum
  p <- ggplot(df_results_all,
              aes(q_hat)) +
    geom_histogram(binwidth = getOption("CONSTANTS_QUANTOGRAM")$STEP,
                   fill = "white",
                   color = "black")+
  geom_vline(
    data = original_quanta,
    aes(xintercept = q_hat),
    linetype = "dashed",
    linewidth = 1) +
  geom_vline(
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
