#' Plot the precision of item parameters
#'
#' The function plots the mean squared error or bias across values of the true parameter.
#'
#' @param object Object. Output from optim_sample() or comp_mse(). Must contain $err.dat.
#' @param y.val String. Either "mse" or "bias" for the y-axis.
#' @param n.bins Integer (optional). Number of bins used to aggregate by sim.val within par.
#'
#' @return A ggplot object
#'
#' @examples
#'  \dontrun{
#'    plot_precision(object = test.optim.sample)
#'}
#'
#' @export
#'

plot_precision <- function(object, y.val = "mse", n.bins = NULL) {


  require(ggplot2)
  require(dplyr)

  # extract error data from object
  if (!is.list(object) || is.null(object$err.dat)) {
    stop("`object` must be the output of comp_mse() or optim_sample() and contain `object$err.dat`.")
  }
  err.dat <- object$err.dat

  # basic checks
  req_cols <- c("par", "sim.val", "err")
  miss <- setdiff(req_cols, names(err.dat))
  if (length(miss) > 0) stop("`object$err.dat` is missing required columns: ", paste(miss, collapse = ", "))

  if (is.null(n.bins)) {
    n_min <- err.dat %>%
      count(par, name = "n") %>%
      summarise(n_min = min(n)) %>%
      pull(n_min)

    u_min <- err.dat %>%
      group_by(par) %>%
      summarise(u = n_distinct(sim.val), .groups = "drop") %>%
      summarise(u_min = min(u)) %>%
      pull(u_min)

    n.bins <- floor(sqrt(n_min))
    n.bins <- max(5, min(30, n.bins))
    n.bins <- min(n.bins, u_min)
    n.bins <- max(2, n.bins)
  }

  bin_means <- err.dat %>%
    group_by(par) %>%
    mutate(bin = ntile(sim.val, n.bins)) %>%   # <- use n.bins
    group_by(par, bin) %>%
    summarise(
      mean_sim = mean(sim.val, na.rm = TRUE),
      mean_err = mean(err, na.rm = TRUE),
      mean_mse = mean(err^2, na.rm = TRUE),
      bin.size = n(),                          # <- bin size per par/bin
      .groups = "drop"
    )

  # summary bin size info to report
  bin_info <- bin_means %>%
    summarise(
      n.bins = dplyr::n_distinct(bin),
      bin.size.median = median(bin.size),
      bin.size.min = min(bin.size),
      bin.size.max = max(bin.size)
    )

  if (y.val == "bias") {
    Y <- "mean_err"
    y_lab <- "Mean bias (per bin)"
  } else if (y.val == "mse") {
    Y <- "mean_mse"
    y_lab <- "Mean squared error (per bin)"
  } else {
    stop("y.val must be either 'bias' or 'mse'")
  }

  p <- ggplot(bin_means, aes(x = mean_sim, y = .data[[Y]])) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(size = 2, alpha = 0.8, color = "grey60") +
    geom_smooth(method = "loess", span = 0.8, se = FALSE, color = "black") +
    facet_wrap(~ par, scales = "free_x") +
    labs(
      x = "Mean simulated value (per bin)",
      y = y_lab,
      caption = paste0(
        "Bins: ", bin_info$n.bins
      )
    )

  return(p)
}
