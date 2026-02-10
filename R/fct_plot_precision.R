#' Plot the precision of item parameters
#'
#' The function plots the mean squared error or bias across values of the true parameter. For objects from \code{optim_sample()}, the
#' function uses the data at the minimum N, or, if the optimization stopped due to N being outside the specified range, at the respective bound.
#'
#' @param object Object. Output from \code{optim_sample()} or \code{comp_rmse()}.
#' @param pars String. Either "item" or "person" for which parameters to plot.
#' @param y.val String. Either "rmse" or "bias" for the y-axis.
#' @param n.bins Integer (optional). Number of bins used to aggregate by sim.val within par.
#'
#' @return A ggplot object.
#'
#' @examples
#'  \dontrun{
#'    plot_precision(object = test.optim.sample, pars = "item")
#' }
#'
#' @export
#'
plot_precision <- function(object, pars, y.val = "rmse", n.bins = 30) {

  # --- input checks ---
  pars  <- match.arg(pars, choices = c("item", "person"))
  y.val <- match.arg(y.val, choices = c("rmse", "bias"))

  # --- extract error data ---
  if (!is.null(object$comp.rmse)) {
    err.dat.list <- object$comp.rmse$err.dat
  } else if (!is.null(object$err.dat)) {
    err.dat.list <- object$err.dat
  } else {
    stop("`object` must be an sspLNIRT object from comp_rmse() or optim_sample().")
  }

  err.dat <- err.dat.list[[pars]]
  if (is.null(err.dat)) {
    stop("No error data found for pars = '", pars, "'.")
  }

  # --- detect format and bin if needed ---
  is_binned <- all(c("mean_sim", "mean_err", "mean_rmse") %in% names(err.dat))
  is_full   <- all(c("rep", "par", "sim.val", "err") %in% names(err.dat))

  if (is_binned) {
    bin_means <- err.dat
  } else if (is_full) {
    bin_means <- do.call(rbind, lapply(
      split(err.dat, err.dat$par), function(d) {
        d$bin <- as.integer(cut(rank(d$sim.val, ties.method = "first"),
                                breaks = n.bins, labels = FALSE))
        do.call(rbind, lapply(split(d, d$bin), function(b) {
          data.frame(
            par       = b$par[1],
            bin       = b$bin[1],
            mean_sim  = mean(b$sim.val, na.rm = TRUE),
            mean_err  = mean(b$err, na.rm = TRUE),
            mean_rmse = sqrt(mean(b$err^2, na.rm = TRUE))
          )
        }))
      }
    ))
    rownames(bin_means) <- NULL
  } else {
    stop("Unrecognized error data format. Expected either full data ",
         "(rep, par, sim.val, err) or binned data (par, bin, mean_sim, ",
         "mean_err, mean_rmse).")
  }

  # filter sigma2 for item parameters
  if (pars == "item") {
    bin_means <- bin_means[bin_means$par != "sigma2", ]
  }

  # --- y variable ---
  if (y.val == "bias") {
    Y     <- "mean_err"
    y_lab <- "Mean bias (per bin)"
  } else {
    Y     <- "mean_rmse"
    y_lab <- "Mean RMSE (per bin)"
  }

  n_bins_actual <- length(unique(bin_means$bin))

  # --- plot ---
  p <- ggplot2::ggplot(bin_means, ggplot2::aes(x = .data[["mean_sim"]],
                                               y = .data[[Y]])) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_point(size = 1.8, alpha = 0.7, colour = "grey50") +
    ggplot2::geom_smooth(method = "loess", span = 0.8,
                         se = FALSE, colour = "grey15", linewidth = 0.6) +
    ggplot2::facet_wrap(~ par, scales = "free_x") +
    ggplot2::labs(
      x        = "Mean simulated value (per bin)",
      y        = y_lab,
      caption  = paste0("Bins: ", n_bins_actual)
    ) +
    ggplot2::theme_minimal()

  return(p)
}
