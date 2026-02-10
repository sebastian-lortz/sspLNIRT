#' Plot the power curve from sample size optimization
#'
#' The function extracts the optimization trace from an \code{optim_sample()} object,
#' fits a log-log power curve (log(result) ~ log(N)), and displays the relationship
#' on both the log-log and original scales side by side, annotated with regression
#' coefficients and R-squared.
#'
#' @param object Object. Output from \code{optim_sample()}.
#' @param thresh Numeric (optional). Decision threshold used in optimization.
#'   If \code{NULL}, extracted from the object.
#'
#' @return A combined ggplot object (via \pkg{patchwork}).
#'
#' @examples
#'  \dontrun{
#'    plot_power_curve(object = test.optim.sample)
#' }
#'
#' @export
#'
plot_power_curve <- function(object, thresh = NULL) {
  # --- validate input ---
  if (is.null(object$trace)) {
    stop("`object` must be an sspLNIRT object from optim_sample() containing a $trace element.")
  }
  trace <- object$trace
  if (is.null(trace$track.N) || is.null(trace$track.res)) {
    stop("`object$trace` must contain $track.N and $track.res elements.")
  }
  N_vec   <- trace$track.N$N.temp
  res_vec <- trace$track.res$res.temp
  if (length(N_vec) != length(res_vec)) {
    stop("Lengths of tracked N and result vectors do not match.")
  }
  if (length(N_vec) < 3) {
    stop(paste("Cannot fit power curve. The optimization stopped early:", object$N.min))
  }
  keep <- N_vec > 0 & res_vec > 0
  if (sum(keep) < 3) {
    stop("Fewer than 3 valid (positive) trace points; cannot fit power curve.")
  }
  N_vec   <- N_vec[keep]
  res_vec <- res_vec[keep]
  if (is.null(thresh)) thresh <- object$cfg$thresh
  if (is.null(thresh)) stop("No threshold found. Supply `thresh` explicitly.")

  # --- fit log-log model ---
  fit <- stats::lm(log(res_vec) ~ log(N_vec))
  a   <- as.numeric(stats::coef(fit)[1])
  b   <- as.numeric(stats::coef(fit)[2])
  r2  <- summary(fit)$r.squared
  N_curve <- ceiling(exp((log(thresh) - a) / b))

  # --- build combined data ---
  N_grid  <- seq(min(N_vec), max(max(N_vec), N_curve * 1.1), length.out = 200)

  points_df <- rbind(
    data.frame(x = log(N_vec), y = log(res_vec), panel = "Log-log scale"),
    data.frame(x = N_vec,      y = res_vec,      panel = "Original scale")
  )
  line_df <- data.frame(
    x = N_grid, y = exp(a) * N_grid^b, panel = "Original scale"
  )
  abline_df <- data.frame(
    x = log(N_grid), y = a + b * log(N_grid), panel = "Log-log scale"
  )
  hline_df <- rbind(
    data.frame(yint = log(thresh), panel = "Log-log scale"),
    data.frame(yint = thresh,      panel = "Original scale")
  )

  p <- ggplot2::ggplot(points_df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]])) +
    ggplot2::geom_point(size = 1.8, alpha = 0.6, colour = "grey50") +
    ggplot2::geom_line(data = abline_df, colour = "grey15", linewidth = 0.7) +
    ggplot2::geom_line(data = line_df, colour = "grey15", linewidth = 0.7) +
    ggplot2::geom_hline(data = hline_df,
                        ggplot2::aes(yintercept = .data[["yint"]]),
                        linetype = "dashed", colour = "grey35", linewidth = 0.5) +
    ggplot2::geom_vline(
      data = data.frame(xint = N_curve, panel = "Original scale"),
      ggplot2::aes(xintercept = .data[["xint"]]),
      linetype = "dotted", colour = "grey35", linewidth = 0.5
    ) +
    ggplot2::facet_wrap(~ panel, scales = "free") +
    ggplot2::labs(
      x        = NULL,
      y        = "RMSE",
      title    = "Power Curve",
      subtitle = paste0("N.curve = ", N_curve, " | R.sq = ", round(r2, 3))
    ) +
    ggplot2::theme_minimal()

  return(p)
}
