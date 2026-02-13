#' Plot the simulated response time data
#'
#' The function simulates data under the Joint Hierarchical Model using a
#' 2-pl normal ogive model for response accuracy and a 3-pl log-normal model for response
#' time, and plots the resulting response time data.
#'
#' @param level String. Either "person" or "item".
#' @param logRT Logical. Whether to plot on the log-RT scale.
#' @param N Integer. The sample size.
#' @param K Integer. The test length.
#' @param mu.person Numeric vector. Means of theta and zeta.
#' @param mu.item Numeric vector. Means of alpha, beta, phi, and lambda.
#' @param meanlog.sigma2 Numeric. The meanlog of sigma2.
#' @param cov.m.person Matrix. The covariance matrix of theta and zeta.
#' @param cov.m.item Matrix. The covariance matrix of alpha, beta, phi, and lambda.
#' @param sd.item Numeric vector. The standard deviations of alpha, beta, phi, and lambda.
#' @param sdlog.sigma2 Numeric. The sdlog of sigma2.
#' @param item.pars.m Matrix (optional). A matrix containing item parameters.
#' @param cor2cov.item Logical. Whether a correlation matrix instead of covariance matrix is supplied.
#'
#' @return A ggplot object.
#'
#' @examples
#'  \dontrun{
#'    plot_RT(level = "item",
#'            mu.item = c(1, 0, .4, 1),
#'            sd.item = c(.2, 1, .2, .5),
#'            meanlog.sigma2 = log(1),
#'            K = 30,
#'            logRT = FALSE)
#' }
#'
#' @export
#'
plot_RT <- function(level,
                    logRT = FALSE,
                    N = 1e5,
                    K = 10,
                    mu.person = c(0, 0),
                    mu.item = c(1, 0, .5, 1),
                    meanlog.sigma2 = log(.6),
                    cov.m.person = matrix(c(1, .4,
                                            .4, 1), ncol = 2, byrow = TRUE),
                    cov.m.item = matrix(c(1, 0, 0, 0,
                                          0, 1, 0, 0.4,
                                          0, 0, 1, 0,
                                          0, 0.4, 0, 1), ncol = 4, byrow = TRUE),
                    sd.item = c(.2, 1, .2, .5),
                    sdlog.sigma2 = 0,
                    item.pars.m = NULL,
                    cor2cov.item = TRUE) {

  # --- input checks ---
  level <- match.arg(level, choices = c("person", "item"))

  # --- simulate data ---
  data <- sim.jhm.data(iter = 1,
                       N = N, K = K,
                       mu.person = mu.person,
                       mu.item = mu.item,
                       meanlog.sigma2 = meanlog.sigma2,
                       cov.m.person = cov.m.person,
                       cov.m.item = cov.m.item,
                       sdlog.sigma2 = sdlog.sigma2,
                       item.pars.m = item.pars.m,
                       cor2cov.item = cor2cov.item,
                       sd.item = sd.item,
                       scale = FALSE)

  logRT.data <- as.data.frame(data$time.data)
  RT.data    <- exp(logRT.data)

  # --- person level: aggregate over items ---
  if (level == "person") {

    if (logRT) {
      dat  <- data.frame(val = rowMeans(logRT.data))
      lims <- c(NA_real_, NA_real_)
      x_lab <- "Log response time"
    } else {
      dat  <- data.frame(val = rowMeans(RT.data))
      lims <- c(0, stats::quantile(dat$val, 0.99))
      x_lab <- "Response time in seconds"
    }

    probs    <- c(.01, .1, .5, .9, .99)
    quant_df <- data.frame(
      q        = as.numeric(stats::quantile(dat$val, probs)),
      quantile = factor(probs)
    )

    p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data[["val"]])) +
      ggplot2::geom_density(fill = "grey75", colour = "grey30", alpha = 0.6) +
      ggplot2::geom_vline(
        data    = quant_df,
        ggplot2::aes(xintercept = q, colour = quantile),
        linetype = "dashed", linewidth = 0.5
      ) +
      ggplot2::scale_colour_viridis_d(option = "D", end = 0.85) +
      ggplot2::coord_cartesian(xlim = lims) +
      ggplot2::labs(
        x      = x_lab,
        y      = "Density",
        colour = "Quantile"
      ) +
      ggplot2::theme_minimal()

    return(p)
  }

  # --- item level ---
  mat_sub <- if (logRT) logRT.data else RT.data

  long_dat <- stats::setNames(utils::stack(mat_sub), c("RT", "item"))
  long_dat$item <- factor(long_dat$item)

  if (logRT) {
    x_lab      <- "Log response time"
    axis.scale <- "fixed"
  } else {
    x_lab      <- "Response time in seconds"
    axis.scale <- "free_y"
    # trim extreme right tail per item
    long_dat <- do.call(rbind, lapply(split(long_dat, long_dat$item), function(d) {
      d[d$RT <= stats::quantile(d$RT, 0.975, na.rm = TRUE), ]
    }))
  }

  p <- ggplot2::ggplot(long_dat, ggplot2::aes(x = .data[["RT"]])) +
    ggplot2::geom_density(fill = "grey75", colour = "grey30",
                          alpha = 0.5, trim = TRUE) +
    ggplot2::stat_summary(
      ggplot2::aes(xintercept = ggplot2::after_stat(x), y = 0),
      fun = stats::median, geom = "vline", orientation = "y",
      linetype = "dashed", linewidth = 0.4
    ) +
    ggplot2::facet_wrap(~ item, scales = axis.scale) +
    ggplot2::labs(
      x     = x_lab,
      y     = "Density"
    ) +
    ggplot2::theme_minimal()

  return(p)
}
