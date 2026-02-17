#' Plot the simulated response accuracy data
#'
#' The function simulates data under the Joint Hierarchical Model using a
#' 2-pl normal ogive model for response accuracy and a 3-pl log-normal model for response
#' time, and plots the resulting response accuracy data.
#'
#' @param level String. Either "person" or "item".
#' @param by.theta Logical. Whether to plot as a function of theta.
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
#'    plot_RA(level = "item",
#'            by.theta = TRUE,
#'            mu.item = c(1, 0, 1, 1),
#'            sd.item = c(.2, .5, .2, .5),
#'            meanlog.sigma2 = log(.2),
#'            K = 20)
#' }
#'
#' @export
#'
plot_RA <- function(level,
                    by.theta = FALSE,
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

  RA.data     <- as.data.frame(data$response.data[[1]])
  item.data   <- as.data.frame(data$item.par[[1]])
  person.data <- as.data.frame(data$person.par[[1]])

  # --- person level ---
  if (level == "person") {

    if (!by.theta) {

      dat <- data.frame(RA = as.integer(rowSums(RA.data)))

      p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data[["RA"]])) +
        ggplot2::geom_histogram(
          binwidth = 1, boundary = -0.5, closed = "left",
          fill = "grey75", colour = "grey30", alpha = 0.6
        ) +
        ggplot2::scale_x_continuous(breaks = 0:K, limits = c(-0.5, K + 0.5)) +
        ggplot2::labs(
          x = "Total correct score",
          y = "Count"
        ) +
        ggplot2::theme_minimal()

      return(p)

    } else {

      dat <- data.frame(
        theta = person.data$theta,
        RA    = rowSums(RA.data)
      )

      p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data[["theta"]], y = .data[["RA"]])) +
        ggplot2::stat_summary_bin(fun = mean, bins = K,
                                  geom = "col", fill = "grey75",
                                  colour = "grey30", alpha = 0.6) +
        ggplot2::labs(
          x = "Theta",
          y = "Total correct score"
        ) +
        ggplot2::theme_minimal()

      return(p)
    }
  }

  # --- item level ---
  theta <- person.data$theta
  alpha <- item.data$alpha
  beta  <- item.data$beta

  eta      <- sweep(outer(theta, beta, FUN = "-"), 2, alpha, FUN = "*")
  prob_mat <- stats::pnorm(eta)

  prob_long <- data.frame(
    prob  = as.vector(prob_mat),
    item = factor(paste0("Item", rep(seq_len(length(alpha)), each = length(theta)))),
    theta = rep(theta, K)
  )

  if (!by.theta) {

    p <- ggplot2::ggplot(prob_long, ggplot2::aes(x = .data[["prob"]])) +
      ggplot2::geom_density(fill = "grey75", colour = "grey30",
                            alpha = 0.5, trim = TRUE) +
      ggplot2::stat_summary(
        ggplot2::aes(xintercept = ggplot2::after_stat(x), y = 0),
        fun = stats::median, geom = "vline", orientation = "y",
        linetype = "dashed", linewidth = 0.4
      ) +
      ggplot2::facet_wrap(~ item, scales = "free_y") +
      ggplot2::labs(
        x     = "Probability of correct response",
        y     = "Density"
      ) +
      ggplot2::theme_minimal()

  } else {

    # sort by theta within each item for clean area/line rendering
    prob_long <- prob_long[order(prob_long$item, prob_long$theta), ]

    # theta at which P(correct) = 0.50
    beta_df <- data.frame(
      item     = factor(paste0("Item", seq_len(length(beta)))),
      beta_val = beta
    )

    p <- ggplot2::ggplot(prob_long, ggplot2::aes(x = .data[["theta"]],
                                                 y = .data[["prob"]])) +
      ggplot2::geom_area(fill = "grey75", alpha = 0.5) +
      ggplot2::geom_line(colour = "grey30", linewidth = 0.4) +
      ggplot2::geom_vline(
        data     = beta_df,
        ggplot2::aes(xintercept = .data[["beta_val"]]),
        linetype = "dashed", linewidth = 0.4
      ) +
      ggplot2::facet_wrap(~ item, scales = "fixed") +
      ggplot2::labs(
        x     = "Theta",
        y     = "Probability of correct response"
      ) +
      ggplot2::theme_minimal()
  }

  return(p)
}
