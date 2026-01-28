#' Plot the simulated response accuracy data on the population level
#'
#' The function simulates data under the the Joint Hierarchical Model using a
#' 2-pl normal ogive model for response accuracy and a 3-pl log-normal model for response
#' time, and plots the resulting response accuracy data.
#'
#' @param iter Integer. The number of iteration or the number of data sets.
#' @param N Integer. The sample size.
#' @param I Integer. The test length.
#' @param mu.person Numeric vector. Means of theta and zeta
#' @param mu.item Numeric vector. Means of alpha, beta, phi, and lambda
#' @param meanlog.sigma2 Numeric. The meanlog of sigma2.
#' @param cov.m.person Matrix. The covariance matrix of theat and zeta
#' @param cov.m.item Matrix. The covariance matrix of of alpha, beta, phi, and lambda
#' @param sdlog.sigma2 Numeric. The sdlog of sigma2
#' @param item.pars.m Matrix. (optional) A Matrix containing item parameters.
#' @param cor2cov.item Logical. Whether a correlation matrix instead of covariance matrix is supplied
#' @param sd.item Numeric vector. (optional) The standard deviations of alpha, beta, phi, and lambda
#'
#' @return A ggplot object
#'
#' @examples
#'  \dontrun{
#'     plot_RA(level = "item",
#'             by.theta = TRUE,
#'             mu.item = c(1,0,1,1),
#'             sd.item = c(.2, .5, .2, .5),
#'             meanlog.sigma2 = log(.2),
#'             I = 20)
#'}
#'
#' @export
#'
plot_RA <- function(level ,
                    by.theta = FALSE,
                    N = 1e5,
                    I = 20,
                    mu.person = c(0,0),
                    mu.item = c(1,0,1,0),
                    meanlog.sigma2 = log(.3),
                    cov.m.person = matrix(c(1,.5,
                                            .5,1), ncol = 2, byrow = TRUE),
                    cov.m.item = matrix(c(1, 0, 0, 0,
                                          0, 1, 0, 0,
                                          0, 0, 1, 0,
                                          0, 0, 0, 1), ncol =  4, byrow = TRUE),
                    sd.item = c(.2, 1, .2, .5),
                    sdlog.sigma2 = 0.2,
                    item.pars.m = NULL,
                    cor2cov.item = TRUE
) {

  require(ggplot2)
  require(dplyr)


  # simulate data
  data <- sim.jhm.data(iter = 1,
                       N = N,
                       I = I,
                       mu.person = mu.person,
                       mu.item = mu.item,
                       meanlog.sigma2 = meanlog.sigma2,
                       cov.m.person = cov.m.person,
                       cov.m.item = cov.m.item,
                       sdlog.sigma2 = sdlog.sigma2,
                       item.pars.m = item.pars.m,
                       cor2cov.item = cor2cov.item,
                       sd.item = sd.item)

  # get RA
  RA.data <- as.data.frame(data$response.data[[1]])
  item.data <- as.data.frame(data$item.par[[1]])
  person.data <- as.data.frame(data$person.par[[1]])

  # person level, aggregate over items
  # person level, aggregate over items
  if (level == "person") {

    if (!by.theta) {

      dat  <- data.frame(RA = as.integer(rowSums(RA.data)))
      head <- "Response accuracy of persons over items"

      # quantiles
      probs <- c(.01, .1, .5, .9, .99)
      quant <- apply(dat, 2, function(x) quantile(x, probs))
      quant_df <- data.frame(
        q        = as.numeric(quant),
        quantile = paste0(probs)
      )

      p <- ggplot(dat, aes(x = RA)) +
        geom_histogram(
          binwidth = 1,
          boundary = -0.5,
          closed = "left",
          alpha = .4
        ) +
        geom_vline(
          data = quant_df,
          aes(xintercept = q, colour = quantile),
          linetype = "dashed",
          linewidth = 0.6
        ) +
        scale_x_continuous(breaks = 0:I, limits = c(-0.5, I + 0.5)) +
        scale_colour_grey(start = 0.1, end = 0.5) +
        labs(
          x      = "Total correct score",
          y      = "Count",
          colour = "Score Quantiles"
        ) +
        ggtitle(head)

      return(p)

    } else {

      dat  <- data.frame(
        theta = person.data$theta,
        RA    = rowSums(RA.data)   # proportion correct
      )
      head <- "Response accuracy of persons by theta"

      # optional theta quantiles (to mirror your quantile lines logic)
      probs <- c(.01, .1, .5, .9, .99)
      quant <- quantile(dat$theta, probs)
      quant_df <- data.frame(
        q        = as.numeric(quant),
        quantile = paste0(probs)
      )

      p <- ggplot(dat, aes(x = theta, y = RA)) +
        stat_summary_bin(fun = mean, bins = I, geom = "col", alpha = 0.4) +
        stat_summary_bin(fun = mean, bins = 60, geom = "line", alpha = 0.4) +
        geom_vline(
          data = quant_df,
          aes(xintercept = q, colour = quantile),
          linetype = "dashed",
          linewidth = 0.6
        ) +
        scale_colour_grey(start = 0.1, end = 0.5) +
        labs(
          x      = "Theta",
          y      = "Total correct score",
          colour = "Theta Quantiles"
        ) +
        ggtitle(head)

      return(p)
    }
  }


  # item level
  if (level == "item") {

    # vectors
    theta <- person.data$theta
    alpha <- item.data$alpha
    beta  <- item.data$beta

    # compute
    eta <- sweep(outer(theta, beta, FUN = "-"), 2, alpha, FUN = "*")

    # probabilities under the normal ogive:
    prob_mat <- pnorm(eta)

    # long format
    prob_long <- data.frame(
      prob = as.vector(prob_mat),
      item = factor(rep(seq_len(length(alpha)), each = length(theta))),
      theta = rep(theta, I)
    )

    # limits
    lab = "Probability of correct reponse"
    axis.scale = if (by.theta) "fixed" else "free_y"
    lims <- quantile(prob_long$prob, c(.025/I, 1-.025/I), na.rm = TRUE)

    # plot
    if (!by.theta) {
      p <- ggplot(prob_long, aes(x = prob, fill = item)) +
        geom_density(alpha = 0.2, trim = TRUE, fill = "grey50", color = "grey30") +
        stat_summary(aes(xintercept = after_stat(x), y = 0), fun = median, geom = "vline", orientation = "y", linetype = 2) +
        labs(
          x      = "Response accuracy",
          y      = "Density",
          colour = "Item",
          title  = "Within-item response accuracy distributions"
        ) +
        facet_wrap(~ item, scales = axis.scale,
        ) +
        labs(x = lab, y = "Density")

    } else {
      p <- ggplot(prob_long, aes(x = theta, y = prob, colour = item)) +
        geom_line(alpha = 0.4, color = "grey30") +
        labs(
          x      = "Response accuracy",
          y      = "Density",
          colour = "Item",
          title  = "Within-item response accuracy distributions"
        ) +
        facet_wrap(~ item, scales = axis.scale,
        ) +
        labs(x = "Theta", y = lab)
    }

    return(p)
  }
}

