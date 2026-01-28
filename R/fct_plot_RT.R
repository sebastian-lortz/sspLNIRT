#' Plot the simulated response time data on the population level
#'
#' The function simulates data under the the Joint Hierarchical Model using a
#' 2-pl normal ogive model for response accuracy and a 3-pl log-normal model for response
#' time, and plots the resulting response time data.
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
#'     plot_RT(level = "item",
#'             mu.item = c(1,0,1,1),
#'             sd.item = c(.2, 1, .2, .5),
#'             meanlog.sigma2 = log(.2),
#'             I = 30,
#'             logRT = FALSE)
#'}
#'
#' @export
#'
plot_RT <- function(level ,
                         logRT = FALSE,
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
  require(sspLNIRT)


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

    # get RT
    logRT.data <- as.data.frame(data$time.data)
    RT.data    <- exp(logRT.data)

    # person level, aggregate over items
    if (level == "person") {

      if (logRT) {
        dat  <- data.frame(logRT = rowMeans(logRT.data))
        X    <- "logRT"
        head <- "Log response time of persons over items"
      } else {
        dat  <- data.frame(RT = rowMeans(RT.data))
        X    <- "RT"
        head <- "Response time of persons over items"
      }

      # quantiles
      probs <- c(.01, .1, .5, .9, .99)
      quant <- apply(dat, 2, function(x) quantile(x, probs))
      quant_df <- data.frame(
        q        = as.numeric(quant),
        quantile = paste0(probs)
      )
      lims <- apply(dat, 2, function(x) quantile(x, c(.01, .99)))

      p <- ggplot(dat, aes(x = .data[[X]])) +
        geom_density() +
        geom_vline(
          data = quant_df,
          aes(xintercept = q, colour = quantile),
          linetype = "dashed",
          linewidth = 0.6
        ) +
        scale_colour_grey(start = 0.1, end = 0.5) +  # Same as in plot_RA
        xlim(lims) +
        labs(
          x      = if (logRT) "Log response time" else "Response time in seconds",
          y      = "Density",
          colour = "Quantiles"
        ) +
        ggtitle(head)

      return(p)
    }

    # item level
    if (level == "item") {

      # choose log
      mat_sub <- if (logRT) logRT.data else RT.data

      # long format
      long_dat <- stack(mat_sub)
      colnames(long_dat) <- c("RT", "item")
      long_dat$item <- factor(long_dat$item)

      # limits
      if (logRT) {
        lab = "Response time in log-seconds"
        axis.scale = "fixed"
        lims <- quantile(long_dat$RT, c(.025/I, 1-.025/I), na.rm = TRUE)
      } else {
        lab = "Response time in seconds"
        axis.scale = "free_y"
        lims <- NULL
        long_dat <- long_dat %>%
          group_by(item) %>%
          filter(RT <= quantile(RT, 0.975, na.rm = TRUE)) %>%
          ungroup()

      }
      # plot
      p <- ggplot(long_dat, aes(x = RT, fill = item)) +
        geom_density(alpha = 0.2, trim = TRUE, fill = "grey50", color = "grey30") +
        stat_summary(aes(xintercept = after_stat(x), y = 0), fun = median, geom = "vline", orientation = "y", linetype = 2) +
         labs(
            x      = if (logRT) "Log response time" else "Response time in seconds",
            y      = "Density",
            colour = "Item",
            title  = "Within-item response time distributions"
          ) +
        facet_wrap(~ item, scales = axis.scale,
                   ) +
        labs(x = lab, y = "Density")


      return(
        if (logRT) { p + coord_cartesian(xlim = lims)
          } else {
          p
        }
      )
    }
}


