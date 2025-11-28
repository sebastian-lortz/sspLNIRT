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
#' @param scale Logical. Weather the item and person parameters are scaled.
#' @param random.item Logical. Weather the item parameters are sampled.
#' @param item.pars.m Matrix. (optional) A Matrix containing item parameters.
#' @param item.seed Integer. (optional) Seed for drawing samples from item parameter distributions.
#' @param person.seed Integer. (optional) Seed for drawing samples from person parameter distributions.
#' @param cor2cov.item Logical. Whether a correlation matrix instead of covariance matrix is supplied
#' @param sd.item Numeric vector. (optional) The standard deviations of alpha, beta, phi, and lambda
#'
#' @return A ggplot object
#'
#' @examples
#'  \dontrun{
#'     test.plot <- plot_RT(level,
#'                          logRT = FALSE,
#'                          iter = 10,
#'                          N = 1000,
#'                          I = 10,
#'                          mu.person = c(0,0),
#'                          mu.item = c(1,0,1,0),
#'                          meanlog.sigma2 = log(.3),
#'                          cov.m.person = matrix(c(1,0.5,
#'                                                  0.5,1), ncol = 2, byrow = TRUE),
#'                          cov.m.item = matrix(c(.2, 0, 0, 0,
#'                                                0, .5, 0, 0,
#'                                                0, 0, .5, 0,
#'                                                0, 0, 0, .2), ncol =  4, byrow = TRUE),
#'                          sdlog.sigma2 = 0.2,
#'                          item.seed = NULL,
#'                          person.seed = NULL
#'                          )
#'}
#'
#' @export
#'
plot_RT <- function(level ,
                         logRT = FALSE,
                         N = 5e4,
                         I = 20,
                         mu.person = c(0,0),
                         mu.item = c(1,0,1,4),
                         meanlog.sigma2 = log(.3),
                         cov.m.person = matrix(c(1,.5,
                                                 .5,1), ncol = 2, byrow = TRUE),
                         cov.m.item = matrix(c(1, 0, 0, 0,
                                               0, 1, 0, 0,
                                               0, 0, 1, 0,
                                               0, 0, 0, 1), ncol =  4, byrow = TRUE),
                         sd.item = c(.2, .5, .2, .5),
                         sdlog.sigma2 = 0.2,
                         scale = TRUE,
                         random.item = TRUE,
                         item.pars.m = NULL,
                         item.seed = NULL,
                         person.seed = NULL,
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
                         scale = scale,
                         random.item = random.item,
                         item.pars.m = item.pars.m,
                         item.seed = item.seed,
                         person.seed = person.seed,
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
        xlim(lims) +
        theme_minimal() +
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

      # density
      dens_dat <- long_dat %>%
        group_by(item) %>%
        do({
          d <- density(.$RT, adjust = 2)
          data.frame(x = d$x, density = d$y)
        })

      # limits
      lims <- quantile(long_dat$RT, c(.01, .99), na.rm = TRUE)

      # plot
      p <- ggplot(dens_dat, aes(x = x, y = density, colour = item)) +
          geom_line() +
          coord_cartesian(xlim = lims) +
          theme_minimal() +
          labs(
            x      = if (logRT) "Log response time" else "Response time in seconds",
            y      = "Density",
            colour = "Item",
            title  = "Within-item response time distributions (per-item KDE)"
          )
      return(p)
    }
}

plot_RT(level = "person", I = 30, logRT = TRUE)

