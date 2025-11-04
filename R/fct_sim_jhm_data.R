#' Simulate Data under Joint Hierarchical Model
#'
#' The function simulates data under the the Joint Hierarchical Model using a
#' 2-pl normal ogive model for response accuracy and a 3-pl log-normal model for response
#' time.
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
#' @param seed Integer. (optional) Seed for simulating the whole list of data sets.
#' @param cor2cov.item Logical. Whether a correlation matrix instead of covariance matrix is supplied
#' @param sd.item Numeric vector. (optional) The standard deviations of alpha, beta, phi, and lambda
#'
#' @return A list of length `iter` containing:
#' \describe{
#'   \item{long.data}{Data Frame. The simulated data in long format.}
#'   \item{time.data}{Data Frame. The simulated reponse time data.}
#'   \item{response.data}{Data Frame. The simulated response accuracy data.}
#'   \item{person.par}{Data Frame. The person parameters.}
#'   \item{item.par}{Data Frame. The item parameters.}
#' }
#'
#' @export
#'

sim.jhm.data <- function(iter,
                         N,
                         I,
                         mu.person = c(0,0),
                         mu.item = c(1,0,4,0),
                         meanlog.sigma2 = log(.3),
                         cov.m.person = matrix(c(1,.5,
                                                 .5,1), ncol = 2, byrow = TRUE),
                         cov.m.item = matrix(c(.2, 0, 0, 0,
                                               0, .5, -.35, -.15,
                                               0, -.35, .5, .15,
                                               0, -.15, .15, .2), ncol =  4, byrow = TRUE),
                         sdlog.sigma2 = 0.2,
                         scale = TRUE,
                         random.item = TRUE,
                         item.pars.m = NULL,
                         item.seed = NULL,
                         seed = NULL,
                         cor2cov.item = FALSE,
                         sd.item = NULL
) {

  # set seed
  set.seed(seed)

  # open lists
  sim.time <- sim.response <- person.par <- item.par <- sim.data <- list()

  # iterate
  for (k in 1:iter) {

    # person parameters
    person <- person.par(N = N,
                         cov.m.person = cov.m.person,
                         mu.person = mu.person)

    # item parameters
    if (random.item) {
      item <- item.par(I = I,
                       mu.item = mu.item,
                       cov.m.item = cov.m.item,
                       meanlog.sigma2 = meanlog.sigma2,
                       sdlog.sigma2 = sdlog.sigma2,
                       cor2cov.item = cor2cov.item,
                       sd.item = sd.item,
                       item.seed = item.seed)
    } else {
      item <- as.data.frame(
        item.pars.m
      )
    }

    # scale parameters
    if (scale) {
      scaled.pars <- scale_M(item.pars = item,
                             person.pars = person)
      item <- scaled.pars$items.pars.scaled
      person <- scaled.pars$person.pars.scaled
    }

    # open output matrix
    time <- response <- matrix(nrow = N, ncol =  I)
    colnames(time) <- colnames(response) <- paste0("Item", seq_len( I))

    # generate RA using 2par normal ogive model
    for (i in 1:I) {
      delta.b <- person$theta - item$beta[i]
      prob <- pnorm(item$alpha[i] * delta.b)
      response[,i] <- rbinom(N, 1, prob = prob)
    }

    # generate RT using 3par lognormal model
    for (r in 1:I) {
      t <- item$lambda[r] - item$phi[r] * person$zeta
      time[,r] <- t + rnorm(N, 0, sqrt(item$log.sigma2[r]))
    }

    # assemble data in long format
    data <- data.frame(
      ID = rep(1:N, I),
      Item = rep(seq_len(I), each = N),
      y = as.vector(response),
      RT = exp(as.vector(time)),
      logRT = as.vector(time)
    )

    # add to list per iteration
    sim.data[[k]] <- data
    sim.time[[k]] <- time
    sim.response[[k]] <- response
    person.par[[k]] <- person
    item.par[[k]] <- item

  }

  # print confirmation
  cat(paste("Simulated", k,"complete data sets \n"))

  # return output
  return(list(
    long.data = sim.data,
    time.data = sim.time,
    response.data = sim.response,
    person.par = person.par,
    item.par = item.par
  ))
}
