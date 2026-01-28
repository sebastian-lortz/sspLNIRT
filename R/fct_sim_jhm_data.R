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
#' @param item.pars.m Matrix. (optional) A Matrix containing item parameters.
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
#' @examples
#'  \dontrun{
#' test.data <- sim.jhm.data(iter = 2,
#'                          N = 100,
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
#'                          sdlog.sigma2 = 0.2
#'                          )
#'}
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
                         item.pars.m = NULL,
                         cor2cov.item = FALSE,
                         sd.item = NULL
) {

  # open lists
  sim.time <- sim.response <- person.par <- item.par <- sim.data <- scale.factor <- list()

  # iterate
  for (k in 1:iter) {

    # person parameters
    person <- person.par(N = N,
                         cov.m.person = cov.m.person,
                         mu.person = mu.person)

    # item parameters
    if (is.null(item.pars.m)) {
      item <- item.par(I = I,
                       mu.item = mu.item,
                       cov.m.item = cov.m.item,
                       meanlog.sigma2 = meanlog.sigma2,
                       sdlog.sigma2 = sdlog.sigma2,
                       cor2cov.item = cor2cov.item,
                       sd.item = sd.item)
    } else {
      item <- as.data.frame(
        item.pars.m
      )
    }

    # scale parameters
      scaled.pars <- scale_M(item.pars = item,
                             person.pars = person)
      item <- scaled.pars$items.pars.scaled
      person <- scaled.pars$person.pars.scaled

    # open output matrix
    time <- response <- matrix(nrow = N, ncol =  I)
    colnames(time) <- colnames(response) <- paste0("Item", seq_len( I))

    # generate RA using 2par normal ogive model
    for (i in 1:I) {
      response[,i] <- rbinom(N,
                             1,
                             prob = pnorm(item$alpha[i] * (person$theta - item$beta[i])))
    }

    # generate RT using 3par lognormal model
    for (r in 1:I) {
      time[,r] <- item$lambda[r] - item$phi[r] * person$zeta + rnorm(N, 0, sqrt(item$sigma2[r]))
    }

    # add to list per iteration
    sim.time[[k]] <- time
    sim.response[[k]] <- response
    person.par[[k]] <- person
    item.par[[k]] <- item
    scale.factor[[k]] <- data.frame(c.alpha = scaled.pars$c.alpha,
                          c.phi = scaled.pars$c.phi)

  }

  # print confirmation
  cat(paste("Simulated", k,"complete data sets \n"))

  # return output
  return(list(
    time.data = sim.time,
    response.data = sim.response,
    person.par = person.par,
    item.par = item.par,
    scale.factor = scale.factor
  ))
}
