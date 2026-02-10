#' Generate Item parameters
#'
#' The function generates a data frame with item parameters from a truncated
#' multivariate normal distribution for alpha, beta, phi, lambda, and from a
#' lognormal distribution for sigma2.
#'
#' @param K Integer. The test length or number of items.
#' @param mu.item Numeric vector. Means of alpha, beta, phi, and lambda
#' @param cov.m.item Matrix. The covariance matrix of of alpha, beta, phi, and lambda
#' @param meanlog.sigma2 Numeric. The meanlog of sigma2
#' @param sdlog.sigma2 Numeric. The sdlog of sigma2
#' @param cor2cov.item Logical. Whether a correlation matrix instead of covariance matrix is supplied
#' @param sd.item Numeric vector. (optional) The standard deviations of alpha, beta, phi, and lambda
#'
#' @return A data frame with K rows and 5 columns:
#' \describe{
#'   \item{alpha}{Numeric. Item discrimination parameter.}
#'   \item{beta}{Numeric. Item difficulty parameter.}
#'   \item{phi}{Numeric. Time discrimination parameter.}
#'   \item{lambda}{Numeric. Time intensity parameter.}
#'   \item{sigma2}{Numeric. Error variance parameter.}
#' }
#'
#' @examples NULL
#' @noRd
#'

item.par <- function(K,
                     mu.item = c(1,0,4,0),
                     cov.m.item = matrix(c(.2, 0, 0, 0,
                                           0, .5, -.35, -.15,
                                           0, -.35, .5, .15,
                                           0, -.15, .15, .2), ncol =  4, byrow = TRUE),
                     meanlog.sigma2 = log(.3),
                     sdlog.sigma2 = 0.2,
                     cor2cov.item = FALSE,
                     sd.item = NULL
) {

  # sample sigma2 from lognormal distribution
  sigma2 <- rlnorm(K, meanlog = meanlog.sigma2, sdlog = sdlog.sigma2)

  # if correlation matrix is supplied
  if (cor2cov.item) {
    D <- diag(sd.item)
    cov.m.item <- D %*% cov.m.item %*% D
  }

  # sample alpha, beta, phi, lambda
  item.pars <- tmvtnorm::rtmvnorm(n = K,
                                  mean = mu.item,
                                  sigma = cov.m.item,
                                  lower = c(0, -Inf, 0, -Inf),
                                  upper = c(Inf, Inf, Inf, Inf))
  colnames(item.pars) <- c("alpha", "beta", "phi", "lambda")


  # combine item parameters
  pars.out <- as.data.frame(
    cbind(item.pars, sigma2)
  )

  # return output
  return(
    pars.out
  )
}



