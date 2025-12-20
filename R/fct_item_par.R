#' Generate Item parameters
#'
#' The function generates a data frame with item parameters from a truncated
#' multivariate normal distribution for alpha, beta, phi, lambda, and from a
#' lognormal distribution for sigma2.
#'
#' @param I Integer. The test length or number of items.
#' @param mu.item Numeric vector. Means of alpha, beta, phi, and lambda
#' @param cov.m.item Matrix. The covariance matrix of of alpha, beta, phi, and lambda
#' @param meanlog.sigma2 Numeric. The meanlog of sigma2
#' @param sdlog.sigma2 Numeric. The sdlog of sigma2
#' @param cor2cov.item Logical. Whether a correlation matrix instead of covariance matrix is supplied
#' @param sd.item Numeric vector. (optional) The standard deviations of alpha, beta, phi, and lambda
#'
#' @return A data frame containing item parameters
#'\describe{
#'   \column{alpha}{Numeric vector. The values of the item discrimination parameter.}
#'   \column{beta}{Numeric vector. The values of the item difficulty parameter.}
#'   \column{phi}{Numeric vector. The values of the time discrimination parameter.}
#'   \column{lambda}{Numeric vector. The values of the time intensity parameter.}
#'   \column{sigma2}{Numeric vector. The values of the error variance parameter.}
#' }
#'
#' @examples
#'  \dontrun{
#'
#' }
#' @export
#'

item.par <- function(I,
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
  log.sigma2 <- rlnorm(I, meanlog = meanlog.sigma2, sdlog = sdlog.sigma2)

  # if correlation matrix is supplied
  if (cor2cov.item) {
    cov.m.item <- lavaan::cor2cov(cov.m.item , sds = sd.item)
  }

  # sample alpha, beta, phi, lambda
  item.pars <- tmvtnorm::rtmvnorm(n = I,
                                  mean = mu.item,
                                  sigma = cov.m.item,
                                  lower = c(0, -Inf, 0, -Inf),
                                  upper = c(Inf, Inf, Inf, Inf))
  colnames(item.pars) <- c("alpha", "beta", "phi", "lambda")


  # combine item parameters
  pars.out <- as.data.frame(
    cbind(item.pars, log.sigma2)
  )

  # return output
  return(
    pars.out
  )
}



