#' Generate Person Parameters
#'
#' The function generates a data frame with person parameters from a
#' multivariate normal distribution for theta and zeta.
#'
#' @param N Integer. The sample size or number of rows.
#' @param mu.person Numeric vector. Means of theta and zeta
#' @param cov.m.person Matrix. The covariance matrix of theat and zeta
#'
#' @return A data frame containing person parameters
#'\describe{
#'   \column{theta}{Numeric vector. The values of theta.}
#'   \column{zeta}{Numeric vector. The values of zeta,}
#' }
#'
#' @examples
#'  \dontrun{
#' test.data <- person.par(N = 50,
#'                           mu.person = c(0,0),
#'                          cov.m.person = matrix(c(1,.5,
#'                                                  .5,1), ncol = 2, byrow = TRUE),
#'                          person.seed = 123)
#'}
#'
#' @export
#'

person.par <- function(N,
                       mu.person = c(0,0),
                       cov.m.person = matrix(c(1,.5,
                                               .5,1), ncol = 2, byrow = TRUE)
                       ) {

  # sample person parameters from MVN
  hyper.par = as.data.frame(
    MASS::mvrnorm(N,
            mu = mu.person,
            Sigma = cov.m.person
    )
  )

  # first col: theta, second col: zeta
  colnames(hyper.par) <- c("theta", "zeta")

  # return output
  return(
    hyper.par
  )
}
