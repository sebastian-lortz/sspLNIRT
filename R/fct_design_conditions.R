#' Generate the Design Conditions
#'
#' The function generates a list with specified design conditions for all
#' manipulated parameters of the sample size estimation function.
#'
#' @param
#'
#' @return A list containing  parameters
#'\describe{
#'   \item{out.par}{Character. The name of the target parameter for the threshold.}
#'   \item{thresh}{Numeric. The desired threshold of the target parameter to be achieved.}
#'   \item{I}{Integer. The test length.}
#'   \item{cov.m.person}{Matrix. The covariance matrix of theat and zeta}
#'   \item{mu.item}{Numeric vector. Means of alpha, beta, phi, and lambda.}
#'   \item{cov.m.item}{Matrix. The covariance matrix of of alpha, beta, phi, and lambda.}
#' }
#'
#' @examples
#'  \dontrun{
#' design_conditions()
#'}
#'
#' @export
#'

design_conditions <- function(exclude = NULL) {

# parameter combinations
  par_vals <- list(
    out.par         = c("mse.alpha", "mse.beta", "mse.phi", "mse.lambda"),
    thresh          = c(.01, .005, .001),
    I               = c(15, 30, 45),
    rho             = c(.2, .4, .6),
    mu.alpha        = c(.6, 1.0, 1.4),
    cov.beta.lambda = c(.2, .4, .6),
    meanlog.sigma2  = c(log(.2), log(.6), log(1))
  )

  # exclude
  if (!is.null(exclude)) {
    par_vals <- par_vals[setdiff(names(par_vals), exclude)]
  }

  # create grid
  par.grid <- do.call(
    expand.grid,
    c(par_vals, list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
  )

# storage
design <- list()

# create design
for (i in 1:nrow(par.grid)) {

  # get parameter values
  out.par = par.grid$out.par[i]
  thresh = par.grid$thresh[i]
  I = par.grid$I[i]
  mu.alpha = par.grid$mu.alpha[i]
  rho = par.grid$rho[i]
  cov.beta.lambda = par.grid$cov.beta.lambda[i]
  meanlog.sigma2 = par.grid$meanlog.sigma2[i]

  # create design
  design[[i]] <- list(
    out.par = out.par,
    thresh = thresh,
    I = I,
    cov.m.person = matrix(c(1, rho,
                            rho ,1), ncol = 2, byrow = TRUE),
    mu.item = c(mu.alpha, 0, 1, 0),
    cov.m.item = matrix(c(1, 0, 0, 0,
                          0, 1, 0, cov.beta.lambda,
                          0, 0, 1, 0,
                          0, cov.beta.lambda, 0, 1), ncol =  4, byrow = TRUE),
    meanlog.sigma2 = meanlog.sigma2
   )
}

# return design conditions
return(
  design
)

}


