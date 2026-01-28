#' Optimize for Sample Size
#'
#' The function optimizes for the minimum sample size to reach the threshold of root mean
#' squared errors (RMSE) of estimated parameters. MSE are based on simulated data
#' under the Joint Hierarchical Model using a 2-pl normal ogive model for
#' response accuracy and a 3-pl log-normal model for response time.
#'
#' @param FUN Function. The function to calculate the target parameter (default: `comp_rmse`).
#' @param thresh Numeric. The desired RMSE threshold of the target parameter to be achieved.
#' @param range Vector. Integer vector of length 2 specifying the lower and upper bounds of the sample size.
#' @param out.par Character. The name of the target parameter for the threshold.
#' @param iter Integer. The number of iterations or the number of data sets.
#' @param K Integer. The test length.
#' @param mu.person Numeric vector. Means of theta and zeta.
#' @param mu.item Numeric vector. Means of alpha, beta, phi, and lambda.
#' @param meanlog.sigma2 Numeric. The meanlog of sigma2.
#' @param cov.m.person Matrix. The covariance matrix of theta and zeta.
#' @param cov.m.item Matrix. The covariance matrix of alpha, beta, phi, and lambda.
#' @param sdlog.sigma2 Numeric. The sdlog of sigma2.
#' @param item.pars.m Matrix. (optional) A matrix containing item parameters.
#' @param cor2cov.item Logical. Whether a correlation matrix instead of covariance matrix is supplied.
#' @param sd.item Numeric vector. (optional) The standard deviations of alpha, beta, phi, and lambda.
#' @param keep.err.dat Logical. Whether to keep the full error data.
#' @param ssp.seed Integer or NULL. Seed for reproducibility.
#' @param XG Integer. The number of Gibbs sampler iterations.
#' @param burnin Integer. The burn-in percentage.
#' @param rhat Numeric. The R-hat convergence cutoff.
#'
#' @return A list of class `sspLNIRT.object` containing:
#' \describe{
#'   \item{N.best}{Integer (or character if bounds not met). The minimum sample size achieving the threshold.}
#'   \item{res.best}{Numeric. The RMSE result at the optimal N.}
#'   \item{comp.mse}{List. The full output from `comp_rmse` at the optimal N.}
#'   \item{trace}{List containing optimization diagnostics: `steps` (integer), `track.res` (data frame), `track.N` (data frame), and `time.taken` (difftime).}
#' }
#'
#' @examples
#' \dontrun{
#' test.optim.sample <- optim_sample(
#'   FUN = comp_rmse,
#'   thresh = .1,
#'   range = c(100, 500),
#'   out.par = "alpha",
#'   iter = 5,
#'   K = 10,
#'   mu.person = c(0, 0),
#'   mu.item = c(1, 0, 1, 0),
#'   meanlog.sigma2 = log(.3),
#'   cov.m.person = matrix(c(1, 0.5, 0.5, 1), ncol = 2, byrow = TRUE),
#'   cov.m.item = matrix(c(1, 0, 0, 0,
#'                         0, 1, 0, 0.3,
#'                         0, 0, 1, 0,
#'                         0, 0.3, 0, 1), ncol = 4, byrow = TRUE),
#'   sd.item = c(.2, .5, .2, .5),
#'   cor2cov.item = TRUE,
#'   sdlog.sigma2 = 0.2,
#'   XG = 1000
#' )
#' }
#' @export
#'
optim_sample <- function(FUN = comp_rmse,
                         thresh,
                         range,
                         out.par = 'alpha',
                         iter = 100,
                         K = 10,
                         mu.person = c(0,0),
                         mu.item = c(1,0,1,1),
                         meanlog.sigma2 = log(.6),
                         cov.m.person = matrix(c(1,.2,
                                                 .2,1), ncol = 2, byrow = TRUE),
                         cov.m.item = matrix(c(.2, 0, 0, 0,
                                               0, 1, 0, .4,
                                               0, 0, .2, 0,
                                               0, .4, 0, .5), ncol =  4, byrow = TRUE),
                         sdlog.sigma2 = 0.2,
                         item.pars.m = NULL,
                         cor2cov.item = FALSE,
                         sd.item = NULL,
                         keep.err.dat = TRUE,
                         ssp.seed = NULL,
                         XG = 6000,
                         burnin = 20,
                         rhat = 1.05
) {

  # start time
  start.time = Sys.time()

  # get range
  lb = range[1]
  ub = range[2]

  # helper: compute FUN value given new N
  compute_obj <- function(newN) {

    FUN.out <- FUN(
      N = newN,
      iter = iter,
      K = K,
      mu.person = mu.person,
      mu.item = mu.item,
      meanlog.sigma2 = meanlog.sigma2,
      cov.m.person = cov.m.person,
      cov.m.item = cov.m.item,
      sdlog.sigma2 = sdlog.sigma2,
      item.pars.m = item.pars.m,
      cor2cov.item = cor2cov.item,
      sd.item = sd.item,
      keep.err.dat = keep.err.dat,
      XG = XG,
      burnin = burnin,
      rhat = rhat,
      mse.seed = ssp.seed
    )

    return(
      list(
        res = FUN.out$item$rmse[[out.par]],
        mc.sd = FUN.out$item$mc.sd.rmse[[out.par]],
        comp.mse = FUN.out)
    )
  }

  # compute N for lower bound
  res.lb <- compute_obj(newN = lb)
  cat("LB result is", res.lb$res, "\n")

  # check lower bound result
  if (res.lb$res < thresh) {
    cat("stop due to res.lb < thresh with", lb, "\n")

    # track time
    end.time = Sys.time()
    time.taken = end.time - start.time

    output <- list(N.best = "res.lb < thresh",
                res.best = res.lb$res,
                comp.mse = res.lb$comp.mse,
                trace = list(steps = 1,
                             track.res = data.frame(res.lb = res.lb$res,
                                                    res.ub = NA,
                                                    res.temp = res.lb$res,
                                                    mc.sd = c(res.lb$mc.sd)),
                             track.N = data.frame(N.lb = rep(lb),
                                                  N.ub = rep(ub),
                                                  N.temp = "res.lb < thresh"),
                             time.taken = time.taken))

    class(output) <- "sspLNIRT.object"
    return(output)
    }

  # compute N for upper bound
  res.ub <- compute_obj(newN = ub)
  cat("UB result is", res.ub$res, "\n")

  # check upper bound result
  if (res.ub$res > thresh) {
    cat("stop due to res.ub > thresh with", ub, "\n")

    # track time
    end.time = Sys.time()
    time.taken = end.time - start.time

    output <- list(N.best = "res.ub > thresh",
                res.best = res.ub$res,
                comp.mse = res.ub$comp.mse,
                trace = list(steps = 2,
                             track.res = data.frame(res.lb = res.lb$res,
                                                    res.ub = res.ub$res,
                                                    res.temp = c(res.lb$res, res.ub$res),
                                                    mc.sd = c(res.lb$mc.sd, res.ub$mc.sd)),
                             track.N = data.frame(N.lb = rep(lb, 2),
                                                  N.ub = rep(ub, 2),
                                                  N.temp = c(lb, ub)),
                             time.taken = time.taken))

    class(output) <- "sspLNIRT.object"
    return(output)
    }

  # track N and resulting output parameter
  track.N <- data.frame(N.lb = rep(lb, 2),
                        N.ub = rep(ub, 2),
                        N.temp = c(lb, ub))
  track.res <- data.frame(res.lb = res.lb$res,
                          res.ub = res.ub$res,
                          res.temp = c(res.lb$res, res.ub$res),
                          mc.sd = c(res.lb$mc.sd, res.ub$mc.sd))

  ## N Optimizer
  # initialize
  res.temp = res.lb
  N.lb = lb
  N.temp = N.ub = ub
  steps = 2

  # start routine
  repeat {

    # compute increment, stop if < 1
    inc <- (N.ub - N.lb)/2
    if (inc < 1) {
      cat("stop due to inc", inc, "with", N.temp, "\n")
      break
    }

    # check upper bound result
    if (res.ub$res > thresh) {
      cat("stop due to res.ub > thresh with", N.temp, "\n")
      break
    }

    # check lower bound result
    if (res.lb$res < thresh) {
      cat("stop due to res.lb < thresh with", N.temp, "\n")
      break
    }

    # move N.temp up or down
    if (res.temp$res < thresh) {
      N.temp <- ceiling(N.lb + inc)
    } else {
      N.temp <- ceiling(N.ub - inc)
    }

    # check N-change
    if (N.temp == track.N$N.temp[steps-1]) {
      cat("stop due to N.temp = N.temp with", N.temp, "\n")
      break
    }
    cat("N temp is", N.temp, "\n")

    # compute result
    res.temp <- compute_obj(newN = N.temp)

    # set new bounds
    if (res.temp$res < thresh) {
      N.ub <- N.temp
      res.ub <- res.temp
    } else {
      N.lb <- N.temp
      res.lb <- res.temp
    }

    # track results
    steps <- steps + 1
    track.res[steps,] <- c(res.lb$res, res.ub$res, res.temp$res, res.temp$mc.sd)
    track.N[steps, ] <- c(N.lb, N.ub, N.temp)
    cat("New result is", c(res.lb$res, res.ub$res), "\n")

  }
  # end routine

  # assemble output
  N.best  <- N.ub
  res.best <- res.ub$res
  cat("Best result is", res.best," for threshold", thresh, "\n")
  cat("Minimum N is", N.best, "\n")

  # track time
  end.time = Sys.time()
  time.taken = end.time - start.time

  # return output
  output <- list(N.best = N.best,
                 res.best = res.best,
                 comp.mse = res.ub$comp.mse,
                  trace = list(steps = steps,
                               track.res = track.res,
                               track.N = track.N,
                               time.taken = time.taken)
              )
  class(output) <- "sspLNIRT.object"
  return(output)
}


