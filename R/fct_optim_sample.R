#' Optimize for Sample Size
#'
#' The function optimizes for the minimum sample size to reach the threshold of mean
#' squared errors (MSE) of estimated parameters. MSE are based on simulated data
#' under the the Joint Hierarchical Model using a 2-pl normal ogive model for
#' response accuracy and a 3-pl log-normal model for response time.
#'
#' @param FUN Function. The function to calculate the target parameter.
#' @param thresh Numeric. The desired threshold of the target parameter to be achieved.
#' @param lb Integer. The lower bound of the sample size.
#' @param ub Integer. The upper bound of the sample size.
#' @param out.par Character. The name of the target parameter for the threshold.
#' @param N NULL. Place holder for the sample size in the optimizer.
#' @param iter Integer. The number of iteration or the number of data sets.
#' @param I Integer. The test length.
#' @param mu.person Numeric vector. Means of theta and zeta.
#' @param mu.item Numeric vector. Means of alpha, beta, phi, and lambda.
#' @param meanlog.sigma2 Numeric. The meanlog of sigma2.
#' @param cov.m.person Matrix. The covariance matrix of theat and zeta.
#' @param cov.m.item Matrix. The covariance matrix of of alpha, beta, phi, and lambda.
#' @param sdlog.sigma2 Numeric. The sdlog of sigma2.
#' @param scale Logical. Weather the item and person parameters are scaled.
#' @param random.item Logical. Weather the item parameters are sampled.
#' @param item.pars.m Matrix. (optional) A Matrix containing item parameters.
#' @param cor2cov.item Logical. Whether a correlation matrix instead of covariance matrix is supplied
#' @param sd.item Numeric vector. (optional) The standard deviations of alpha, beta, phi, and lambda
#'
#' @return A list of containing:
#' \describe{
#'   \item{N.best}{Integer. The minimum sample size for the closest result to the threshold.}
#'   \item{res.best}{Numeric. The result of the target parameter closest to the threshold.}
#'   \item{reps}{Integer. Number of optimization repetitions.}
#'   \item{track.res}{Data Frame. The results around the threshold per repetition.}
#'   \item{track.N}{Data Frame. The sample size in each repetition.}
#'   \item{mse.sigma2}{Numeric. The pooled MSE of the sigma2 paramters.}
#'   \item{track.conv}{List. The Rhat convergence rate per repetition.}
#'   \item{time.taken}{Numeric. The observed running time of the function.}
#' }
#'
#' @examples
#'  \dontrun{
#' optim_sample(
#'     FUN = comp_mse,
#'     thresh = .01,
#'     lb = 100,
#'     ub = 500,
#'     out.par = 'alpha',
#'     iter = 5,
#'     I = 10,
#'     mu.person = c(0,0),
#'     mu.item = c(1,0,1,0),
#'     meanlog.sigma2 = log(.3),
#'     cov.m.person = matrix(c(1,0.5,
#'                             0.5,1), ncol = 2, byrow = TRUE),
#'                             cov.m.item = matrix(c(1, 0, 0, 0,
#'                                                   0, 1, 0, 0.3,
#'                                                   0, 0, 1, 0,
#'                                                   0, 0.3, 0, 1), ncol =  4, byrow = TRUE),
#'     sd.item         = c(.2, .5, .2, .5),
#'     cor2cov.item    = TRUE,
#'     sdlog.sigma2 = 0.2,
#'     XG = 1000)
#' }
#' @export
#'

optim_sample <- function(FUN = comp_mse,
                         thresh,
                         lb,
                         ub,
                         out.par = 'alpha',
                         N = NULL,
                         iter = 1,
                         I = 20,
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
                         cor2cov.item = FALSE,
                         sd.item = NULL,
                         ssp.seed = NULL,
                         XG = 6000
) {

  # start time
  start.time = Sys.time()

  # helper: compute FUN value given new N
  compute_obj <- function(newN) {

    FUN.out <- FUN(
      N = newN,
      iter = iter,
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
      cor2cov.item = cor2cov.item,
      sd.item = sd.item,
      XG = XG,
      mse.seed = ssp.seed
    )

    return(
      list(
        res = FUN.out$mse.items[[out.par]],
        mc.sd = FUN.out$mc.sd.mse[[out.par]],
        conv.rate = FUN.out$conv.rate,
        mse.items = FUN.out$mse.items,
        bias.items = FUN.out$bias.items,
        var.items  = FUN.out$var.items,
        err.dat    = FUN.out$err.dat)
    )
  }

  # compute N for lower bound
  res.lb <- compute_obj(newN = lb)
  cat("LB result is", res.lb$res, "\n")

  # check lower bound result
  if (res.lb$res < thresh) {
    cat("stop due to res.lb < thresh with", lb, "\n")
    return(list(N.best = NA,
                res.best = "res.lb < thresh",
                reps = 1,
                track.res = data.frame(res.lb = "res.lb < thresh",
                                       res.ub = NA,
                                       res.temp = c("res.lb < thresh"),
                                       mc.sd = c(res.lb$mc.sd)),
                track.N = data.frame(N.lb = rep(lb),
                                     N.ub = rep(ub),
                                     N.temp = NA),
                track.conv = list(res.lb$conv.rate),
                mse.items = res.lb$mse.items,
                bias.items = res.lb$bias.items,
                var.items  = res.lb$var.items,
                err.dat    = res.lb$err.dat
                ))}

  # compute N for upper bound
  res.ub <- compute_obj(newN = ub)
  cat("UB result is", res.ub$res, "\n")

  # check upper bound result
  if (res.ub$res > thresh) {
    cat("stop due to res.ub > thresh with", ub, "\n")
    return(list(N.best = NA,
                res.best = "res.ub > thresh",
                reps = 2,
                track.res = data.frame(res.lb = res.lb$res,
                                       res.ub = "res.ub > thresh",
                                       res.temp = c(res.lb$res, "res.ub > thresh"),
                                       mc.sd = c(res.lb$mc.sd, res.ub$mc.sd)),
                track.N = data.frame(N.lb = rep(lb, 2),
                                     N.ub = rep(ub, 2),
                                     N.temp = c(lb, ub)),
                track.conv = list(res.lb$conv.rate,
                                  res.ub$conv.rate),
                mse.items = res.ub$mse.items,
                bias.items = res.ub$bias.items,
                var.items  = res.ub$var.items,
                err.dat    = res.ub$err.dat
                ))}

  # track N and resulting output parameter
  track.N <- data.frame(N.lb = rep(lb, 2),
                        N.ub = rep(ub, 2),
                        N.temp = c(lb, ub))
  track.res <- data.frame(res.lb = res.lb$res,
                          res.ub = res.ub$res,
                          res.temp = c(res.lb$res, res.ub$res),
                          mc.sd = c(res.lb$mc.sd, res.ub$mc.sd))
  track.conv <- list(res.lb$conv.rate,
                     res.ub$conv.rate)

  ## N Optimizer
  # initialize
  res.temp = res.lb
  N.lb = lb
  N.temp = N.ub = ub
  reps = 2

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
    if (N.temp == track.N$N.temp[reps-1]) {
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
    reps <- reps + 1
    track.res[reps,] <- c(res.lb$res, res.ub$res, res.temp$res, res.temp$mc.sd)
    track.N[reps, ] <- c(N.lb, N.ub, N.temp)
    track.conv[[reps]] <- res.temp$conv.rate
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
  return(list(N.best = N.best,
              res.best = res.best,
              reps = reps,
              track.res = track.res,
              track.N = track.N,
              track.conv = track.conv,
              time.taken = time.taken,
              mse.items = res.ub$mse.items,
              bias.items = res.ub$bias.items,
              var.items  = res.ub$var.items,
              err.dat    = res.ub$err.dat
              ))
}
