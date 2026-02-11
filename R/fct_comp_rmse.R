#' Compute RMSE of Parameters
#'
#' The function computes the mean squared errors of estimated parameters based on
#' simulated data under the Joint Hierarchical Model using a 2-pl normal
#' ogive model for response accuracy and a 3-pl log-normal model for response
#' time.
#'
#' @param N Integer. The sample size.
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
#' @param XG Integer. The number of Gibbs sampler iterations.
#' @param burnin Integer. The burn-in percentage.
#' @param seed Integer or NULL. Seed for reproducibility.
#' @param keep.err.dat Logical. Whether to keep the full error data.
#' @param rhat Numeric. The R-hat convergence cutoff.
#'
#' @return A list of class `sspLNIRT.object` containing:
#' \describe{
#'   \item{person}{List with `rmse` (named vector), `mc.sd.rmse` (named vector), and `bias` (named vector) for theta and zeta.}
#'   \item{item}{List with `rmse` (named vector), `mc.sd.rmse` (named vector), and `bias` (named vector) for alpha, beta, phi, lambda, and sigma2.}
#'   \item{conv.rate}{Numeric. Proportion of iterations that converged (n converged / iter).}
#'   \item{err.dat}{List with `person` and `item` data frames containing per-replication errors (if `keep.err.dat = TRUE`).}
#' }
#'
#' @examples
#' \dontrun{
#' test <- comp_rmse(
#'   iter = 5,
#'   N = 100,
#'   K = 10,
#'   mu.person = c(0, 0),
#'   mu.item = c(1, 0, 1, 1),
#'   meanlog.sigma2 = log(1),
#'   cov.m.person = matrix(c(1, 0, 0, 1), ncol = 2, byrow = TRUE),
#'   cov.m.item = diag(4),
#'   sd.item = c(.2, 1, .2, .5),
#'   cor2cov.item = TRUE,
#'   sdlog.sigma2 = 0.2,
#'   XG = 2000,
#'   keep.err.dat = FALSE,
#'   rhat = 1.05
#' )
#' summary(test)
#' }
#' @export
comp_rmse <- function(N,
                     iter,
                     K,
                     mu.person = c(0,0),
                     mu.item = c(1,0,1,1),
                     meanlog.sigma2 = log(.2),
                     cov.m.person = matrix(c(1,.5,
                                             .5,1), ncol = 2, byrow = TRUE),
                     cov.m.item = matrix(c(.2, 0, 0, 0,
                                           0, .5, -.35, -.15,
                                           0, -.35, .5, .15,
                                           0, -.15, .15, .2), ncol =  4, byrow = TRUE),
                     sdlog.sigma2 = 0.2,
                     item.pars.m = NULL,
                     cor2cov.item = FALSE,
                     sd.item = NULL,
                     XG = 3000,
                     burnin = 20,
                     seed = NULL,
                     keep.err.dat = FALSE,
                     rhat = 1.05) {

  if (is.null(seed)) {
    seed = TRUE } else {
      seed = seed
    }

  # set progressr
  handler <-list(progressr::handler_txtprogressbar())

  # run in parallel with progress
  out <- progressr::with_progress({
    p <- progressr::progressor(steps = iter)

    future.apply::future_lapply(
      X = seq_len(iter),
      FUN = function(i) {

        # simulate data
        data <- sim.jhm.data(iter = 1,
                             N = N,
                             K = K ,
                             mu.person = mu.person,
                             mu.item = mu.item,
                             meanlog.sigma2 = meanlog.sigma2,
                             cov.m.person = cov.m.person,
                             cov.m.item = cov.m.item,
                             sdlog.sigma2 = sdlog.sigma2,
                             item.pars.m = item.pars.m,
                             cor2cov.item = cor2cov.item,
                             sd.item = sd.item)

        RT <- data$time.data[[1]]
        Y  <- data$response.data[[1]]
        item.par   <- data$item.par[[1]]
        person.par <- data$person.par[[1]]
        scale.factor <- data$scale.factor[[1]]

        rm(data); gc()

        # i=1
        # fit LNIRT with 4 chains
        fit.list <- vector("list", 4)
        for (f in 1:4) {
          fit.list[[f]] <- LNIRT::LNIRT(
            RT = RT,
            Y = Y,
            XG = XG,
            burnin = burnin,
            residual = FALSE,
            par1 = TRUE
          )}

        # compute r.hat of item parameter samples
        r.hat.out <- rhat_LNIRT(fit.list, chains = 4, cutoff = rhat)
        r.hat.rates <- unlist(r.hat.out$rate)

        # compute posterior means
        XGburnin <- ceiling(fit.list[[1]]$XG * fit.list[[1]]$burnin / 100)
        idx <- XGburnin:fit.list[[1]]$XG
        nchains <- length(fit.list)

        post.theta <- Reduce(`+`, lapply(fit.list, \(x) colMeans(x$MCMC.Samples$Person.Ability[idx, , drop=FALSE]))) / nchains
        post.zeta  <- Reduce(`+`, lapply(fit.list, \(x) colMeans(x$MCMC.Samples$Person.Speed[idx, , drop=FALSE])))   / nchains

        post.alpha  <- Reduce(`+`, lapply(fit.list, \(x) x$Post.Means$Item.Discrimination)) / nchains
        post.beta   <- Reduce(`+`, lapply(fit.list, \(x) x$Post.Means$Item.Difficulty))      / nchains
        post.phi    <- Reduce(`+`, lapply(fit.list, \(x) x$Post.Means$Time.Discrimination))  / nchains
        post.lambda <- Reduce(`+`, lapply(fit.list, \(x) x$Post.Means$Time.Intensity))       / nchains
        post.sigma2 <- Reduce(`+`, lapply(fit.list, \(x) x$Post.Means$Sigma2))               / nchains

        # drop objects in workers
        for (f in seq_along(fit.list)) fit.list[[f]]$MCMC.Samples <- NULL
        rm(fit.list, RT, Y)
        gc()

        # re-scale to input scale
        post.item.pars <- cbind(post.alpha, post.beta, post.phi, post.lambda, post.sigma2)
        post.person.pars <- cbind(post.theta, post.zeta)
        re.scaled.post <- scale_M(item.pars = post.item.pars,
                                  person.pars = post.person.pars,
                                  re.scale = TRUE,
                                  c.alpha = scale.factor$c.alpha,
                                  c.phi = scale.factor$c.phi)
        re.scaled.data <- scale_M(item.pars = item.par,
                                  person.pars = person.par,
                                  re.scale = TRUE,
                                  c.alpha = scale.factor$c.alpha,
                                  c.phi = scale.factor$c.phi)

        # calculate (mean squared) errors on input scale
        res <- list(
          err.theta = (re.scaled.post$person.pars.scaled$post.theta - re.scaled.data$person.pars.scaled$theta),
          err.zeta = (re.scaled.post$person.pars.scaled$post.zeta - re.scaled.data$person.pars.scaled$zeta),
          err.alpha = (re.scaled.post$items.pars.scaled$post.alpha - re.scaled.data$items.pars.scaled$alpha),
          err.beta = (re.scaled.post$items.pars.scaled$post.beta - re.scaled.data$items.pars.scaled$beta),
          err.phi = (re.scaled.post$items.pars.scaled$post.phi - re.scaled.data$items.pars.scaled$phi),
          err.lambda = (re.scaled.post$items.pars.scaled$post.lambda - re.scaled.data$items.pars.scaled$lambda),
          err.sigma2 = (re.scaled.post$items.pars.scaled$post.sigma2 - re.scaled.data$items.pars.scaled$sigma2),
          conv.rate = r.hat.rates
        )

          res$sim.alpha  <- re.scaled.data$items.pars.scaled$alpha
          res$sim.beta   <- re.scaled.data$items.pars.scaled$beta
          res$sim.phi    <- re.scaled.data$items.pars.scaled$phi
          res$sim.lambda <- re.scaled.data$items.pars.scaled$lambda
          res$sim.sigma2 <- re.scaled.data$items.pars.scaled$sigma2
          res$sim.theta  <- re.scaled.data$person.pars.scaled$theta
          res$sim.zeta   <- re.scaled.data$person.pars.scaled$zeta

        # empty memory
        rm(post.theta, post.zeta, post.alpha, post.beta, post.phi, post.lambda, post.sigma2,
           post.item.pars, post.person.pars, re.scaled.post, re.scaled.data,
           item.par, person.par, scale.factor)
        gc()

        # track and return
        p()
        if (r.hat.rates[2] == 1) {
          return(res)
        } else {
          return(NA)
        }
      },
      future.seed = seed,
      future.stdout = FALSE,
      future.packages = c("LNIRT"),
      future.globals = structure(
        TRUE,  # automatic detection
        add = c(
          # ship these helpers:
          "sim.jhm.data",
          "person.par",
          "item.par",
          "scale_M",
          "rhat_LNIRT"
        )
      )
    )
  },
  handlers = handler)

  # keep non-NA replications
  keep <- vapply(out, function(x) !(length(x) == 1 && is.na(x)), logical(1))
  out  <- out[keep]

  # error array person x parameter x replication
  err.person <- simplify2array(lapply(out, function(x) {
    cbind(
      theta  = x$err.theta,
      zeta   = x$err.zeta
    )
  }))

  # pooled bias per parameter
  bias.person <- apply(err.person, 2, mean, na.rm = TRUE)

  # pooled RMSE per replication
  rmse.rep.person <- sqrt(apply(err.person^2, c(3, 2), mean, na.rm = TRUE))

  # mean and MC sd
  rmse.person <- apply(rmse.rep.person, 2, mean, na.rm = TRUE)
  mc.sd.person  <- apply(rmse.rep.person, 2, sd, na.rm = TRUE)

  person = list(
    rmse = rmse.person,
    mc.sd.rmse = mc.sd.person,
    bias = bias.person
  )

  # error array item x parameter x replication
  err.item <- simplify2array(lapply(out, function(x) {
    cbind(
      alpha  = x$err.alpha,
      beta   = x$err.beta,
      phi    = x$err.phi,
      lambda = x$err.lambda,
      sigma2 = x$err.sigma2
    )
  }))

  # pooled bias per parameter
  bias.item <- apply(err.item, 2, mean, na.rm = TRUE)

  # pooled RMSE per replication
  rmse.rep.item <- sqrt(apply(err.item^2, c(3, 2), mean, na.rm = TRUE))

  # mean and MC sd
  rmse.item <- apply(rmse.rep.item, 2, mean, na.rm = TRUE)
  mc.sd.item  <- apply(rmse.rep.item, 2, sd, na.rm = TRUE)

  # compile item
  item = list(
    rmse = rmse.item,
    mc.sd.rmse = mc.sd.item,
    bias = bias.item
  )

  # full error data
  err.item <- do.call(rbind, Map(function(x, r) {

    data.frame(
      rep     = r,
      par     = rep(c("alpha","beta","phi","lambda","sigma2"),
                    times = c(length(x$err.alpha),
                              length(x$err.beta),
                              length(x$err.phi),
                              length(x$err.lambda),
                              length(x$err.sigma2))),
      sim.val = c(x$sim.alpha, x$sim.beta, x$sim.phi, x$sim.lambda, x$sim.sigma2),
      err     = c(x$err.alpha, x$err.beta, x$err.phi, x$err.lambda, x$err.sigma2),
      stringsAsFactors = FALSE
    )

  }, out, seq_along(out)))

  err.person <- do.call(rbind, Map(function(x, r) {

    data.frame(
      rep     = r,
      par     = rep(c("theta","zeta"),
                    times = c(length(x$err.theta),
                              length(x$err.zeta))),
      sim.val = c(x$sim.theta, x$sim.zeta),
      err     = c(x$err.theta, x$err.zeta),
      stringsAsFactors = FALSE
    )

  }, out, seq_along(out)))

# binning error data
  if(!keep.err.dat) {
    n.bins <- 30

    ntile_base <- function(x, n) {
      floor((rank(x, ties.method = "first") - 1) * n / length(x)) + 1L
    }

    # helper: summarize by par and bin
    bin_summarise <- function(df, n.bins) {
      do.call(rbind, lapply(split(df, df$par), function(d) {
        d$bin <- ntile_base(d$sim.val, n.bins)
        do.call(rbind, lapply(split(d, d$bin), function(b) {
          data.frame(
            par       = b$par[1],
            bin       = b$bin[1],
            mean_sim  = mean(b$sim.val, na.rm = TRUE),
            mean_err  = mean(b$err, na.rm = TRUE),
            mean_rmse = sqrt(mean(b$err^2, na.rm = TRUE))
          )
        }))
      }))
    }

    err.item <- bin_summarise(err.item, n.bins)
    rownames(err.item) <- NULL

    err.person <- bin_summarise(err.person, n.bins)
    rownames(err.person) <- NULL
  }

  # convergence rates
  conv.rate <- length(out) / iter

  # empty memory
  rm(out)
  gc()

  # return output
  output <- list(
    person = person,
    item = item,
    conv.rate = conv.rate,
    err.dat = list(person = err.person,
                   item = err.item)
  )
  class(output) <- "sspLNIRT.object"
  return(output)
}

