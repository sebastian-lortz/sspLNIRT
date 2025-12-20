#' Compute MSE of Parameters
#'
#' The function computes the mean squared errors of estimated parameters based on
#' simulated data under the the Joint Hierarchical Model using a 2-pl normal
#' ogive model for response accuracy and a 3-pl log-normal model for response
#' time.
#'
#' @param N Integer. The sample size.
#' @param iter Integer. The number of iteration or the number of data sets.
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
#' @param cor2cov.item Logical. Whether a correlation matrix instead of covariance matrix is supplied
#' @param sd.item Numeric vector. (optional) The standard deviations of alpha, beta, phi, and lambda
#'
#' @return A list of containing:
#' \describe{
#'   \item{mse.theta}{Numeric. The pooled MSE of the theta parameters.}
#'   \item{mse.alpha}{Numeric. The pooled MSE of the alpha paramters.}
#'   \item{mse.beta}{Numeric. The pooled MSE of the beta paramters.}
#'   \item{mse.phi}{Numeric. The pooled MSE of the phi paramters.}
#'   \item{mse.lambda}{Numeric. The pooled MSE of the lambda paramters.}
#'   \item{mse.sigma2}{Numeric. The pooled MSE of the sigma2 paramters.}
#'   \item{conv.rate}{Data Frame. The Rhat convergence rate per iteration (rows)
#'   by parameter blocks (columns).}
#' }
#'
#' @examples
#'  \dontrun{
#' comp_mse(
#'           iter = 6,
#'           N = 500,
#'           I = 100,
#'           mu.person = c(0,0),
#'           mu.item = c(1,0,1,4),
#'           meanlog.sigma2 = log(1),
#'           cov.m.person = matrix(c(1,0,
#'                                   0,1), ncol = 2, byrow = TRUE),
#'           cov.m.item = matrix(c(1, 0, 0, 0,
#'                                 0, 1, 0, 0,
#'                                 0, 0, 1, 0,
#'                                 0, 0, 0, 1), ncol =  4, byrow = TRUE),
#'           sd.item         = c(.2, .5, .2, .5),
#'           cor2cov.item    = TRUE,
#'           sdlog.sigma2 = 0.2
#' }
#' @export
#'


comp_mse <- function(N,
                     iter,
                     I,
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
                     scale = TRUE,
                     random.item = TRUE,
                     item.pars.m = NULL,
                     cor2cov.item = FALSE,
                     sd.item = NULL,
                     XG = 3000,
                     burnin = 20,
                     par1 = TRUE,
                     mse.seed = NULL) {

  if (is.null(mse.seed)) {
    seed = TRUE } else {
      seed = mse.seed
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
                             sd.item = sd.item)

        # i=1
        # fit LNIRT with 4 chains
        fit.list <- list()
        for (f in 1:4) {
          fit.list[[f]] <- LNIRT::LNIRT(
            RT = data$time.data[[1]],
            Y = data$response.data[[1]],
            XG = XG,
            burnin = burnin,
            residual = FALSE,
            par1 = par1
          )}

        # compute r.hat of item parameter samples
        r.hat.out <- rhat_LNIRT(fit.list, chains = 4, cutoff = 1.05)
        r.hat.rates <- unlist(r.hat.out$rate)

        # compute posterior means
        XGburnin <- ceiling(fit.list[[1]]$XG * fit.list[[1]]$burnin / 100)
        post.theta = rowMeans(as.data.frame(lapply(fit.list, FUN = function(x) {
          colMeans(x$MCMC.Samples$Person.Ability[XGburnin:x$XG,])
        })))

        post.zeta = rowMeans(as.data.frame(lapply(fit.list, FUN = function(x) {
          colMeans(x$MCMC.Samples$Person.Speed[XGburnin:x$XG,])
        })))

        post.alpha = rowMeans(as.data.frame(lapply(fit.list, FUN = function(x) {
          (x$Post.Means$Item.Discrimination)
        })))
        post.beta = rowMeans(as.data.frame(lapply(fit.list, FUN = function(x) {
          (x$Post.Means$Item.Difficulty)
        })))
        post.phi = rowMeans(as.data.frame(lapply(fit.list, FUN = function(x) {
          (x$Post.Means$Time.Discrimination)
        })))
        post.lambda = rowMeans(as.data.frame(lapply(fit.list, FUN = function(x) {
          (x$Post.Means$Time.Intensity)
        })))
        post.sigma2 = rowMeans(as.data.frame(lapply(fit.list, FUN = function(x) {
          (x$Post.Means$Sigma2)
        })))

        # re-scale to input scale
        post.item.pars <- cbind(post.alpha, post.beta, post.phi, post.lambda, post.sigma2)
        post.person.pars <- cbind(post.theta, post.zeta)
        re.scaled.post <- scale_M(item.pars = post.item.pars,
                                  person.pars = post.person.pars,
                                  re.scale = TRUE,
                                  c.alpha = data$scale.factor[[1]]$c.alpha,
                                  c.phi = data$scale.factor[[1]]$c.phi)
        re.scaled.data <- scale_M(item.pars = data$item.par[[1]],
                                  person.pars = data$person.par[[1]],
                                  re.scale = TRUE,
                                  c.alpha = data$scale.factor[[1]]$c.alpha,
                                  c.phi = data$scale.factor[[1]]$c.phi)

        # calculate (mean) squared errors on input scale
        res <- list(
          mse.theta = mean((re.scaled.post$person.pars.scaled$post.theta - re.scaled.data$person.pars.scaled$theta)^2),
          mse.zeta = mean((re.scaled.post$person.pars.scaled$post.zeta - re.scaled.data$person.pars.scaled$zeta)^2),
          se.alpha = (re.scaled.post$items.pars.scaled$post.alpha - re.scaled.data$items.pars.scaled$alpha)^2,
          se.beta = (re.scaled.post$items.pars.scaled$post.beta - re.scaled.data$items.pars.scaled$beta)^2,
          se.phi = (re.scaled.post$items.pars.scaled$post.phi - re.scaled.data$items.pars.scaled$phi)^2,
          se.lambda = (re.scaled.post$items.pars.scaled$post.lambda - re.scaled.data$items.pars.scaled$lambda)^2,
          se.sigma2 = (re.scaled.post$items.pars.scaled$post.sigma2 - re.scaled.data$items.pars.scaled$log.sigma2)^2,
          conv.rate = r.hat.rates
        )

        # empty memory
        rm(fit.list,
           post.theta, post.zeta,
           post.alpha, post.beta, post.phi, post.lambda, post.sigma2,
           post.item.pars, post.person.pars,
           re.scaled.post, re.scaled.data)
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
        TRUE,  # <- do automatic detection
        add = c(
          # but ALWAYS also ship these helpers:
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

  # take mean across iterations and items/persons
  mse.theta = list(mse.theta = mean(unlist(lapply(out[!is.na(out)], FUN = function(x) {
    x$mse.theta
  }))))
  mse.zeta= list(mse.zeta = mean(unlist(lapply(out[!is.na(out)], FUN = function(x) {
    x$mse.zeta
  }))))
  mse.person = append(mse.theta, mse.zeta)
  mse.items =
    colMeans(apply(
      simplify2array(
        lapply(out[!is.na(out)], FUN = function(x) {
          cbind(mse.alpha = x$se.alpha,
                mse.beta = x$se.beta,
                mse.phi = x$se.phi,
                mse.lambda = x$se.lambda,
                mse.sigma2 = x$se.sigma2)
        })), MARGIN = c(1,2), FUN = function(x) {
          mean(x, na.rm = TRUE)
        }))

  # sd of MSE over replications
  mc.sd.items =
    apply(apply(
      simplify2array(
        lapply(out[!is.na(out)], FUN = function(x) {
          cbind(mse.alpha = x$se.alpha,
                mse.beta = x$se.beta,
                mse.phi = x$se.phi,
                mse.lambda = x$se.lambda,
                mse.sigma2 = x$se.sigma2)
        })), MARGIN = c(3,2), FUN = function(x) {
          mean(x, na.rm = TRUE)
        }), 2, sd)

  # store convergence rates
  conv.rate = as.data.frame(t(sapply(out[!is.na(out)], FUN = function(x) {
    x$conv.rate
  })))

  # empty memory
  rm(out)
  gc()

  # return output
  return(
    c(mse.person,
      as.list(mse.items),
      list(mc.se.items = mc.se.items),
      list(conv.rate = conv.rate))
  )

}


