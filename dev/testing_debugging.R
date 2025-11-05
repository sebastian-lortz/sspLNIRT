#' Title: Minimum Sample Size Estimation for JHM | File: <script_name>.R
#' Purpose: <one-liner>
#' Author: Sebastian A. J. Lortz, University of Groningen
#' Contact: <s.a.j.lortz@gmail.com>
#' Date: <YYYY-MM-DD> | Version: 0.1.0 | License: <MIT/GPL-3/CC-BY-4.0>
#' Reproducibility: R >= <4.4.2>; pkgs: ggplot2, ...
#' Inputs: <path/desc>; Outputs: <path/desc>
#' How to run: Rscript <script_name>.R
#' SPDX-License-Identifier: MIT
#
#rm(list = ls())
R.version
# dependencies ------------------------------------------------------------
pkg <- function(pkgs, attach = TRUE, repos = "https://cloud.r-project.org") {
  pkgs <- unique(pkgs[nzchar(pkgs)])
  miss <- setdiff(pkgs, rownames(installed.packages()))
  if (length(miss)) install.packages(miss, repos = repos, dependencies = TRUE)
  loader <- if (attach) function(p) suppressPackageStartupMessages(
    require(p, character.only = TRUE, quietly = FALSE, warn.conflicts = TRUE)
  ) else function(p) requireNamespace(p, quietly = FALSE)
  invisible(sapply(pkgs, loader))
}
pkgs <- c(
  "tidyverse",
  "MASS",
  "LNIRT",
  "lavaan",
  "tmvtnorm"
)
pkg(pkgs)



# Generate Item Parameters ------------------------------------------------

item.par <- function(I,
                     mu.item = c(1,0,4,0),
                     cov.m.item = matrix(c(.2, 0, 0, 0,
                                           0, .5, -.35, -.15,
                                           0, -.35, .5, .15,
                                           0, -.15, .15, .2), ncol =  4, byrow = TRUE),
                     meanlog.sigma2 = log(.3),
                     sdlog.sigma2 = 0.2,
                     cor2cov.item = FALSE,
                     sd.item = NULL,
                     item.seed = NULL
) {

  # sample sigma2 from lognormal distribution
  log.sigma2 <- rlnorm(I, meanlog = meanlog.sigma2, sdlog = sdlog.sigma2)

  # if correlation matrix is supplied
  if (cor2cov.item) {
    cov.m.item <- lavaan::cor2cov(cov.m.item , sds = sd.item)
  }

  # sample alpha, beta, phi, lambda
  set.seed(item.seed)
  item.pars <- tmvtnorm::rtmvnorm(n = I,
                                  mean = mu.item,
                                  sigma = cov.m.item,
                                  lower = c(0, -Inf, -Inf, -Inf),
                                  upper = c(Inf, Inf, Inf, Inf))
  colnames(item.pars) <- c("alpha", "beta", "phi", "lambda")
  set.seed(NULL)

  # combine item parameters
  pars.out <- as.data.frame(
    cbind(item.pars, log.sigma2)
  )

  # return output
  return(
    pars.out
  )
}



# Generate Person Parameters ----------------------------------------------

person.par <- function(N,
                       mu.person = c(0,0),
                       cov.m.person = matrix(c(1,.5,
                                               .5,1), ncol = 2, byrow = TRUE)
) {

  # sample person parameters from MVN
  hyper.par = as.data.frame(
    mvrnorm(N,
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


# Scale Parameters --------------------------------------------------------

scale_M <- function(item.pars,
                    person.pars) {

  # I and N
  I = nrow(item.pars)
  N = nrow(person.pars)

  # scaling factor
  c.alpha <- prod(item.pars$alpha)^(1/I)
  c.phi <- prod(item.pars$phi)^(1/I)

  # scaling vectors
  c.items <- matrix(rep(c(1/c.alpha, c.alpha, 1/c.phi, c.phi, 1), I), ncol = 5, byrow = TRUE)
  c.persons <- matrix(rep(c(c.alpha, c.phi), N), ncol = 2, byrow = TRUE)

  # scale matrices
  items.pars.scaled <- item.pars * c.items
  person.pars.scaled <- person.pars * c.persons

  # return output
  return(
    list(
      items.pars.scaled = items.pars.scaled,
      person.pars.scaled = person.pars.scaled,
      c.alpha = c.alpha,
      c.phi = c.phi
    )
  )
}



# rhat Convergence ------------------------------------------------------

rhat_LNIRT <- function(object.list, chains = 4, cutoff = 1.01) {
  stopifnot(length(object.list) >= 2)
  # sanity: same blocks everywhere
  blocks <- Reduce(intersect, lapply(object.list, function(f) names(f$MCMC.Samples)))
  XG = object.list[[1]]$XG
  burn = ceiling(object.list[[1]]$burnin/100 * XG)

  D1 <-  c(
    "Mu.Person.Ability",
    "Mu.Person.Speed",
    "Var.Person.Ability",
    "Var.Person.Speed",
    "Cov.Person.Ability.Speed",
    "Mu.Item.Discrimination",
    "Mu.Item.Difficulty",
    "Mu.Time.Discrimination",
    "Mu.Time.Intensity")
  D2.item <- c(
    "Item.Discrimination",
    "Item.Difficulty",
    "Time.Discrimination",
    "Time.Intensity",
    "Sigma2")
  D2.person <- c(
    "Person.Ability",
    "Person.Speed")
  D2 <- c(D2.item, D2.person)
  D3 <- c(
    "CovMat.Item"
  )

  mcmc.samples <- lapply(object.list, FUN = function(x) {
    x$MCMC.Samples })

  D1.chains <- transpose(lapply(mcmc.samples, FUN = function(x) {
    x[D1]
  }))

  D1.mcmc <- lapply(D1.chains, function(x) {
    m <- do.call(cbind, x)
    m[burn:XG,]
  })
  D1.r.hat <- lapply(D1.mcmc, posterior::rhat)

  D2.item.chains <- transpose(lapply(mcmc.samples, FUN = function(x) {
    x[D2.item]
  }))

  D2.item.mcmc <- lapply(D2.item.chains, function(x) {
    do.call(rbind, x)
  })

  D2.item.r.hat <- lapply(D2.item.mcmc, FUN = function(x) {
    apply(x, 2, FUN = function(y) {
      posterior::rhat(matrix(y, ncol = chains)[burn:XG, ])
    }, simplify = FALSE)
  })

  D2.person.chains <- transpose(lapply(mcmc.samples, FUN = function(x) {
    x[D2.person]
  }))

  D2.person.mcmc <- lapply(D2.person.chains, function(x) {
    do.call(rbind, x)
  })

  D2.person.r.hat <- lapply(D2.person.mcmc, FUN = function(x) {
    apply(x, 2, FUN = function(y) {
      posterior::rhat(matrix(y, ncol = chains)[burn:XG, ])
    }, simplify = FALSE)
  })

  D3.chains <- lapply(mcmc.samples, FUN = function(x) {
    x[D3]$CovMat.Item
  })

  D3.mcmc <- lapply(D3.chains, FUN = function(x) {
    temp <- x[,,1]
    for (i in 2:4) {
      temp <- cbind(temp, x[,,i])
    }
    temp
  })

  D3.r.hat <- lapply(
    apply(do.call(rbind, D3.mcmc), 2, FUN = function(y) {
      matrix(y, ncol = chains)[burn:XG, ]
    }, simplify = FALSE), FUN = function(x) {
      posterior::rhat(x)
    }
  )
  D1.count <- ifelse(D1.r.hat < cutoff, 1, 0)
  D2.item.count <- ifelse(unlist(D2.item.r.hat) < cutoff, 1, 0)
  D2.person.count <- ifelse(unlist(D2.person.r.hat) < cutoff, 1, 0)
  D3.count <- ifelse(unlist(D3.r.hat) < cutoff, 1, 0)

  # assemble output
  return(list(
    value = list(D1 = D1.r.hat,
                 D2.item = unlist(D2.item.r.hat),
                 D2.person = unlist(D2.person.r.hat),
                 D3 = unlist(D3.r.hat)),
    convergence = list(D1 = D1.count,
                       D2.item = D2.item.count,
                       D2.person = D2.person.count,
                       D3 = D3.count),
    rate = list(D1 = mean(D1.count),
                D2.item = mean(D2.item.count),
                D2.person = mean(D2.person.count),
                D3 = mean(D3.count))
  )
  )
}



# Geweke Convergence ------------------------------------------------------

geweke.LNIRT <- function (object, z.cut = 1.96)  {
  XG = object$XG
  burn  = ceiling(object$burnin/100 * XG)
  print(burn)
  print(XG)
  D1 <-  c(
    "Mu.Person.Ability",
    "Mu.Person.Speed",
    "Var.Person.Ability",
    "Var.Person.Speed",
    "Cov.Person.Ability.Speed",
    "Mu.Item.Discrimination",
    "Mu.Item.Difficulty",
    "Mu.Time.Discrimination",
    "Mu.Time.Intensity")
  mcmc.D1 <- lapply(object$MCMC.Samples[D1], coda::as.mcmc)
  geweke.D1 <- sapply(mcmc.D1, FUN = function(x) {
    geweke <- unlist(coda::geweke.diag(x[burn:XG]))
    z.vars <- geweke[grep("z", names(geweke))]
    ifelse(z.vars > z.cut | z.vars < -z.cut, 0, 1)
  })

  D2.item <- c(
    "Item.Discrimination",
    "Item.Difficulty",
    "Time.Discrimination",
    "Time.Intensity",
    "Sigma2")
  mcmc.D2.item <- lapply(object$MCMC.Samples[D2.item], coda::as.mcmc)
  geweke.D2.item <- unlist(lapply(mcmc.D2.item, FUN = function(x) {
    geweke <- unlist(coda::geweke.diag(x[burn:XG,]))
    z.vars <- geweke[grep("z", names(geweke))]
    ifelse(z.vars > z.cut | z.vars < -z.cut, 0, 1)
  }))

  D2.person <- c(
    "Person.Ability",
    "Person.Speed")
  mcmc.D2.person <- lapply(object$MCMC.Samples[D2.person], coda::as.mcmc)
  geweke.D2.person <- unlist(lapply(mcmc.D2.person, FUN = function(x) {
    geweke <- unlist(coda::geweke.diag(x[burn:XG,]))
    z.vars <- geweke[grep("z", names(geweke))]
    ifelse(z.vars > z.cut | z.vars < -z.cut, 0, 1)
  }))

  D3 <- c(
    "CovMat.Item"
  )
  mcmc.D3 <- apply(object$MCMC.Samples[[D3]], c(2,3), coda::as.mcmc)
  geweke.D3 <- c()
  for (i in 1:4) {
    geweke <- unlist(apply(mcmc.D3[burn:XG, , i], 2, coda::geweke.diag))
    z.vars <- geweke[grep("z", names(geweke))]
    geweke.D3 <- append(geweke.D3,
                        ifelse(z.vars > z.cut | z.vars < -z.cut, 0, 1)
    )
  }

  # calculate diagnostic
  return(
    list(D1 = mean(geweke.D1),
         D2.item = mean(geweke.D2.item),
         D2.person = mean(geweke.D2.person),
         D3 = mean(geweke.D3))
  )
}




# Simulation Function -----------------------------------------------------

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

# test function
data <- sim.jhm.data(
  iter = 1,
  I = 20,
  N = 700,
  seed = 123)


# Compute MSE  ---------------------------------------------------------

comp_mse <- function(N,
                     iter,
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
                     sd.item = NULL,
                     n.cores = NULL) {

  # simulate data
  data <- sim.jhm.data(iter = iter,
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
                       item.seed = item.seed,
                       seed = seed,
                       cor2cov.item = cor2cov.item,
                       sd.item = sd.item)
  #print(data$person.par[[1]])
  # open parallel backend
  if (is.null(n.cores)) {
    n.cores = future::availableCores() - 1
  }
  if (n.cores > 1L) {
    future::plan(future::multisession, workers = n.cores)
  } else {
    future::plan(future::sequential)
  }

  # set progressr
  handler <-list(progressr::handler_txtprogressbar())

  # run in parallel with progress
  out <- progressr::with_progress({
    p <- progressr::progressor(steps = iter)

    future.apply::future_lapply(
      X = seq_len(iter),
      FUN = function(i) {

        # fit LNIRT with 4 chains
        fit.list <- list()
        for (f in 1:4) {
          fit.list[[f]] <- LNIRT::LNIRT(
            RT = data$time.data[[i]],
            Y = data$response.data[[i]],
            XG = 3000,
            burnin = 33.33,
            residual = FALSE,
            par1 = TRUE
          )}

        # compute r.hat of item parameter samples
        r.hat.out <- rhat_LNIRT(fit.list, chains = 4, cutoff = 1.05)
        r.hat.rates <- unlist(r.hat.out$rate)

        # compute posterior means
        post.theta = rowMeans(as.data.frame(lapply(fit.list, FUN = function(x) {
          (x$Post.Means$Person.Ability)
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

        # calculate (mean) squared errors
        res <- list(
          mse.theta = mean((post.theta - data$person.par[[i]]$theta)^2),
          se.alpha = (post.alpha - data$item.par[[i]]$alpha)^2,
          se.beta = (post.beta - data$item.par[[i]]$beta)^2,
          se.phi = (post.phi - data$item.par[[i]]$phi)^2,
          se.lambda = (post.lambda - data$item.par[[i]]$lambda)^2,
          se.sigma2 = (post.sigma2 - data$item.par[[i]]$log.sigma2)^2,
          conv.rate = r.hat.rates
        )

        # track and return
        p()
        res
      },
      future.seed = TRUE, # ensure random workers
      future.stdout = FALSE
    )
  },
  handlers = handler)

  # take median across iterations
  mse.theta = list(mse.theta = median(unlist(lapply(out, FUN = function(x) {
    x$mse.theta
  }))))
  mse.items =
    colMeans(apply(
      simplify2array(
        lapply(out, FUN = function(x) {
          cbind(mse.alpha = x$se.alpha,
                mse.beta = x$se.beta,
                mse.phi = x$se.phi,
                mse.lambda = x$se.lambda,
                mse.sigma2 = x$se.sigma2)
        })), MARGIN = c(1,2), median))

  # store convergence rates
  conv.rate = as.data.frame(t(sapply(out, FUN = function(x) {
    x$conv.rate
  })))

  # return output
  return(
    MSE = append(
      append(mse.theta, as.list(mse.items)), list(conv.rate = conv.rate))
  )
}

# test
start.time <- Sys.time()
comp_mse(iter = 5, N = 100, I = 20, n.cores = 5)
end.time <- Sys.time()
print(end.time - start.time)

# Optimize Sample Size ----------------------------------------------------

optim.sample <- function(FUN = comp_mse,
                         thresh,
                         lb,
                         ub,
                         tol,
                         out.par = 'mse.alpha',
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
                         item.seed = 12345,
                         seed = NULL,
                         cor2cov.item = FALSE,
                         sd.item = NULL,
                         n.cores = NULL
) {

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
      seed = seed,
      cor2cov.item = cor2cov.item,
      sd.item = sd.item,
      n.cores = n.cores
    )

    return(
      list(
        res = FUN.out[[out.par]],
        conv.rate = FUN.out$conv.rate)
    )
  }

  # compute N for lower bound
  res.lb <- compute_obj(newN = lb)
  cat("LB result is", res.lb$res, "\n")

  # check lower bound result
  if (res.lb$res < thresh) {
    cat("stop due to res.lb < thresh with", lb, "\n")
    stop(list(N.best = NA,
              res.best = "res.lb < thresh",
              reps = 1,
              track.res = data.frame(res.lb = "res.lb < thresh",
                                     res.ub = NULL,
                                     res.temp = c("res.lb < thresh")),
              data.frame(N.lb = rep(lb),
                         N.ub = rep(ub),
                         N.temp = NA)))}

  # compute N for upper bound
  res.ub <- compute_obj(newN = ub)
  cat("UB result is", res.ub$res, "\n")

  # check upper bound result
  if (res.ub$res > thresh) {
    cat("stop due to res.ub > thresh with", ub, "\n")
    stop(list(N.best = NA,
              res.best = "res.ub > thresh",
              reps = 2,
              track.res = data.frame(res.lb = res.lb$res,
                                     res.ub = "res.ub > thresh",
                                     res.temp = c(res.lb$res, "res.ub > thresh")),
              data.frame(N.lb = rep(lb, 2),
                         N.ub = rep(ub, 2),
                         N.temp = c(lb, ub))))}

  # track N and resulting output parameter
  track.N <- data.frame(N.lb = rep(lb, 2),
                        N.ub = rep(ub, 2),
                        N.temp = c(lb, ub))
  track.res <- data.frame(res.lb = res.lb$res,
                          res.ub = res.ub$res,
                          res.temp = c(res.lb$res, res.ub$res))
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
    track.res[reps,] <- c(res.lb$res, res.ub$res, res.temp$res)
    track.N[reps, ] <- c(N.lb, N.ub, N.temp)
    track.conv[[reps]] <- res.temp$conv.rate
    cat("New result is", c(res.lb$res, res.ub$res), "\n")

    # check tolerance
    if (thresh > res.temp$res & (abs(thresh - res.temp$res) < tol)) {
      break
    }
  }
  # end routine

  # assemble output
  res.best <- max(track.res$res.temp[which(track.res$res.temp < thresh)])
  N.best <- min(track.N[which(track.res$res.temp == res.best), ]$N.temp)
  cat("Best result is", res.best," for threshold", thresh, "\n")
  cat("Minimum N is", N.best, "\n")

  # return output
  return(list(N.best,
              res.best,
              reps,
              track.res,
              track.N,
              track.conv))
}


# test
res <- optim.sample(
  FUN = comp_mse,
  thresh = .04,
  lb = 100,
  ub = 1000,
  tol = .001,
  out.par = "mse.alpha",
  I = 10,
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
  iter = 7,
  n.cores = 7,
  item.seed = 12345
)

# assessing variance of the mse estimates for fixed iter
res.list <- list()
for (i in 1:3) {
  res.list[[i]] <- comp_mse(N = 100,
                            I = 10,
                            iter = 100,
                            n.cores = 6,
                            item.seed = 12345)
}

res.dat <- as.data.frame(t(sapply(res.list, FUN = function(x) {
  c(x$mse.alpha, x$mse.beta, x$mse.phi, x$mse.lambda)
})))

save(res.dat, file = "/Users/lortz/Desktop/PhD/Research/Chapter 1/objective_variance")
ggplot(data = res.dat, mapping = aes(V1)) +
  geom_histogram() +
  geom_density(size = 1)

# now fix the person sampling
res.list.seed <- list()
for (i in 41:50) {
  res.list.seed[[i]] <- comp_mse(N = 100,
                                 I = 10,
                                 iter = 100,
                                 n.cores = 6,
                                 item.seed = 12345,
                                 seed = 123)
}

res.dat.seed <- as.data.frame(t(sapply(res.list.seed, FUN = function(x) {
  c(x$mse.alpha, x$mse.beta, x$mse.phi, x$mse.lambda)
})))

save(res.dat.seed, file = "/Users/lortz/Desktop/PhD/Research/Chapter 1/objective_variance_seed")
ggplot(data = res.dat.seed, mapping = aes(V1)) +
  geom_histogram() +
  geom_density(size = 1)


# Testing & Debugging -----------------------------------------------------

# computation time
# 1.5h for 100 iterations with N = 1000, I = 20, XG = 3000
1.5*10 # 15h for 1000 iterations
1.5*10*15 # 225h for optim.sample -> 10 days for single design condition
100 * 1.5*10*15 / (100*.85) # 264h with 100 conditions and 100 cores

4^6
# plotting step increase
upper.bounds <- seq(1000,100000, 100)
steps.needed <- ceiling(log(upper.bounds)/log(2))
plot(ubounds, steps.needed)


fit.list[[1]]$Post.Means$Person.Ability


#sim.test <- list(N = 1:trials, res = 1:trials, stept = 1:trials)
for (i in 7:trials) {
  out.test <- optim.sample(
    FUN = comp_mse,
    thresh = .05,
    lb = 50,
    ub = 500,
    iter = 100,
    tol = .001)
  #out.test[[1]]
  sim.test[[1]][i] <- out.test[[1]]
  sim.test[[2]][i] <- out.test[[2]]
  sim.test[[3]][i] <- out.test[[3]]
}
sim.test <- as.data.frame(sim.test)

ggplot(sim.test, aes(x = res)) +
  geom_density() +
  xlim(c(thresh-.1,thresh+.1)) +
  geom_vline(xintercept = thresh+tol, col = "red")
ggplot(sim.test, aes(x = N)) +
  geom_density() +
  geom_vline(xintercept = mean(sim.test$N), col = "red")
ggplot(sim.test, aes(x = stept)) +
  geom_density() +
  xlim(c(-1, max(sim.test$stept))) +
  geom_vline(xintercept = mean(sim.test$stept), col = "red")





