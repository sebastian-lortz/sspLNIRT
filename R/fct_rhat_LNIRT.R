#' Rhat for LNIRT objects
#'
#' The function calculates Rhat convergence diagnostics (Vehtari et al., 2021) for
#' all parameters from a list of LNIRT objects.
#'
#' @param object.list List. A list containing fitted LNIRT objects.
#' @param chains Integer. The number of chains to consider.
#' @param cutoff Numeric. The Rhat cutoff value for convergence.
#'
#' @return A list containing:
#' \describe{
#'   \item{value}{List. R-hat values by block}
#'   \item{convergence}{List. Binary indicators (1 if R-hat < \code{cutoff}, else 0) by block}
#'   \item{rate}{List. Mean convergence rate (proportion below \code{cutoff}) by block}
#' }
#'
#' @export

rhat_LNIRT <- function(object.list,
                       chains = 4,
                       cutoff = 1.01) {

  # input checks
  stopifnot(length(object.list) >= 2)
  blocks <- Reduce(intersect, lapply(object.list, function(f) names(f$MCMC.Samples)))

  # get sampler length and burn-in
  XG = object.list[[1]]$XG
  burn = ceiling(object.list[[1]]$burnin/100 * XG)

  # create blocks by dimensions
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

  # get all mcmc samples
  mcmc.samples <- lapply(object.list, FUN = function(x) {
    x$MCMC.Samples })

  # get mcmc chains of the blocks
  D1.chains <- purrr::transpose(lapply(mcmc.samples, FUN = function(x) {
    x[D1]
  }))
  D2.item.chains <- purrr::transpose(lapply(mcmc.samples, FUN = function(x) {
    x[D2.item]
  }))
  D2.person.chains <- purrr::transpose(lapply(mcmc.samples, FUN = function(x) {
    x[D2.person]
  }))
  D3.chains <- lapply(mcmc.samples, FUN = function(x) {
    x[D3]$CovMat.Item
  })

  # create list of matrices, colums = mcmc chains
  D1.mcmc <- lapply(D1.chains, function(x) {
    m <- do.call(cbind, x)
    m[burn:XG,]
  })
  D2.item.mcmc <- lapply(D2.item.chains, function(x) {
    do.call(rbind, x)
  })
  D2.person.mcmc <- lapply(D2.person.chains, function(x) {
    do.call(rbind, x)
  })
  D3.mcmc <- lapply(D3.chains, FUN = function(x) {
    temp <- x[,,1]
    for (i in 2:4) {
      temp <- cbind(temp, x[,,i])
    }
    temp
  })

  # compute Rhat values
  D1.r.hat <- lapply(D1.mcmc, posterior::rhat)
  D2.item.r.hat <- lapply(D2.item.mcmc, FUN = function(x) {
    apply(x, 2, FUN = function(y) {
      posterior::rhat(matrix(y, ncol = chains)[burn:XG, ])
    }, simplify = FALSE)
  })
  D2.person.r.hat <- lapply(D2.person.mcmc, FUN = function(x) {
    apply(x, 2, FUN = function(y) {
      posterior::rhat(matrix(y, ncol = chains)[burn:XG, ])
    }, simplify = FALSE)
  })
  D3.r.hat <- lapply(
    apply(do.call(rbind, D3.mcmc), 2, FUN = function(y) {
      matrix(y, ncol = chains)[burn:XG, ]
    }, simplify = FALSE), FUN = function(x) {
      posterior::rhat(x)
    }
  )

  # count convergence
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
