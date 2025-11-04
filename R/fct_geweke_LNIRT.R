
#' Geweke Convergence for LNIRT objects
#'
#' The function calculates Geweke's convergence diagnostics (Geweke, 1992) for
#' all parameters from a LNIRT objects.
#'
#' @param object List. A fitted LNIRT object.
#' @param z.cut Numeric. The cutoff z-value (two-tailed) for rejecting convergence.
#'
#' @return A list containing:
#' \describe{
#'   \item{D1}{Numeric. Proportion with |z| ≤ z.cut for hyperparameters.}
#'   \item{D2.item}{Numeric. Proportion with |z| ≤ z.cut for item-level parameters.}
#'   \item{D2.person}{Numeric. Proportion with |z| ≤ z.cut for person-level parameters.}
#'   \item{D3}{Numeric. Proportion with |z| ≤ z.cut for item covariance elements.}
#' }
#'
#' @export
#'

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

