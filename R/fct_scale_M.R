#' Scale Item and Person Matrices
#'
#' The function scales the matrices containing item and person parameters by setting the
#' geometric means of alpha and phi being equal to 1, to align with the LNIRT package.
#'
#' @param item.pars Data frame or Matrix. The item parameters.
#' @param person.pars Data frame or Matrix. The person parameters.
#'
#' @return A list containing:
#' \describe{
#'   \item{items.pars.scaled}{Matrix. The scaled item parameters.}
#'   \item{person.pars.scaled}{Matrix. The scaled person parameters.}
#'   \item{c.alpha}{Numeric. The scaling constant of alpha.}
#'   \item{c.phi}{Numeric. The scaling constant of phi}
#' }
#'
#' @export
#'



scale_M <- function(item.pars,
                    person.pars,
                    re.scale = FALSE,
                    c.alpha = NULL,
                    c.phi = NULL) {

  # I and N
  I = nrow(item.pars)
  N = nrow(person.pars)

  # parameters re-scaling
  if (re.scale) {

    # scaling vectors
    c.items <- matrix(rep(c(c.alpha, 1/c.alpha, c.phi, 1/c.phi, 1), I), ncol = 5, byrow = TRUE)
    c.persons <- matrix(rep(c(1/c.alpha, 1/c.phi), N), ncol = 2, byrow = TRUE)

    # parameter scaling
  } else {

  # scaling factor
  c.alpha <- prod(item.pars$alpha)^(1/I)
  c.phi <- prod(item.pars$phi)^(1/I)

  # scaling vectors
  c.items <- matrix(rep(c(1/c.alpha, c.alpha, 1/c.phi, c.phi, 1), I), ncol = 5, byrow = TRUE)
  c.persons <- matrix(rep(c(c.alpha, c.phi), N), ncol = 2, byrow = TRUE)
  }

  # scale matrices
  items.pars.scaled <- item.pars * c.items
  person.pars.scaled <- person.pars * c.persons

  # return output
  return(
    list(
      items.pars.scaled = as.data.frame(items.pars.scaled),
      person.pars.scaled = as.data.frame(person.pars.scaled),
      c.alpha = c.alpha,
      c.phi = c.phi
    )
  )
}
