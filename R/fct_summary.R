
#' Summarize a sspLNIRT.object
#'
#' Provides a comprehensive summary of a `sspLNIRT.object`.
#'
#' @param object A `sspLNIRT.object` produced by `optim_sample` or `comp_mse`.
#' @param ... Additional arguments (unused).
#'
#' @return An object of class `summary.sspLNIRT.object`.
#'
#' @method summary sspLNIRT.object
#' @exportS3Method summary sspLNIRT.object
summary.sspLNIRT.object <- function(object, ...) {
  # input checks
  if (!inherits(object, "sspLNIRT.object")) {
    stop("Input must be a sspLNIRT.object.")
  }

  if (!is.null(object$N.best)) {
    # optim_sample output
    summary_obj <- list(
      N.best = object$N.best,
      res.best = object$res.best,
      comp.mse = object$comp.mse,
      trace = object$trace
    )
  } else {
    # comp_mse output
    summary_obj <- list(
      person = object$person,
      item = object$item,
      conv.rate = object$conv.rate
    )
  }

  class(summary_obj) <- "summary.sspLNIRT.object"
  summary_obj
}
