#' Get an sspLNIRT.object from the precomputed sspLNIRT data
#'
#' The function gets the sspLNIRT object from the sspLNIRT data tibble, containing
#' the minimum sample size to reach the threshold of root mean squared errors (RMSE)
#' of estimated parameters. The sspLNIRT data was precomputed using `optim_sample`.
#' The RMSE is based on simulated data under the Joint Hierarchical Model using a 2-pl
#' normal ogive model for response accuracy and a 3-pl log-normal model for response time.
#'
#' @param thresh Numeric. The desired RMSE threshold of the target parameter to be achieved.
#' @param out.par Character. The name of the target parameter for the threshold.
#' @param K Integer. The test length.
#' @param mu.alpha Numeric. The mean of the discrimination parameter alpha.
#' @param meanlog.sigma2 Numeric. The meanlog of sigma2.
#' @param rho Numeric. The correlation between theta and zeta.
#'
#' @return A list containing:
#' \describe{
#'   \item{object}{The sspLNIRT.object with results.}
#'   \item{design}{The design i.e., set of parameter values used for this result.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- get_sspLNIRT(
#'   thresh = .1,
#'   out.par = "alpha",
#'   K = 10,
#'   mu.alpha = 1,
#'   meanlog.sigma2 = log(.6),
#'   rho = 0.2
#' )
#' summary(result$object)
#' result$cfg
#' }
#' @export
#'
get_sspLNIRT <- function(
    thresh,
    out.par,
    K,
    mu.alpha,
    meanlog.sigma2,
    rho
) {

  # helper function to compare values

  values_match <- function(a, b, tol = 1e-3) {
    if (is.null(a) || is.null(b)) return(FALSE)
    abs(as.numeric(a) - as.numeric(b)) < tol
  }

  # find matching row
  match_idx <- NULL
  mismatches <- list()

  for (i in seq_len(nrow(sspLNIRT.data))) {
    cfg <- sspLNIRT.data$cfg[[i]]

    all_match <- TRUE
    row_mismatches <- c()

    # check design conditions only
    if (!values_match(cfg$thresh, thresh)) {
      all_match <- FALSE
      row_mismatches <- c(row_mismatches, sprintf("thresh: requested %s, available %s", thresh, cfg$thresh))
    }
    if (!identical(cfg$out.par, out.par)) {
      all_match <- FALSE
      row_mismatches <- c(row_mismatches, sprintf("out.par: requested '%s', available '%s'", out.par, cfg$out.par))
    }
    if (!values_match(cfg$K, K)) {
      all_match <- FALSE
      row_mismatches <- c(row_mismatches, sprintf("K: requested %s, available %s", K, cfg$K))
    }
    if (!values_match(cfg$mu.item[1], mu.alpha)) {
      all_match <- FALSE
      row_mismatches <- c(row_mismatches, sprintf("mu.alpha: requested %s, available %s", mu.alpha, cfg$mu.item[1]))
    }
    if (!values_match(cfg$meanlog.sigma2, meanlog.sigma2)) {
      all_match <- FALSE
      row_mismatches <- c(row_mismatches, sprintf("meanlog.sigma2: requested %s, available %s",
                                                  meanlog.sigma2, cfg$meanlog.sigma2))
    }
    if (!values_match(cfg$cov.m.person[1, 2], rho)) {
      all_match <- FALSE
      row_mismatches <- c(row_mismatches, sprintf("rho: requested %s, available %s", rho, cfg$cov.m.person[1, 2]))
    }

    if (all_match) {
      match_idx <- i
      break
    } else {
      mismatches[[i]] <- row_mismatches
    }
  }

  # return result or error
  if (!is.null(match_idx)) {
    res <- sspLNIRT.data$res[[match_idx]]
    cfg <- sspLNIRT.data$cfg[[match_idx]]
    class(res) <- "sspLNIRT.object"
    return(list(object = res, design = cfg))
  } else {
    # find the row with fewest mismatches for helpful error
    n_mismatches <- sapply(mismatches, length)
    best_row <- which.min(n_mismatches)

    stop(paste0(
      "No matching configuration found in precomputed data.\n\n",
      "Closest match (row ", best_row, ") differs in ", n_mismatches[best_row], " parameter(s):\n",
      paste("
", mismatches[[best_row]], collapse = "\n"),
      "\n\nUse `available_configs()` to see all available configurations."
    ))
  }
}

