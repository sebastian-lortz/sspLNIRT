#' Print summary for summary.sspLNIRT.object
#'
#' A minimal S3 print method for objects of class `summary.sspLNIRT.object`, providing
#' diagnostics and summary statistics in the console.
#'
#' @param x A `summary.sspLNIRT.object` produced by `summary.sspLNIRT.object`.
#' @param ... Unused. Additional arguments are ignored.
#'
#' @return Invisibly returns the input object `x` after printing its summary.
#'
#' @method print summary.sspLNIRT.object
#' @exportS3Method print summary.sspLNIRT.object
print.summary.sspLNIRT.object <- function(x, ...) {
  cat("==================================================\n\n")
  if (!is.null(x$N.min)) {
    # optim_sample output
    cat("Call: optim_sample()\n\n")
    cat("Sample Size Optimization\n")
    cat("--------------------------------------------------\n")
    cat("  Minimum N: ", x$N.min, "\n")
    cat("  Power curve N: ", x$N.curve, "\n")
    cat("  RMSE at minimum N: ", x$res.best, "\n")
    cat("  Steps: ", x$trace$steps, "\n")
    cat("  Time elapsed:", format(x$trace$time.taken), "\n\n")
    # Item parameter MSEs
    cat("Item Parameter MSEs:\n")
    cat("--------------------------------------------------\n")
    rmse_item <- unlist(x$comp.rmse$item$rmse)
    mc_sd_item <- unlist(x$comp.rmse$item$mc.sd.rmse)
    bias_item <- unlist(x$comp.rmse$item$bias)
    item_df <- data.frame(
      RMSE = sprintf("%.6f", rmse_item),
      `MC SD` = sprintf("%.6f", mc_sd_item),
      Bias = sprintf("%.6f", bias_item),
      check.names = FALSE
    )
    rownames(item_df) <- names(rmse_item)
    print(t(item_df), right = TRUE, quote = FALSE)
    # Person parameter MSEs
    cat("\nPerson Parameter RMSEs:\n")
    cat("--------------------------------------------------\n")
    rmse_person <- unlist(x$comp.rmse$person$rmse)
    mc_sd_person <- unlist(x$comp.rmse$person$mc.sd.rmse)
    bias_person <- unlist(x$comp.rmse$person$bias)
    person_df <- data.frame(
      RMSE = sprintf("%.6f", rmse_person),
      `MC SD` = sprintf("%.6f", mc_sd_person),
      Bias = sprintf("%.6f", bias_person),
      check.names = FALSE
    )
    rownames(person_df) <- names(rmse_person)
    print(t(person_df), right = TRUE, quote = FALSE)
    # Convergence info
    if (!is.null(x$comp.rmse$conv.rate)) {
      cat("\nConverged MC iterations at minimum N:",x$comp.rmse$conv.rate*100, "%\n")
    }
  } else {
    # comp_mse output
    cat("Call: comp_mse()\n\n")
    # Item parameters
    cat("Item Parameter RMSEs:\n")
    cat("--------------------------------------------------\n")
    rmse_item <- unlist(x$item$rmse)
    mc_sd_item <- unlist(x$item$mc.sd.rmse)
    item_df <- data.frame(
      RMSE = sprintf("%.6f", rmse_item),
      `MC SD` = sprintf("%.6f", mc_sd_item),
      check.names = FALSE
    )
    rownames(item_df) <- names(rmse_item)
    print(t(item_df), right = TRUE, quote = FALSE)
    # Person parameters
    cat("\nPerson Parameter MSEs:\n")
    cat("--------------------------------------------------\n")
    rmse_person <- unlist(x$person$rmse)
    mc_sd_person <- unlist(x$person$mc.sd.rmse)
    person_df <- data.frame(
      RMSE = sprintf("%.6f", rmse_person),
      `MC SD` = sprintf("%.6f", mc_sd_person),
      check.names = FALSE
    )
    rownames(person_df) <- names(rmse_person)
    print(t(person_df), right = TRUE, quote = FALSE)
    # Convergence
    if (!is.null(x$conv.rate)) {
      cat("\nConverged MC iterations at minimum N: ",x$conv.rate*100, "%\n")
    }
  }
  cat("---\n")
  invisible(x)
}
