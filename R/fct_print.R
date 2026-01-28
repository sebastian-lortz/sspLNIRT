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
  if (!is.null(x$N.best)) {
    # optim_sample output
    cat("Call: optim_sample()\n\n")
    cat("Sample Size Optimization\n")
    cat("--------------------------------------------------\n")
    cat("  Optimal N: ", x$N.best, "\n")
    cat("  MSE result: ", x$res.best, "\n")
    cat("  Steps: ", x$trace$steps, "\n")
    cat("  Time elapsed:", format(x$trace$time.taken), "\n\n")
    # Item parameter MSEs
    cat("Item Parameter MSEs:\n")
    cat("--------------------------------------------------\n")
    rmse_items <- unlist(x$comp.mse$item$rmse)
    mc_sd_item <- unlist(x$comp.mse$item$mc.sd.rmse)
    item_df <- data.frame(
      RMSE = sprintf("%.6f", rmse_items),
      `MC SD` = sprintf("%.6f", mc_sd_item),
      check.names = FALSE
    )
    rownames(item_df) <- names(rmse_items)
    print(t(item_df), right = TRUE, quote = FALSE)
    # Person parameter MSEs
    cat("\nPerson Parameter MSEs:\n")
    cat("--------------------------------------------------\n")
    rmse_person <- unlist(x$comp.mse$person$rmse)
    mc_sd_person <- unlist(x$comp.mse$person$mc.sd.rmse)
    person_df <- data.frame(
      RMSE = sprintf("%.6f", rmse_person),
      `MC SD` = sprintf("%.6f", mc_sd_person),
      check.names = FALSE
    )
    rownames(person_df) <- names(rmse_person)
    print(t(person_df), right = TRUE, quote = FALSE)
    # Convergence info
    if (!is.null(x$comp.mse$conv.rate)) {
      cat("\nConverged Iterations in final step:", nrow(x$comp.mse$conv.rate), "\n")
    }
  } else {
    # comp_mse output
    cat("Call: comp_mse()\n\n")
    # Item parameters
    cat("Item Parameter MSEs:\n")
    cat("--------------------------------------------------\n")
    rmse_items <- unlist(x$item$rmse)
    mc_sd_item <- unlist(x$item$mc.sd.rmse)
    item_df <- data.frame(
      RMSE = sprintf("%.6f", rmse_items),
      `MC SD` = sprintf("%.6f", mc_sd_item),
      check.names = FALSE
    )
    rownames(item_df) <- names(rmse_items)
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
      cat("\nConverged Iterations:", nrow(x$conv.rate), "\n")
    }
  }
  cat("---\n")
  invisible(x)
}
