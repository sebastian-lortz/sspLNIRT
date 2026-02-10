
#' Show available precomputed configurations
#'
#' @return A tibble summarizing available design conditions
#' @export
available_configs <- function() {
  data.frame(
    thresh = sapply(sspLNIRT.data$cfg, `[[`, "thresh"),
    out.par = sapply(sspLNIRT.data$cfg, `[[`, "out.par"),
    K = sapply(sspLNIRT.data$cfg, `[[`, "K"),
    mu.alpha = sapply(sspLNIRT.data$cfg, function(x) x$mu.item[1]),
    meanlog.sigma2 = sapply(sspLNIRT.data$cfg, `[[`, "meanlog.sigma2"),
    rho = sapply(sspLNIRT.data$cfg, function(x) x$cov.m.person[1, 2])
  )
}
