#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

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
  "MASS",
  "lavaan",
  "tmvtnorm",
  "LNIRT",
  "future",
  "progressr",
  "purrr",
  "parallel",
  "future.apply"
)
pkg(pkgs)
