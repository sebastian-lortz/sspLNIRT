
#' Generate optim_sample() function call as text
#'
#' @param input Shiny input object
#' @return Character string containing the function call
#' @noRd
generate_optim_sample_call <- function(input) {

  # Extract correlation matrix and SD from matrix input
  mat <- input$corr_sd_item
  corr_mat <- matrix(as.numeric(mat[1:4, 1:4]), nrow = 4, ncol = 4)
  sd_item <- as.numeric(mat[5, ])

  # Format matrix for display
  format_matrix <- function(m, indent = "                         ") {
    rows <- apply(m, 1, function(r) paste(format(r, nsmall = 1), collapse = ", "))
    paste0("matrix(c(", paste(rows, collapse = ",\n                  "), "),\n",
           indent, "nrow = 4, byrow = TRUE)")
  }

  # Format sigma2 as log(value)
  sigma2_display <- paste0("log(", input$sigma2_raw, ")")

  # Build the function call string
  call_str <- paste0(
    "optim_sample(\n",
    "  FUN            = comp_rmse,\n",
    "  thresh         = ", input$thresh, ",\n",
    "  range          = c(", input$lb, ", ", input$ub, "),\n",
    "  out.par        = '", input$out_par, "',\n",
    "  iter           = ", input$iter, ",\n",
    "  K              = ", as.integer(input$K), ",\n",
    "  mu.person      = c(", input$mu_theta, ", ", input$mu_zeta, "),\n",
    "  mu.item        = c(", input$mu_alpha, ", ", input$mu_beta, ", ",
    input$mu_phi, ", ", input$mu_lambda, "),\n",
    "  meanlog.sigma2 = ", sigma2_display, ",\n",
    "  cov.m.person   = matrix(c(1, ", input$corr_person, ",\n",
    "                            ", input$corr_person, ", 1), nrow = 2, byrow = TRUE),\n",
    "  cov.m.item     = ", format_matrix(corr_mat), ",\n",
    "  sd.item        = c(", paste(sd_item, collapse = ", "), "),\n",
    "  cor2cov.item   = TRUE,\n",
    "  sdlog.sigma2   = ", input$sdlog_sigma2, ",\n",
    "  XG             = ", input$XG, ",\n",
    "  burnin         = ", input$burnin, ",\n",
    "  rhat           = ", input$rhat, ",\n",
    "  seed           = ", input$seed, "\n",
    ")"
  )

  return(call_str)
}

#' Build design list from inputs
#'
#' @param input Shiny input object
#' @return List containing the design parameters
#' @noRd
build_design_from_inputs <- function(input) {

  # Extract correlation matrix and SD from matrix input
  # Ensure numeric conversion for shinyMatrix output
  mat <- input$corr_sd_item
  corr_mat <- matrix(as.numeric(mat[1:4, 1:4]), nrow = 4, ncol = 4)
  sd_item <- as.numeric(mat[5, ])

  list(
    thresh         = input$thresh,
    range          = c(input$lb, input$ub),
    out.par        = input$out_par,
    iter           = input$iter,
    K              = as.integer(input$K),
    mu.person      = c(input$mu_theta, input$mu_zeta),
    mu.item        = c(input$mu_alpha, input$mu_beta, input$mu_phi, input$mu_lambda),
    meanlog.sigma2 = log(input$sigma2_raw),
    cov.m.person   = matrix(c(1, input$corr_person,
                              input$corr_person, 1), nrow = 2, byrow = TRUE),
    cov.m.item     = corr_mat,
    sd.item        = sd_item,
    cor2cov.item   = TRUE,
    sdlog.sigma2   = input$sdlog_sigma2,
    XG             = input$XG,
    burnin         = input$burnin,
    rhat           = input$rhat,
    seed           = input$seed
  )
}

#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)
