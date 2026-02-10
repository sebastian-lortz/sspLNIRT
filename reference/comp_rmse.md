# Compute RMSE of Parameters

The function computes the mean squared errors of estimated parameters
based on simulated data under the Joint Hierarchical Model using a 2-pl
normal ogive model for response accuracy and a 3-pl log-normal model for
response time.

## Usage

``` r
comp_rmse(
  N,
  iter,
  K,
  mu.person = c(0, 0),
  mu.item = c(1, 0, 1, 1),
  meanlog.sigma2 = log(0.2),
  cov.m.person = matrix(c(1, 0.5, 0.5, 1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(0.2, 0, 0, 0, 0, 0.5, -0.35, -0.15, 0, -0.35, 0.5, 0.15, 0,
    -0.15, 0.15, 0.2), ncol = 4, byrow = TRUE),
  sdlog.sigma2 = 0.2,
  item.pars.m = NULL,
  cor2cov.item = FALSE,
  sd.item = NULL,
  XG = 3000,
  burnin = 20,
  seed = NULL,
  keep.err.dat = TRUE,
  rhat = 1.05
)
```

## Arguments

- N:

  Integer. The sample size.

- iter:

  Integer. The number of iterations or the number of data sets.

- K:

  Integer. The test length.

- mu.person:

  Numeric vector. Means of theta and zeta.

- mu.item:

  Numeric vector. Means of alpha, beta, phi, and lambda.

- meanlog.sigma2:

  Numeric. The meanlog of sigma2.

- cov.m.person:

  Matrix. The covariance matrix of theta and zeta.

- cov.m.item:

  Matrix. The covariance matrix of alpha, beta, phi, and lambda.

- sdlog.sigma2:

  Numeric. The sdlog of sigma2.

- item.pars.m:

  Matrix. (optional) A matrix containing item parameters.

- cor2cov.item:

  Logical. Whether a correlation matrix instead of covariance matrix is
  supplied.

- sd.item:

  Numeric vector. (optional) The standard deviations of alpha, beta,
  phi, and lambda.

- XG:

  Integer. The number of Gibbs sampler iterations.

- burnin:

  Integer. The burn-in percentage.

- seed:

  Integer or NULL. Seed for reproducibility.

- keep.err.dat:

  Logical. Whether to keep the full error data.

- rhat:

  Numeric. The R-hat convergence cutoff.

## Value

A list of class \`sspLNIRT.object\` containing:

- person:

  List with \`rmse\` (named vector), \`mc.sd.rmse\` (named vector), and
  \`bias\` (named vector) for theta and zeta.

- item:

  List with \`rmse\` (named vector), \`mc.sd.rmse\` (named vector), and
  \`bias\` (named vector) for alpha, beta, phi, lambda, and sigma2.

- conv.rate:

  Numeric. Proportion of iterations that converged (n converged / iter).

- err.dat:

  List with \`person\` and \`item\` data frames containing
  per-replication errors (if \`keep.err.dat = TRUE\`).

## Examples

``` r
if (FALSE) { # \dontrun{
test <- comp_rmse(
  iter = 5,
  N = 100,
  K = 10,
  mu.person = c(0, 0),
  mu.item = c(1, 0, 1, 1),
  meanlog.sigma2 = log(1),
  cov.m.person = matrix(c(1, 0, 0, 1), ncol = 2, byrow = TRUE),
  cov.m.item = diag(4),
  sd.item = c(.2, 1, .2, .5),
  cor2cov.item = TRUE,
  sdlog.sigma2 = 0.2,
  XG = 2000,
  keep.err.dat = FALSE,
  rhat = 1.05
)
summary(test)
} # }
```
