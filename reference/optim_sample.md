# Optimize for Sample Size

The function optimizes for the minimum sample size to reach the
threshold of root mean squared errors (RMSE) of estimated parameters.
MSE are based on simulated data under the Joint Hierarchical Model using
a 2-pl normal ogive model for response accuracy and a 3-pl log-normal
model for response time.

## Usage

``` r
optim_sample(
  thresh,
  range,
  out.par = "alpha",
  iter = 100,
  K = 10,
  mu.person = c(0, 0),
  mu.item = c(1, 0, 0.5, 1),
  meanlog.sigma2 = log(0.6),
  cov.m.person = matrix(c(1, 0.2, 0.2, 1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(0.2, 0, 0, 0, 0, 1, 0, 0.4, 0, 0, 0.2, 0, 0, 0.4, 0, 0.5), ncol =
    4, byrow = TRUE),
  sdlog.sigma2 = 0.2,
  item.pars.m = NULL,
  cor2cov.item = FALSE,
  sd.item = NULL,
  keep.err.dat = FALSE,
  seed = NULL,
  XG = 6000,
  burnin = 20,
  rhat = 1.05
)
```

## Arguments

- thresh:

  Numeric. The desired RMSE threshold of the target parameter to be
  achieved.

- range:

  Integer vector. The lower and upper bounds of the sample size to be
  considered.

- out.par:

  Character. The name of the target parameter for the threshold.

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

- keep.err.dat:

  Logical. Whether to keep the full error data.

- seed:

  Integer or NULL. Seed for reproducibility.

- XG:

  Integer. The number of Gibbs sampler iterations.

- burnin:

  Integer. The burn-in percentage.

- rhat:

  Numeric. The R-hat convergence cutoff.

## Value

A list of class \`sspLNIRT.object\` containing:

- N.min:

  Integer (or character if bounds not met). The minimum sample size
  achieving the threshold.

- res.best:

  Numeric. The RMSE result at the optimal N.

- comp.rmse:

  List. The full output from \`comp_rmse\` at the optimal N.

- trace:

  List containing optimization diagnostics: \`steps\` (integer),
  \`track.res\` (data frame), \`track.N\` (data frame), and
  \`time.taken\` (difftime).

## Examples

``` r
if (FALSE) { # \dontrun{
test.optim.sample <- optim_sample(

  thresh = .1,
  range = c(100, 500),
  out.par = "alpha",
  iter = 5,
  K = 10,
  mu.person = c(0, 0),
  mu.item = c(1, 0, 1, 0),
  meanlog.sigma2 = log(.3),
  cov.m.person = matrix(c(1, 0.5, 0.5, 1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, 0, 0, 0,
                        0, 1, 0, 0.3,
                        0, 0, 1, 0,
                        0, 0.3, 0, 1), ncol = 4, byrow = TRUE),
  sd.item = c(.2, .5, .2, .5),
  cor2cov.item = TRUE,
  sdlog.sigma2 = 0.2,
  XG = 1000
)
summary(test.optim.sample)
} # }
```
