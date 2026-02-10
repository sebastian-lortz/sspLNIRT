# Plot the simulated response time data

The function simulates data under the Joint Hierarchical Model using a
2-pl normal ogive model for response accuracy and a 3-pl log-normal
model for response time, and plots the resulting response time data.

## Usage

``` r
plot_RT(
  level,
  logRT = FALSE,
  N = 1e+05,
  K = 20,
  mu.person = c(0, 0),
  mu.item = c(1, 0, 1, 0),
  meanlog.sigma2 = log(0.3),
  cov.m.person = matrix(c(1, 0.5, 0.5, 1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1), ncol = 4, byrow
    = TRUE),
  sd.item = c(0.2, 1, 0.2, 0.5),
  sdlog.sigma2 = 0.2,
  item.pars.m = NULL,
  cor2cov.item = TRUE
)
```

## Arguments

- level:

  String. Either "person" or "item".

- logRT:

  Logical. Whether to plot on the log-RT scale.

- N:

  Integer. The sample size.

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

- sd.item:

  Numeric vector. The standard deviations of alpha, beta, phi, and
  lambda.

- sdlog.sigma2:

  Numeric. The sdlog of sigma2.

- item.pars.m:

  Matrix (optional). A matrix containing item parameters.

- cor2cov.item:

  Logical. Whether a correlation matrix instead of covariance matrix is
  supplied.

## Value

A ggplot object.

## Examples

``` r
 if (FALSE) { # \dontrun{
   plot_RT(level = "item",
           mu.item = c(1, 0, .4, 1),
           sd.item = c(.2, 1, .2, .5),
           meanlog.sigma2 = log(1),
           K = 30,
           logRT = FALSE)
} # }
```
