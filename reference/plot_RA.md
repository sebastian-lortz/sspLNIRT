# Plot the simulated response accuracy data

The function simulates data under the Joint Hierarchical Model using a
2-pl normal ogive model for response accuracy and a 3-pl log-normal
model for response time, and plots the resulting response accuracy data.

## Usage

``` r
plot_RA(
  level,
  by.theta = FALSE,
  N = 1e+05,
  K = 20,
  mu.person = c(0, 0),
  mu.item = c(1, 0, 0.5, 0),
  meanlog.sigma2 = log(0.3),
  cov.m.person = matrix(c(1, 0.4, 0.4, 1), ncol = 2, byrow = TRUE),
  cov.m.item = matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1), ncol = 4, byrow
    = TRUE),
  sd.item = c(0.2, 1, 0.2, 0.5),
  sdlog.sigma2 = 0,
  item.pars.m = NULL,
  cor2cov.item = TRUE
)
```

## Arguments

- level:

  String. Either "person" or "item".

- by.theta:

  Logical. Whether to plot as a function of theta.

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
   plot_RA(level = "item",
           by.theta = TRUE,
           mu.item = c(1, 0, 1, 1),
           sd.item = c(.2, .5, .2, .5),
           meanlog.sigma2 = log(.2),
           K = 20)
} # }
```
