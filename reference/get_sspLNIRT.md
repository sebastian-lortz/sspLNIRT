# Get an sspLNIRT.object from the precomputed sspLNIRT data

The function gets the sspLNIRT object from the sspLNIRT data tibble,
containing the minimum sample size to reach the threshold of root mean
squared errors (RMSE) of estimated parameters. The sspLNIRT data was
precomputed using \`optim_sample\`. The RMSE is based on simulated data
under the Joint Hierarchical Model using a 2-pl normal ogive model for
response accuracy and a 3-pl log-normal model for response time.

## Usage

``` r
get_sspLNIRT(thresh, out.par, K, mu.alpha, meanlog.sigma2, rho)
```

## Arguments

- thresh:

  Numeric. The desired RMSE threshold of the target parameter to be
  achieved.

- out.par:

  Character. The name of the target parameter for the threshold.

- K:

  Integer. The test length.

- mu.alpha:

  Numeric. The mean of the discrimination parameter alpha.

- meanlog.sigma2:

  Numeric. The meanlog of sigma2.

- rho:

  Numeric. The correlation between theta and zeta.

## Value

A list containing:

- object:

  The sspLNIRT.object with results.

- design:

  The design i.e., set of parameter values used for this result.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- get_sspLNIRT(
  thresh = .1,
  out.par = "alpha",
  K = 10,
  mu.alpha = 1,
  meanlog.sigma2 = log(.6),
  rho = 0.2
)
summary(result$object)
result$cfg
} # }
```
