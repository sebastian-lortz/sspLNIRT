# Plot the power curve from sample size optimization

The function extracts the optimization trace from an
[`optim_sample()`](https://sebastian-lortz.github.io/sspLNIRT/reference/optim_sample.md)
object, fits a log-log power curve (log(result) ~ log(N)), and displays
the relationship on both the log-log and original scales side by side,
annotated with regression coefficients and R-squared.

## Usage

``` r
plot_power_curve(object, thresh = NULL)
```

## Arguments

- object:

  Object. Output from
  [`optim_sample()`](https://sebastian-lortz.github.io/sspLNIRT/reference/optim_sample.md).

- thresh:

  Numeric (optional). Decision threshold used in optimization. If
  `NULL`, extracted from the object.

## Value

A combined ggplot object (via patchwork).

## Examples

``` r
 if (FALSE) { # \dontrun{
   plot_power_curve(object = test.optim.sample)
} # }
```
