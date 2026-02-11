# Plot the estimation metrics of the item parameters

The function plots the root mean squared error (RMSE) or bias across
values of the true parameter. For objects from
[`optim_sample()`](https://sebastian-lortz.github.io/sspLNIRT/reference/optim_sample.md),
the function uses the data at the minimum N, or, if the optimization
stopped due to N being outside the specified range, at the respective
bound.

## Usage

``` r
plot_precision(object, pars, y.val = "rmse", n.bins = 30)
```

## Arguments

- object:

  Object. Output from
  [`optim_sample()`](https://sebastian-lortz.github.io/sspLNIRT/reference/optim_sample.md)
  or
  [`comp_rmse()`](https://sebastian-lortz.github.io/sspLNIRT/reference/comp_rmse.md).

- pars:

  String. Either "item" or "person" for which parameters to plot.

- y.val:

  String. Either "rmse" or "bias" for the y-axis.

- n.bins:

  Integer (optional). Number of bins used to aggregate by sim.val within
  par.

## Value

A ggplot object.

## Examples

``` r
 if (FALSE) { # \dontrun{
   plot_precision(object = test.optim.sample, pars = "item")
} # }
```
