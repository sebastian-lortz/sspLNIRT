# Rhat for LNIRT objects

The function calculates Rhat convergence diagnostics (Vehtari et al.,
2021) for all parameters from a list of LNIRT objects.

## Usage

``` r
rhat_LNIRT(object.list, chains = 4, cutoff = 1.05)
```

## Arguments

- object.list:

  List. A list containing fitted LNIRT objects.

- chains:

  Integer. The number of chains to consider.

- cutoff:

  Numeric. The Rhat cutoff value for convergence.

## Value

A list containing:

- value:

  List. R-hat values by block

- convergence:

  List. Binary indicators (1 if R-hat \< `cutoff`, else 0) by block

- rate:

  List. Mean convergence rate (proportion below `cutoff`) by block
