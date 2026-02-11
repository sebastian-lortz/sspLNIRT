# sspLNIRT

``` r
# install devtools if needed
if (!requireNamespace("devtools")) {install.packages("devtools")}

# install from GitHub
devtools::install_github("sebastian-lortz/sspLNIRT")
```

``` r
# load package
library(sspLNIRT)
```

## Overview

The `sspLNIRT` package provides sample size planning tools for item
calibration under the Joint Hierarchical Model (JHM; van der Linden,
2007), combining a 2-parameter normal ogive model for response accuracy
with a log-normal model for response times, estimated via Gibbs sampling
through the `LNIRT` package (Fox et al., 2023).

The core question is: *What is the minimum sample size to achieve a
desired precision of item parameter estimates, given the assumed
data-generating process?* Precision is defined as the root mean squared
error (RMSE) of a target item parameter (e.g., discrimination $\alpha$)
falling below a user-specified threshold, averaged across items and
Monte Carlo iterations.

For a detailed description of the method, simulation model, and
precomputed design conditions, see the manuscript.

## Step 1: Specify the data-generating process

The first step is to specify the assumed data-generating process. This
includes the item and person parameter distributions that define the
simulation model. The following example uses a 10-item test with the
following simulation model specifications.

``` r
sim.mod.args <- list(
  K              = 10,
  mu.person      = c(0, 0),
  mu.item        = c(1, 0, .5, 1),
  meanlog.sigma2 = log(0.6),
  sdlog.sigma2   = 0,
  cov.m.person   = matrix(c(1,   0.4,
                             0.4, 1), ncol = 2, byrow = TRUE),
  cov.m.item     = matrix(c(1, 0,   0,   0,
                             0, 1,   0,   0.4,
                             0, 0,   1,   0,
                             0, 0.4, 0,   1), ncol = 4, byrow = TRUE),
  sd.item        = c(.2, 1, .2, .5),
  cor2cov.item   = TRUE
)
```

### Inspecting implied distributions

Before committing to a design, it is useful to inspect the implied
response accuracy and response time distributions. This helps evaluate
whether the assumed data-generating process is plausible for the
intended application. I set N = 1e4 as an example. One could use the
estimated mimum sample size as well to inspect these distributions post
hoc.

``` r
do.call(plot_RA, c(list(level = "item", by.theta = TRUE, N = 1e4), sim.mod.args))
```

![](sspLNIRT_files/figure-html/unnamed-chunk-3-1.png)

``` r
do.call(plot_RT, c(list(level = "item", logRT = FALSE, N = 1e4), sim.mod.args))
```

![](sspLNIRT_files/figure-html/unnamed-chunk-4-1.png)

Additional views are available at the person level:

``` r
do.call(plot_RA, c(list(level = "person", by.theta = TRUE, N = 1e4), sim.mod.args))
```

![](sspLNIRT_files/figure-html/unnamed-chunk-5-1.png)

``` r
do.call(plot_RT, c(list(level = "person", logRT = TRUE, N = 1e4), sim.mod.args))
```

![](sspLNIRT_files/figure-html/unnamed-chunk-6-1.png)

## Step 2: Determine the minimum sample size

### Using `optim_sample()` for custom designs

To run the sample size optimization directly, call
[`optim_sample()`](https://sebastian-lortz.github.io/sspLNIRT/reference/optim_sample.md).
This uses a bisection algorithm that iteratively evaluates
[`comp_rmse()`](https://sebastian-lortz.github.io/sspLNIRT/reference/comp_rmse.md)
at candidate sample sizes until the RMSE of the target parameter crosses
the threshold. Each step runs a full Monte Carlo simulation study with
`iter` replications.

``` r
# Not run — computationally intensive (several hours)
library(future)
plan(multisession, workers = 4)

optim.args <- list(
  thresh       = 0.1,
  range        = c(50, 2000),
  out.par      = "alpha",
  iter         = 100,
  XG           = 6000,
  burnin       = 20,
  rhat         = 1.05,
  keep.err.dat = FALSE,
  seed         = 1234
)

res_alpha <- do.call(optim_sample, c(sim.mod.args, optim.args))
summary(res_alpha)
```

For a single evaluation at a fixed $N$ (without optimization), use
[`comp_rmse()`](https://sebastian-lortz.github.io/sspLNIRT/reference/comp_rmse.md):

``` r
# default optimizer settings
opt.args <- list(N = 200, iter = 100, XG = 6000, burnin = 20,
       rhat = 1.05, keep.err.dat = TRUE, seed = 1234)

# Not run — computationally intensive
rmse_result <- do.call(comp_rmse, c(opt.args,sim.mod.args))
summary(rmse_result)
```

### Using precomputed results

The package ships with precomputed minimum sample sizes across a
factorial grid of design conditions. If the assumed design is covered by
this grid, results can be retrieved instantly without running any
simulations. You can use
[`available_configs()`](https://sebastian-lortz.github.io/sspLNIRT/reference/available_configs.md)
to check if your specifications is in the available conditions:

``` r
configs <- available_configs()
head(configs, 10)
#>    thresh out.par  K mu.alpha meanlog.sigma2 rho
#> 1    0.20   alpha 10      0.6      -1.609438 0.2
#> 2    0.20    beta 10      0.6      -1.609438 0.2
#> 3    0.20     phi 10      0.6      -1.609438 0.2
#> 4    0.20  lambda 10      0.6      -1.609438 0.2
#> 5    0.10   alpha 10      0.6      -1.609438 0.2
#> 6    0.10    beta 10      0.6      -1.609438 0.2
#> 7    0.10     phi 10      0.6      -1.609438 0.2
#> 8    0.10  lambda 10      0.6      -1.609438 0.2
#> 9    0.05   alpha 10      0.6      -1.609438 0.2
#> 10   0.05    beta 10      0.6      -1.609438 0.2
```

The columns correspond to the design factors that were varied: `thresh`
(RMSE threshold), `out.par` (target item parameter), `K` (test length),
`mu.alpha` (mean item discrimination), `meanlog.sigma2` (log-scale mean
of $\sigma^{2}$), and `rho` (ability–speed correlation).

To retrieve a matching result, use
[`get_sspLNIRT()`](https://sebastian-lortz.github.io/sspLNIRT/reference/get_sspLNIRT.md):

``` r
res_alpha <- get_sspLNIRT(
  thresh         = 0.1,
  out.par        = "alpha",
  K              = 10,
  mu.alpha       = 1,
  meanlog.sigma2 = log(0.6),
  rho            = 0.4
)
summary(res_alpha$object)
#> ==================================================
#> 
#> Call: optim_sample()
#> 
#> Sample Size Optimization
#> --------------------------------------------------
#>   Minimum N:  639 
#>   Power Curve N:  
#>   MSE result:  0.09884139 
#>   Steps:  13 
#>   Time elapsed: 1.102363 hours 
#> 
#> Item Parameter MSEs:
#> --------------------------------------------------
#>          alpha     beta      phi   lambda   sigma2
#> RMSE  0.098841 0.092362 0.035406 0.033659 0.043612
#> MC SD 0.031066 0.027087 0.008124 0.008721 0.008937
#> 
#> Person Parameter MSEs:
#> --------------------------------------------------
#>          theta     zeta
#> RMSE  0.459026 0.412612
#> MC SD 0.024429 0.039770
#> 
#> Converged Iterations in final step: 92 
#> ---
```

Having established that the target precision for $\alpha$ is reached, I
can check the other precisions in the output. All other item parameters
are below my RMSE threshold, thus, I choose the minimum sample size from
the ouput as my planned sample size.

## Step 3: Inspect and visualize

### Power curve

The power curve shows how RMSE decreases with $N$, fitted as a log-log
regression through the optimization trace. It can be used to extrapolate
the required $N$ for the target threshold and thus, has less variane
than the N.min from the bisection optimization. It can be used to
further inspect the relation between sample size and precision.

``` r
plot_power_curve(res_alpha$object, thresh = 0.1)
```

![](sspLNIRT_files/figure-html/unnamed-chunk-11-1.png)

The left panel shows the log-log fit; the right panel the original
scale. The dashed line marks the threshold, the dotted line the
extrapolated $N$.

### Estimation precision

[`plot_precision()`](https://sebastian-lortz.github.io/sspLNIRT/reference/plot_precision.md)
shows how RMSE or bias varies across the range of true parameter values.
This is useful for identifying regions of the parameter space where
estimation is more or less precise:

``` r
plot_precision(res_alpha$object, pars = "item", y.val = "rmse")
#> `geom_smooth()` using formula = 'y ~ x'
```

![](sspLNIRT_files/figure-html/unnamed-chunk-12-1.png)

``` r
plot_precision(res_alpha$object, pars = "person", y.val = "rmse")
#> `geom_smooth()` using formula = 'y ~ x'
```

![](sspLNIRT_files/figure-html/unnamed-chunk-13-1.png)

``` r
plot_precision(res_alpha$object, pars = "item", y.val = "bias")
#> `geom_smooth()` using formula = 'y ~ x'
```

![](sspLNIRT_files/figure-html/unnamed-chunk-14-1.png)

### Design configuration

The full set of generating parameters can be inspected:

``` r
str(res_alpha$design)
#> List of 16
#>  $ thresh        : num 0.1
#>  $ range         : num [1:2] 50 2000
#>  $ out.par       : chr "alpha"
#>  $ iter          : num 100
#>  $ K             : num 10
#>  $ mu.person     : num [1:2] 0 0
#>  $ mu.item       : num [1:4] 1 0 0.5 1
#>  $ meanlog.sigma2: num -0.511
#>  $ cov.m.person  : num [1:2, 1:2] 1 0.4 0.4 1
#>  $ cov.m.item    : num [1:4, 1:4] 1 0 0 0 0 1 0 0.4 0 0 ...
#>  $ sd.item       : num [1:4] 0.2 1 0.2 0.5
#>  $ cor2cov.item  : logi TRUE
#>  $ sdlog.sigma2  : num 0
#>  $ XG            : num 6000
#>  $ rhat          : num 1.05
#>  $ seed          : num 311251
#>  - attr(*, "class")= chr "sspLNIRT.design"
```

## Interactive Shiny application

The package includes a Shiny app for interactive exploration of
precomputed results. Users specify design conditions via the interface,
and the app retrieves the corresponding minimum sample size with summary
statistics and visualizations. Alternatively, users can specify their
own design condition, inspect the distribution visualizations, and
download the function call for R.

``` r
run_app()
```

## References

Fox, J.-P., Klotzke, K., & Simsek, A. S. (2023). R-package LNIRT for
joint modeling of response accuracy and times. PeerJ Comput Sci, 9,
e1232. <https://doi.org/10.7717/peerj-cs.1232>

van der Linden, W. J. (2007). A hierarchical framework for modeling
speed and accuracy on test items. Psychometrika, 72 (3), 287–308.
<https://doi.org/10.1007/s11336-006-1478-z>
