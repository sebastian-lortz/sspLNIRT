
# install from GitHub
# devtools::install_github("sebastian-lortz/sspLNIRT")

library(sspLNIRT)
library(ggplot2)

# input paramters
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

## RA plot
set.seed(123)
RA.plot <- do.call(plot_RA, c(list(level = "item", by.theta = TRUE, N = 1e4), sim.mod.args))
ggsave(
  filename = "data-raw/plots/RA.design.plot.pdf",
  plot     = RA.plot,
  width    = 180,
  height   = 100,
  units    = "mm",
  bg       = "white",
  dpi = 300
)

## RT plot
set.seed(456)
RT.plot <- do.call(plot_RT, c(list(level = "item", logRT = FALSE, N = 1e4), sim.mod.args))
ggsave(
  filename = "data-raw/plots/RT.design.plot.pdf",
  plot     = RT.plot,
  width    = 180,
  height   = 100,
  units    = "mm",
  bg       = "white",
  dpi = 300
)


# SSP output
res_alpha <- get_sspLNIRT(
  thresh         = 0.1,
  out.par        = "alpha",
  K              = 10,
  mu.alpha       = 1,
  meanlog.sigma2 = log(0.6),
  rho            = 0.4
)
summary(res_alpha$object)

res_final <- res_alpha

## Power curve
power.plot <- plot_power_curve(res_final$object, thresh = 0.1)
ggsave(
  filename = "data-raw/plots/power.plot.pdf",
  plot     = RT.plot,
  width    = 180,
  height   = 100,
  units    = "mm",
  bg       = "white",
  dpi = 300
)

# estimation plots
est.item <- plot_estimation(res_final$object, pars = "item", y.val = "rmse")
ggsave(
  filename = "data-raw/plots/est.item.plot.pdf",
  plot     = est.item,
  width    = 180,
  height   = 100,
  units    = "mm",
  bg       = "white",
  dpi = 300
)

est.person <- plot_estimation(res_final$object, pars = "person", y.val = "rmse")
ggsave(
  filename = "data-raw/plots/est.person.plot.pdf",
  plot     = est.person,
  width    = 180,
  height   = 100,
  units    = "mm",
  bg       = "white",
  dpi = 300
)
