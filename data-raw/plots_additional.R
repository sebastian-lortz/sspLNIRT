# additional plots for the paper

  # set root path
  root.dir <- "/Users/lortz/Desktop/PhD/Research/Chapter 1/sspLNIRT/"



# required functions
fct.names <- list(
  "R/fct_comp_rmse.R",
  "R/fct_geweke_LNIRT.R",
  "R/fct_item_par.R",
  "R/fct_optim_sample.R",
  "R/fct_person_par.R",
  "R/fct_rhat_LNIRT.R",
  "R/fct_scale_M.R",
  "R/fct_sim_jhm_data.R",
  "R/fct_plot_RA.R",
  "R/fct_plot_RT.R",
  "R/utils_helpers.R"
)

# load to environment
invisible (
  lapply(fct.names, FUN = function(x) {
    source(paste0(root.dir, x))
  }))


# plot illustrations
set.seed(123)
RA.plot <- plot_RA(level = "item",
        by.theta = TRUE,
        mu.item = c(1,0,.5,1),
        sd.item = c(.2, 1, .2, .5),
        meanlog.sigma2 = log(.2),
        K = 12)

set.seed(456)
RT.plot <- plot_RT(level = "item",
        mu.item = c(1,0,.5,1),
        sd.item = c(.2, 1, .2, .5),
        meanlog.sigma2 = log(.2),
        K = 12,
        logRT = FALSE)



ggsave(
  filename = "/Users/lortz/Desktop/PhD/Research/Chapter 1/sspLNIRT/data-raw/plots/RA.design.plot.pdf",
  plot     = RA.plot,
  width    = 180,
  height   = 100,
  units    = "mm",
  bg       = "white"
)

ggsave(
  filename = "/Users/lortz/Desktop/PhD/Research/Chapter 1/sspLNIRT/data-raw/plots/RT.design.plot.pdf",
  plot     = RT.plot,
  width    = 180,
  height   = 100,
  units    = "mm",
  bg       = "white"
)
