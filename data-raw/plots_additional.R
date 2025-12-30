# additional plots for the paper



RA.plot <- plot_RA(level = "item",
        by.theta = TRUE,
        mu.item = c(1,0,1,1),
        sd.item = c(.2, .5, .2, .5),
        meanlog.sigma2 = log(.2),
        I = 12)


RT.plot <- plot_RT(level = "item",
        mu.item = c(1,0,1,1),
        sd.item = c(.2, .5, .2, .5),
        meanlog.sigma2 = log(.2),
        I = 12,
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
