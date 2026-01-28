# script to determine the lower and upper bound of N for the sspLNIRT.data script

# Setup Config ------------------------------------------------------------


# sys settings
Sys.setenv(
  OMP_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  BLAS_NUM_THREADS = "1"
)

# cran repo
options(repos=c(CRAN="https://ftp.belnet.be/mirror/CRAN/"))

# setup for HPC or local
HPC = FALSE

if (HPC) {
  # set root path
  root.dir <- "/home4/p310779/sspLNIRT/"

  # set save path
  save.dir <- "/home4/p310779/sspLNIRT/data-raw/results/"
  dir.create(save.dir, recursive = TRUE, showWarnings = FALSE)

} else {
  # set root path
  root.dir <- "/Users/lortz/Desktop/PhD/Research/Chapter 1/sspLNIRT/"

  # set save path
  save.dir <- "/Users/lortz/Desktop/PhD/Research/Chapter 1/sspLNIRT/data-raw/results/"
  #dir.create(save.dir, recursive = TRUE, showWarnings = FALSE)


}

# required functions
fct.names <- list(
  "R/fct_comp_mse.R",
  "R/fct_geweke_LNIRT.R",
  "R/fct_item_par.R",
  "R/fct_optim_sample.R",
  "R/fct_person_par.R",
  "R/fct_rhat_LNIRT.R",
  "R/fct_scale_M.R",
  "R/fct_sim_jhm_data.R",
  "R/utils_helpers.R"
)

# load to environment
invisible (
  lapply(fct.names, FUN = function(x) {
    source(paste0(root.dir, x))
  }))

# cores
if (HPC ) {
  # set cores
  n.cores <- future::availableCores() - 5
  cat("running with ", n.cores, "cores! \n\n")
  future::plan(future::multisession, workers = n.cores)
} else {
  n.cores <- 6
  future::plan(future::multisession, workers = n.cores)
}




# Run the Job -------------------------------------------------------------


# generate the design conditions for low and high N
design <- expand.grid(
  I = c(30,50),
  N = c(100, 2000),
  mu.alpha = c(.6, 1.4),
  meanlog.sigma2 = c(log(.2), log(1)),
  rho = c(0, .6)
  )

# storage
bounds.res <- list()

# compute MSE
for (i in seq_len(nrow(design))) {

# get pars
N = design$N[i]
I = design$I[i]
meanlog.sigma2 = design$meanlog.sigma2[i]
rho = design$rho[i]
cor.item = design$cor.item[i]
mu.alpha = design$mu.alpha[i]
# comp MSE
res <- comp_mse(
          iter = 1000,
          N = N,
          I = I,
          mu.person = c(0,0),
          mu.item = c(mu.alpha,0,1,1),
          meanlog.sigma2 = meanlog.sigma2,
          cov.m.person = matrix(c(1,rho,
                                  rho,1), ncol = 2, byrow = TRUE),
          cov.m.item = matrix(c(1, 0, 0, 0,
                                 0, 1, 0, .4,
                                 0, 0, 1, 0,
                                 0, .4, 0, 1), ncol =  4, byrow = TRUE),
          sd.item         = c(.2, 1, .2, .5),
          cor2cov.item    = TRUE,
          sdlog.sigma2 = 0.2,
          XG = 6000,
          mse.seed = NULL)
bounds.res[[i]] <- res
cat("design row", i, "of", nrow(design), "done!! \n\n")
rm(res)
gc()
}

saveRDS(bounds.res, paste0(save.dir, "design.bounds"))





# Results -----------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)


L = nrow(design)

# load results
res.names <- readRDS(paste0(save.dir, "design.bounds"))

# check convergence
conv.data =
  sapply(res.names, FUN = function(y) {
    nrow(y$conv.rate)
  })
sum_conv <- data.frame(design, prop = conv.data/1000)


# get mse data
mse.list <- round(as.data.frame(t(sapply(res.names, FUN = function(y) {
    cbind(y$mse.alpha,
          y$mse.beta,
          y$mse.phi,
          y$mse.lambda,
          y$mse.sigma2,
          y$mse.theta,
          y$mse.zeta)
  }))),7)
colnames(mse.list) <- c("alpha", "beta", "phi", "lambda", "sigma2", "theta", "zeta")
mse.data <- data.frame(condition = rep(1:64, each = 7),
                  pivot_longer(mse.list, cols = c(1:7), values_to = "mse", names_to = "parameter"))


out.data <- cbind(sum_conv, mse.list)

# design parameters
x_vars <- names(out.data)[1:5]

make_plot <- function(df, x, param, predictors = x_vars, show_legend = FALSE) {
  others <- setdiff(predictors, x)
  d <- df

  # discrete x-axis (ordered if numeric)
  if (is.numeric(d[[x]])) d[[x]] <- factor(d[[x]], levels = sort(unique(d[[x]])))
  else d[[x]] <- factor(d[[x]])

  # one line per combination of other predictors
  d$cond <- interaction(d[others], drop = TRUE, sep = " | ")

  # line uses mean in case there are repeated rows per (cond, x)
  m <- aggregate(d[[param]], list(cond = d$cond, x = d[[x]]), mean, na.rm = TRUE)
  names(m)[3] <- "y"

  p <- ggplot(d, aes(x = .data[[x]], y = .data[[param]], group = cond, colour = cond)) +
    geom_line(data = m, aes(x = x, y = y), linewidth = 0.7) +
    geom_point(size = 2) +
    labs(x = x, y = "MSE") +
    theme_minimal(base_size = 12)

  if (!show_legend) p <- p + guides(colour = "none")
  p
}

cond_grid <- function(param, df = out.data,
                      predictors = x_vars, ncol = 3, show_legend = FALSE) {
  plots <- lapply(predictors, \(x) make_plot(df, x, param, predictors, show_legend))
  wrap_plots(plots, ncol = ncol) +
    plot_annotation(title = paste0("Conditioned effects on ", param))
}

# plots
cond_grid("prop")
cond_grid("alpha")
cond_grid("beta")
cond_grid("phi")
cond_grid("lambda")
cond_grid("theta")
cond_grid("zeta")


